
#Librerias Principales
library(class)
library(e1071)
#Tree
library(rpart)
library(rpart.plot)
library(dplyr)
#RandomForest
library(randomForest)
#Boosting
library(gbm)

#Definiendo el valor de see para los datos aleatorios generados
set.seed(117)

#Variables Generales
nTrain <- 37445

# Grilla para visualizacion
dfGridMetada <- function(min = 0, max = 5, by = 0.1){grid_df <- data.frame(x = seq(min, max, by))}

## Carga de los datos principales con las ETL generales para todos los algoritmos
loadData <- function()
{
  T0 <- read.csv(file = "Dataset/dataset.csv")
  T0$CustomerID <- NULL
  T0$ServiceArea <- NULL
  T0$Churn <- ifelse(T0$Churn == 'Yes',1,0)
  df <- na.omit(T0)
}

## Calculo del Error de clasificación
fn_err_cla <- function(yhat, y) { mean(yhat != y) }


## Creación de la particion train-test general
partition_train_test <- function(df, ntrain = 10) {
  train_idx <- sample.int(nrow(df), size = ntrain)
  list(train = df[train_idx,], test = df[-train_idx,])
}

## Creación de la particion train-test solo numeric
partition_train_test_numeric <- function(df, ntrain = 10) {
  df_numeric <- which(sapply(df,is.numeric))
  df <- df[,df_numeric]
  train_idx <- sample.int(nrow(df), size = ntrain)
  list(train = df[train_idx,], test = df[-train_idx,])
}


## Particion en K folds, por defecto k_folds = 5
partition_cv <- function(df, k_folds = 5) {
  cv_test <- split(df, seq(1, k_folds))
  cv_train <- list()
  for (k in seq(1, k_folds)) {
    cv_train[[k]] <- data.frame()
    for (i in seq(1, k_folds)) {
      if (i != k) cv_train[[k]] <- rbind(cv_train[[k]], cv_test[[i]])
    }
  }
  list(train = cv_train, test = cv_test, k_folds = k_folds)
}


###########################################################
###########        Regresion con glm             ##########
###########################################################
glm_fit_formulas_cla <- function(train, formulas) {
  list_fit <- list()
  for (i in seq(1, length(formulas))) {
    list_fit[[i]] <- glm(as.formula(formulas[i]), data = train, family = 'binomial')
  }
  return(list_fit)
}


# Prediccion y error 

glm_pred_err_cla <- function(list_fit, newdata, y, umbral =0.5) {
  test_prob <- list()
  test_pred <- list()
  test_err <- rep(0, length(list_fit))
  for (i in seq(1, length(list_fit))) {
    # probabilidad
    test_prob[[i]] <- predict(list_fit[[i]], newdata = newdata, type = 'response') #para que de la probabilidad
    # prediccion
    test_pred[[i]] <- ifelse(test_prob[[i]] > umbral,1,0)
    # error de clasificacion
    test_err[i] <- fn_err_cla(test_pred[[i]], newdata[[y]])
  }
  list(prob = test_prob, pred = test_pred, err = test_err)
}


# Prediccion y matriz de confusión  
glm_pred_err_mc <- function(list_fit, newdata, y) {
  test_prob <- list()
  test_pred <- list()
  test_err  <- list ()
  test_mc <- list()
  for (i in seq(1, length(list_fit))) {
    # probabilidad
    test_prob[[i]] <- predict(list_fit[[i]], newdata = newdata, type = 'response') #para que de la probabilidad
    # prediccion 
    umbral <- seq(0.3, to = 0.7, by = 0.1)
    test_err[[i]] <- rep(0, length(umbral))
    test_pred[[i]] <- list ()
    test_mc[[i]] <- list ()
    for (j in seq(1, length(umbral))) {
    # prediccion 
    test_pred[[i]][[j]] <- ifelse(test_prob[[i]] > umbral[j],1,0)
    # error 
    test_err[[i]][j] <- fn_err_cla(test_pred[[i]][[j]], newdata[[y]])
    # mc 
    test_mc[[i]][[j]] <- table(test_pred[[i]][[j]],newdata[[y]])
          }
  }
  list(prob = test_prob, pred = test_pred, err = test_err, mc = test_mc)
  }

# Prediccion y error MSE
cv_err <- function(cv_part, formulas, y) {
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_matrix_err <- matrix(0, nrow = cv_part$k_folds, ncol = length(formulas))
  for (k in seq(1, cv_part$k_folds)) {
    list_fit <- glm_fit_formulas(cv_train[[k]], formulas = formulas)
    list_pred_err <- glm_pred_err(list_fit, newdata = cv_test[[k]], y = y)
    cv_matrix_err[k, ] <- list_pred_err$err  
  }
  apply(cv_matrix_err, 2, mean)
}





###########################################################
######## Naive Bayes para una lista de formulas  ##########
###########################################################
naiveBayes_fit_formulas <- function(train, formulas) {
  list_fit <- list()
  for (i in seq(1, length(formulas))) {
    list_fit[[i]] <- naiveBayes(as.formula(formulas[i]), data = train)
  }
  return(list_fit)
}

# Prediccion & error Naive Bayes
nbayes_pred_err <- function(list_fit, newdata, y) {
  test_pred <- list()
  test_err <- rep(0, length(list_fit))
  for (i in seq(1, length(list_fit))) {
    # prediccion
    test_pred[[i]] <- predict(list_fit[[i]], newdata = newdata)
    # Error clasificacion
    test_err[i] <- fn_err_cla(test_pred[[i]], newdata[[y]])
  }
  list(pred = test_pred, err = test_err)
  
}


# Prediccion y error Naive Bayes CV
cv_err_nBayes <- function(cv_part, formulas, y) {
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_matrix_err <- matrix(0, nrow = cv_part$k_folds, ncol = length(formulas))
  for (k in seq(1, cv_part$k_folds)) {
    list_fit <- naiveBayes_fit_formulas(cv_train[[k]], formulas = formulas)
    list_pred_err <- nbayes_pred_err(list_fit, newdata = cv_test[[k]], y = y)
    cv_matrix_err[k, ] <- list_pred_err$err  
  }
  apply(cv_matrix_err, 2, mean)
}



#######################################################################
#################### Arboles con rpart ################################
#######################################################################
### FIT con rpart para una lista de CP
rpart_fit_ctrl <- function(train, formula, ctrl, method = 'class', parametros) {
  list_fit <- list()
  for (i in seq(1, length(ctrl))) {
    list_fit[[i]] <- rpart(as.formula(formula),data = train,control = ctrl[[i]],method = method, parms = parametros ) 
  }
  return(list_fit)
}

## Plot de lista de arboles
rpart_plot_fit <- function(list_tree) {
  for (i in seq(1, length(list_tree))){
      rpart.plot(list_tree[[i]], roundint = FALSE)
    }
}

## Prediccion y error de clasificacion con rpart
rpart_pred_err <- function(list_fit, newdata, y) {
  test_pred <- list()
  test_err <- rep(0, length(list_fit))
  for (i in seq(1, length(list_fit))) {
    # prediccion
    test_pred[[i]] <- predict(list_fit[[i]], newdata = newdata, type = 'class')
    # error de clasificacion
    test_err[i] <- fn_err_cla(test_pred[[i]], newdata[[y]])
  }
  list(pred = test_pred, err = test_err)
}


# Prediccion y error de cv con rpart
rpart_cv_err <- function(cv_part, formula, ctrl, y) {
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_matrix_err <- matrix(0, nrow = cv_part$k_folds, ncol = length(ctrl))
  for (k in seq(1, cv_part$k_folds)) {
    list_fit <- rpart_fit_ctrl(cv_train[[k]], 
                               formula = formula, ctrl = ctrl)
    list_pred_err <- rpart_pred_err(list_fit, newdata = cv_test[[k]], y = y)
    cv_matrix_err[k, ] <- list_pred_err$err  
  }
  apply(cv_matrix_err, 2, mean)
}


## Matrix de confusión con lista de umbrales (Reutilizando glm_pred_err_mc)
rpart_pred_err_mc <- function(list_fit, newdata, y,type = 'class',umbral) {
  test_prob <- list()
  test_pred <- list()
  test_err  <- list ()
  test_mc <- list()
  for (i in seq(1, length(list_fit))) {
    # probabilidad
    test_prob[[i]] <- predict(list_fit[[i]], newdata = newdata, type = 'class') #para que de la probabilidad
    # prediccion 
    test_err[[i]] <- rep(0, length(umbral))
    test_pred[[i]] <- list ()
    test_mc[[i]] <- list ()
    for (j in seq(1, length(umbral))) {
      # prediccion 
      test_pred[[i]][[j]] <- ifelse(as.numeric(test_prob[[i]]) > umbral[j],1,0)
      # error 
      test_err[[i]][j] <- fn_err_cla(test_pred[[i]][[j]], newdata[[y]])
      # mc 
      test_mc[[i]][[j]] <- table(test_pred[[i]][[j]],newdata[[y]])
    }
  }
  list(prob = test_prob, pred = test_pred, err = test_err, mc = test_mc)
}


#######################################################################
####################   RandomForest    ################################
#######################################################################

#Fit randomFprest (ensemble)
randomForest_fit <- function(train, formula, pNtree, pMtry = sqrt(length(train)) ) {
  list_fit <- list()
  #h.mtry <- sqrt(length(train)) # Defino el valor como la raiz cuadrada de la cantidad de variables
  for (i in seq(1, length(pNtree))) {
    list_fit[[i]] <- randomForest(as.formula(formula),data = train, ntree = pNtree[i], mtry = h.mtry)
  }
  return(list_fit)
}


### Prediccion y error de clasificacion con randomforest
randomForest_pred_err <- function(list_fit, newdata, y) {
  test_pred <- list()
  test_err <- rep(0, length(list_fit))
  for (i in seq(1, length(list_fit))) {
    #Prediction
    test_pred[[i]] <- predict(list_fit[[i]],
                              newdata = newdata,
                              type = 'class') #class automatically converted to "response", for backward compatibility
    #Clasification error
    test_err[i] <- fn_err_cla(test_pred[[i]], newdata[[y]])
  }
  list(pred = test_pred, err = test_err)
}

randomForest_cv_err <- function(cv_part, formula, y, pNtree, pMtry) {
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_matrix_err <- matrix(0, nrow = cv_part$k_folds, ncol = length(ctrl))
  for (k in seq(1, cv_part$k_folds)) {
    #randomForest
    list_fit <- randomForest_fit(formula = as.formula(formula),
                             train = cv_train[[k]],
                             pNtree = pNtree,
                             pMtry = pMtry)
    #randomForest_pred_err
    list_pred_err <- randomForest_pred_err(list_fit, newdata = cv_test[[k]], y = y)
    cv_matrix_err[k, ] <- list_pred_err$err  
  }
  apply(cv_matrix_err, 2, mean)

  #######################################################################
  ####################   Boosting        ################################
  #######################################################################
  
  # Fit using Boosting (gbm)
  boosting_fit <- function(pTrain, pFormula, dDistribution = "gaussian", pControl) {
    list_fit <- list()
    for (i in seq(1, length(pControl))) {
      list_fit[[i]] <- gbm(pFormula,
                           data = pTrain,
                           distribution = dDistribution,
                           interaction.depth = pControl[[i]]$interaction.depth,
                           shrinkage = pControl[[i]]$shrinkage,
                           n.trees = pControl[[i]]$n.trees)
      }
      return(list_fit)
    }
  }


