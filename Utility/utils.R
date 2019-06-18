
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

## Carga de los datos principales con las ETL generales para todos los algoritmos
loadData <- function()
{
  T0 <- read.csv(file = "Dataset/dataset.csv")
  T0$CustomerID <- NULL
  T0$ServiceArea <- NULL
  T0$Churn <- ifelse(T0$Churn == 'Yes',1,0)
  df <- na.omit(T0)
}

#Error de clasificacion
fn_err_cla <- function(yhat, y) { mean(yhat != y) }

# costo
fn_err_cost <- function(yhat, y) { 
  fp_cost <- 20 * sum( yhat == 1 & y == 0 )
  tp_cost <- -35 * sum( yhat == 1 & y == 1 )
  return((fp_cost + tp_cost) / length(y)) 
}

# particion train-test
partition_train_test <- function(df, ntrain = 100) {
  train_idx <- sample.int(nrow(df), size = ntrain)
  list(train = df[train_idx,], test = df[-train_idx,])
}

# Particion en 5 folds
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

# Plotear error para una lista de vectores de error
plot_umbral_err <- function(list_err, umbral, main = '') {
  # list_err: lista de vectores de la misma longitud
  col_vec <- rainbow(length(list_err))
  plot(list_err[[1]], 
       type = 'l', 
       col = col_vec[1], 
       main = main, 
       ylab = 'Error', 
       xlab = 'Umbral', 
       xaxt = 'n')
  axis(1, 
       at = seq(1, length(umbral)), 
       labels = umbral)
  for (i in seq(2, length(list_err))) {
    lines(list_err[[i]], col = col_vec[i])
  }
  legend("bottomleft", 
         cex = 0.5,
         legend = seq(1, length(list_err)), 
         col = col_vec,
         lty = rep(1, length(list_err)))
}

#******************** RandomForest ***************************

# Train random forest
rf_fit_ctrl <- function(train, formula, ctrl) {
  # train: datos de entrenamiento
  # formula: una formula (para clasificacion usar as.factor en la variable a predecir)
  # ctrl: es una lista de listas cada lista en ctrl tiene dos atributos: ntree y mtry
  list_fit <- list()
  for (i in seq(1, length(ctrl))) {
    list_fit[[i]] <- randomForest(as.formula(formula), 
                                  data = train, 
                                  ntree = ctrl[[i]]$ntree,
                                  mtry = ctrl[[i]]$mtry)
  }
  return(list_fit)
}

# Prediccion
rf_prob <- function(list_fit, newdata) {
  # list_fit: lista retornada por rf_fit_ctrl
  # newdata: datos de test
  test_prob <- list()
  for (i in seq(1, length(list_fit))) {
    # prediccion
    test_prob[[i]] <- predict(list_fit[[i]], 
                              newdata = newdata, 
                              type = 'prob')[,2]
  }
  return(test_prob)
}

# Prediccion para un vector de umbrales
rf_pred <- function(test_prob, umbral) {
  # test_prob: lista retornada por rf_prob
  # umbral: vector de umbrales entre 0 y 1
  test_pred <- list()
  for (i in seq(1, length(test_prob))) {
    test_pred[[i]] <- list()
    for (j in seq(1, length(umbral))) {
      # prediccion
      test_pred[[i]][[j]] <- ifelse(test_prob[[i]] > umbral[j], 1, 0)
    }
  }
  return(test_pred)
}

# Error de prediccion para una lista de predicciones
rf_pred_err <- function(test_pred, y) {
  # test_pred: lista retornada por rf_pred
  test_pred_err <- list()
  for (i in seq(1, length(test_pred))) {
    test_pred_err[[i]] <- rep(0, length(test_pred[[i]]))
    for (j in seq(1, length(test_pred[[i]]))) {
      # error
      test_pred_err[[i]][j] <- fn_err(test_pred[[i]][[j]], y)
    }
  }
  return(test_pred_err)
}

# Error de CV
rf_cv_err <- function(cv_part, formula, ctrl, umbral = 0.5, var_y) {
  # cv_part: particion generada por partition_cv
  # formula: formula
  # ctrl: lista de controles de rf
  # umbral: umbral para calcular la prediccion
  # y: variable a predecir
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_err <- list()
  for (k in seq(1, cv_part$k_folds)) {
    # fit
    list_fit <- rf_fit_ctrl(cv_train[[k]], 
                            formula = formula, 
                            ctrl = ctrl)
    # prob
    list_prob <- rf_prob(list_fit, 
                         newdata = cv_test[[k]])
    # pred
    list_pred <- rf_pred(list_prob, 
                         umbral = umbral) 
    # error
    cv_err[[k]] <- rf_pred_err(list_pred, 
                               cv_test[[k]][[var_y]])
  }
  return(mean(unlist(cv_err)))
}

