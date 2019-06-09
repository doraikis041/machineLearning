#Defincion de funciones utilizadas en los direfentes algoritmos

###########################################################
########### Error de clasificacion  #######################
###########################################################
fn_err_cla <- function(yhat, y) { mean(yhat != y) }


###########################################################
###########  Particion train-test   #######################
###########################################################
partition_train_test <- function(df, ntrain = 10) {
  train_idx <- sample.int(nrow(df), size = ntrain)
  list(train = df[train_idx,], test = df[-train_idx,])
}


###########################################################
###########   Particion en 5 folds  #######################
###########################################################
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
## Regresion con glm para una lista de formulas  ##########
###########################################################
glm_fit_formulas <- function(train, formulas) {
  list_fit <- list()
  for (i in seq(1, length(formulas))) {
    list_fit[[i]] <- glm(as.formula(formulas[i]), data = train)
  }
  return(list_fit)
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


###########################################################
########       Knn       #######################3##########
###########################################################
# knn_fit_formulas <- function(train,test, formulas) {
#   list_fit <- list()
#   for (i in seq(1, length(formulas))) {
#     list_fit[[i]] <- knn(data = train,test = test, cl = train$y, as.formula(formulas[i]), )
#   }
#   return(list_fit)
# }
# #h.A <- function(k, train, test) {knn(train, test, cl = train$y, k)}


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


## Se agregar todo lo de arbol
#######################################################################
rpart_fit_formulas <- function(train, formulas, method = 'class') {
  list_fit <- list()
  for (i in seq(1, length(formulas))) {
    list_fit[[i]] <- rpart(as.formula(formulas[i]), 
                           data = train, 
                           method = method)
  }
  return(list_fit)
}

# Clasificacion con rpart para una lista de cp

rpart_fit_ctrl <- function(train, formula, ctrl, method = 'class', hparms) {
  list_fit <- list()
  for (i in seq(1, length(ctrl))) {
    list_fit[[i]] <- rpart(as.formula(formula), 
                           data = train, 
                           control = ctrl[[i]],
                           method = method,
                           parms = hparms)
  }
  return(list_fit)
}

# Plot de lista de arboles

rpart_plot_fit <- function(list_tree) {
  for (i in seq(1, length(list_tree))) {
    rpart.plot(list_tree[[i]], roundint = FALSE)
  }
}


# Prediccion y error de clasificacion con rpart

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


