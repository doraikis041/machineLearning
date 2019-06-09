# funciones auxiliares

# Clasificacion con rpart para una lista de formulas

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

rpart_fit_ctrl <- function(train, formula, ctrl, method = 'class') {
  list_fit <- list()
  for (i in seq(1, length(ctrl))) {
    list_fit[[i]] <- rpart(as.formula(formula), 
                           data = train, 
                           control = ctrl[[i]],
                           method = method)
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
