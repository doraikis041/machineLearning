# funciones auxiliares

# error de clasificacion

fn_err_cla <- function(yhat, y) { mean(yhat != y) }

# errir de regresion MSE

fn_err_mse <- function(yhat, y) { mean((yhat - y)^2) }

# particion train-test

partition_train_test <- function(df, ntrain = 10) {
  train_idx <- sample.int(nrow(df), size = ntrain)
  list(train = df[train_idx,], test = df[-train_idx,])
}

# Particion en 5 folds

partition_cv <- function(df, k_folds = 5) {
  cv_test <- split(df, seq(1, h.k_folds))
  cv_train <- list()
  for (k in seq(1, k_folds)) {
    cv_train[[k]] <- data.frame()
    for (i in seq(1, k_folds)) {
      if (i != k) cv_train[[k]] <- rbind(cv_train[[k]], cv_test[[i]])
    }
  }
  list(train = cv_train, test = cv_test, k_folds = k_folds)
}

# Regresion con glm para una lista de formulas

glm_fit_formulas <- function(train, formulas) {
  list_fit <- list()
  for (i in seq(1, length(formulas))) {
    list_fit[[i]] <- glm(as.formula(formulas[i]), data = train)
  }
  return(list_fit)
}

# Prediccion y error MSE

glm_pred_err <- function(list_fit, newdata, y) {
  test_pred <- list()
  test_err <- rep(0, length(list_fit))
  for (i in seq(1, length(list_fit))) {
    # prediccion
    test_pred[[i]] <- predict(list_fit[[i]], newdata = newdata)
    # MSE
    test_err[i] <- fn_err_mse(test_pred[[i]], newdata[[y]])
  }
  list(pred = test_pred, err = test_err)
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


