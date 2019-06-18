# error de clasificacion

fn_err_cla <- function(yhat, y) { mean(yhat != y) }

# error de regresion MSE

fn_err_mse <- function(yhat, y) { mean((yhat - y)^2) }

# costo

fn_err_cost <- function(yhat, y) { 
  fp_cost <- 20 * sum( yhat == 1 & y == 0 )
  tp_cost <- -35 * sum( yhat == 1 & y == 1 )
  return((fp_cost + tp_cost) / length(y)) 
}

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
