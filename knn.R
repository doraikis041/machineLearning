source("Utility/functions.R")


# Cargar datos
raw_data <- loadData()

h.data <- partition_train_test_numeric (raw_data, nTrain)
h.train <- h.data$train
h.test <- h.data$test
vars <- names(h.train) 

# h.train1 <- h.train[,-h.train$Churn]
# h.test1 <- h.test[,-h.test$Churn]

h.k <- 2
h.A <- function(k, train, test, y) {knn(train, test, cl = as.factor(y), k)}
h.t_predTest <- list()
for (k in seq(h.k)) {
  h.t_predTest[[k]] <- h.A(k, h.train, h.test, y = h.train$Churn)
  }

print(h.t_predTest)


# error de knn
knn_pred_err_cla <- function (list_pred, newdata, y, k) {
test_err <- rep (0, length(k))
for (i in seq(k)) {
  test_err[i] <- fn_err(list_pred[[i]], newdata[[y]])
}
list(err = test_err, k = k)
}

h.error_knn <- knn_pred_err_cla(list_pred = h.t_predTest, newdata = h.test, y = 'Churn', k= h.k)
print(h.error_knn)


# error CV 
h.cv <- partition_cv(df = h.train) 


knn_cv_err <- function(cv_part, cl, vars, k) {
  cv_err <- rep(0, cv_part$k_folds)
  cv_list_erro <- list()
  for (i in seq(k))
    {
    for (s in seq(1, cv_part$k_folds)) {
      cv_train <- cv_part$train[[s]][, vars]
      cv_test <- cv_part$test[[s]][, vars]
      cv_knn_class <- cv_part$train[[s]][[cl]]
      cv_pred <- knn(cv_train , cv_test , cl = cv_knn_class , k = i)
      cv_err[s] <- fn_err(cv_pred, cv_part$test[[s]][[cl]])
    }
    cv_list_erro[[i]] <- mean(cv_err)
  }
  return(cv_list_erro)
}

h.CV_err <- knn_cv_err(cv_part = h.cv, cl = "Churn", vars = vars, k = h.k)

#h.knn_cv

#h.CV_err <- cv_err_knn(cv_part = h.cv, y = h.cv_con_Churn, k= h.k) 





