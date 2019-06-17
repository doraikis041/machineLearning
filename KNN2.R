

source("Utility/functions.R")


# Cargar datos

raw_data <- loadData()

h.data <- partition_train_test_numeric (raw_data, nTrain)
h.train <- h.data$train
h.test <- h.data$test

h.train1 <- h.train[,-h.train$Churn]
h.test1 <- h.test[,-h.test$Churn]

h.k <- 2
h.A <- function(k, train, test, y) {knn(train, test, cl = as.factor(y), k)}
h.t_predTest <- list()
for (k in seq(h.k)) {
  h.t_predTest[[k]] <- h.A(k, h.train1, h.test1, y = h.train$Churn)
  }

print(h.t_predTest)


# error de knn

knn_pred_err_cla <- function (list_pred, newdata, y, k) {
test_err <- rep (0, length(k))
for (i in seq(k)) {
  test_err[i] <- fn_err_cla(list_pred[[i]], newdata[[y]])
}
list(err = test_err, k = k)
}

h.error_knn <- knn_pred_err_cla(list_pred = h.t_predTest, newdata = h.test, y = 'Churn', k= h.k)
print(h.error_knn)


# error CV 
h.cv <- partition_cv(df = h.train1) 
h.cv_con_Churn <- partition_cv(df = h.train$Churn)

cv_err_knn <- function(cv_part, y, k) {
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_matrix_err <- matrix(0, nrow = cv_part$k_folds)
  for (i in seq(1, cv_part$k_folds)) {
    list_pred_err <- h.A(k = k, train = cv_train[[i]], test = cv_test[[i]], y = y[i])
    cv_matrix_err[k, ] <- knn_pred_err_cla(list_pred = list_pred_err[[i]], newdata = cv_test[[i]], y = y, k= k)
  
  }
  apply(cv_matrix_err, 2, mean)
}

h.CV_err <- cv_err_knn(cv_part = h.cv, y = h.cv_con_Churn, k= h.k) 


# list_pred_err <- h.A(k = h.k, train = h.cv$train[[1]], test = h.cv$test[[1]], y= h.cv_con_Churn$train[[1]]$Churn)

# for (s in seq(1,h.kfolds)  { 
#CV_pred[[s]] <- knn(CV_Train[[s]], CV_test[[s]]....)
#CV_err[[s]] <- fn_err(CV_pred[[s]], CV_test[[s]])
#}

# for (i in seq(1,h.cv$k_folds)) {
#   cv_pred <- list()
#   for (j in h.k) {
#     cv_pred[[i]] <- h.A(j, h.cv$train[[i]], h.cv$test[[i]], h.cv$train[[i])
#   }
# }
# 



