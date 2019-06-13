

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

h.error_test <- list()
for (k in seq(h.k)) {
  errorsTest <- fn_err_cla(h.t_predTest[[k]],h.test$Churn)
  h.error_test[[k]] <- mean(as.vector(unlist(errorsTest)))
}

print(h.error_test)




# error CV 

h.cv <- partition_cv(df = h.train1) 


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



