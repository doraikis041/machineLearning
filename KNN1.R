source("Utility/head.R")
source("Utility/funciones.R")

#Genero la grilla de visualización
h.gridDF <- dfGridMetada()

#Se carga los datos con las ETL generales
h.data <- loadData()

#Generar la particion de test & train
h.part <- partition_train_test(h.data, ntrain = nTrain)
h.train <- h.part$train
h.test <- h.part$test

#Entrenamiento del algoritmo

h.numeric_vars <- which(sapply(h.data, is.numeric))
h.train <- h.train[,h.numeric_vars]
h.test <- h.test[,h.numeric_vars]

h.train1 <- h.train[,-h.train$Churn]
h.test1 <- h.test[,-h.test$Churn]

h.k <- 2
h.A <- function(k, train, test, y) {knn(train, test, cl = as.factor(y), k)}
h.t_predTest1 <- list()
for (k in seq(h.k)) {
  h.t_predTest1[[k]] <- h.A(k, h.train1, h.test1, y = h.train$Churn)
  }

print(h.t_predTest1)






# h.t_predTrain <- list()
# for (k in seq(h.k)) {
#   h.t_predTrain[[k]] <- list()
#   for (s in seq(h.ds_t)) {
#     h.t_predTest[[k]][[s]] <- h.A(k,h.T[[s]], T0)
#     h.t_predTrain[[k]][[s]] <- h.A(k,h.T[[s]], h.T[[s]])
#   }
# }



# error de knn

h.error_test <- list()
for (k in seq(h.k)) {
  errorsTest <- fn_err_cla(h.t_predTest1[[k]],h.test$Churn)
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



