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


h.test$Churn <- ifelse(h.test$Churn == "Yes", 1, 0)
h.train$Churn <- ifelse(h.train$Churn == "Yes", 1, 0)

h.test1 <- h.test[,1:10]
h.train1 <- h.train[,1:10]

h.k <- 10
h.A <- function(k, train, test) {knn(train, test, cl = train$Churn, k)}
h.t_predTest1 <- list()
for (k in seq(h.k)) {
  h.t_predTest1[[k]] <- h.A(k, h.train1, h.test1)
  }

print(h.t_predTest1)


# error de knn

h.error_test <- list()
for (k in seq(h.k)) {
  errorsTest <- fn_err_cla(h.t_predTest1[[k]],h.test1$Churn)
  h.error_test[[k]] <- mean(as.vector(unlist(errorsTest)))
}

print(h.error_test)

