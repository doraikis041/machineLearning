  source("Utility/utils.R")
  
  #Se carga los datos con las ETL generales
  h.data <- loadData()
  
  #Generar la particion de test & train
  h.ntrain <- nTrain
  h.part <- partition_train_test_numeric(h.data, ntrain = h.ntrain)
  h.train <- h.part$train
  h.test <- h.part$test
  
  # Particion para cross validation
  h.cv_part <- partition_cv(df = h.train)
  
  #Variables
  h.k <- 2
  h.vars <- names(h.train)
  
  #Se elemina variable a predecir del data set de train y test
  #h.train$Churn <- NULL
  #h.test$Churn <- NULL
  
  # Formulas
  h.formula <- 'as.factor(Churn) ~ .'
  fn_err <- fn_err_cost #fn_err_cla
  
  #Predicción (k, train, test, y)
  h.knn_pred <- knn_pred(k = h.k,
                         train = h.train,
                         test = h.test,
                         y = h.train$Churn)
  
  # Error de predicción en knn
  h.error_knn <- knn_pred_err(list_pred = h.knn_pred,
                              newdata = h.test,
                              y = 'Churn',
                              k= h.k)
  #Imprimo resueltdo
  print(h.error_knn)
  
  
  # error CV 
  h.CV_err <- knn_cv_err(cv_part = h.cv_part,
                         cl = "Churn",
                         vars = h.vars,
                         k = h.k)
  
  
  
  #Plot del error en Knn para test y para CV
  ejex <- rev(c(1:h.k))/h.k
  plot(ejex, h.error_knn$err, type = 'l', ylim = c(-1, 0),
       col = 'red', xaxt = "n", main = 'error_Test_CV', xlab = "complexity", ylab = "error")
  axis(1,ejex, labels = ejex)
  lines(ejex, h.CV_err, col = 'green')
  legend("bottomleft", cex = 0.5,
         legend = c("eTest", "eCV"), 
         col = c("red", "green"),
         lty = rep(1, length(h.CV_err)))
  
  
