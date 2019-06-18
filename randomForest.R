  source("Utility/functions.R")
  
  #Umbral para la ejecución de la matriz de confusión
  umbral <- seq(0.3, to = 0.7, by = 0.1)
  
  #Se carga los datos con las ETL generales
  h.data <- loadData()
  
  #Generar la particion de test & train
  h.part <- partition_train_test(h.data, ntrain = nTrain)
  h.train <- h.part$train
  h.test <- h.part$test
  

  # Particion para cross validation con k_folds = 5
  h.cv_part <- partition_cv(df = h.train,k_folds = 5 )
  
  # Formulas
  h.formula <- 'as.factor(Churn) ~ .'
  
  #Definir el valor de mtry
  #h.ctrl <- list(list(ntree=200, mtry= 5))
  h.mtry <-  5 #sqrt(length(h.train))
  h.ntree <- c(50)
  
  # Clasificacion con randomforest
  h.randomForest_fit <- randomForest_fit(train = h.train,
                                         formula = h.formula,
                                         pNtree = h.ntree,
                                         pMtry = h.mtry)
  # Prueba de la llamada al algoritmo
  # a <-randomForest(as.factor(Churn)~.,data = h.train, ntree= 500, mtry = h.mtry)
  # print(a)
  # plot(a)
  
  
  # Importancia de las variables
  # importance(h.randomForest_fit[[1]])        
  # varImpPlot(h.randomForest_fit[[1]])  
  
  ######  Error en test
  h.rFTest_pred_err <- rpart_pred_err(h.randomForest_fit,
                                      newdata = h.test,
                                      y = 'Churn')
  h.rFTest_pred <- h.rFTest_pred_err$pred
  h.rFTest_err <- h.rFTest_pred_err$err

  ###### Error en train
  h.rFTrain_pred_err <- rpart_pred_err(h.randomForest_fit,
                                       newdata = h.train,
                                       y = 'Churn')
  h.rFTrain_pred <- h.rFTrain_pred_err$pred
  h.rFTrain_err <- h.rFTrain_pred_err$err

  #Cross Validation
  h.rF_cv_err <- randomForest_cv_err(cv_part = h.cv_part,
                                        formula = h.formula,
                                        y = 'Churn',
                                        pNtree = h.ntree,
                                        pMtry = h.mtry)
  
  print(h.rFTest_err)
  print( h.rFTrain_err)
  print(h.rF_cv_err)
 
  
  
  # plot_umbral_err <- function(list_err, umbral, main = '') {
  #   # list_err: lista de vectores de la misma longitud
  #   col_vec <- rainbow(length(list_err))
  #   plot(list_err[[1]], 
  #        type = 'l', 
  #        col = col_vec[1], 
  #        main = main, 
  #        ylab = 'Error', 
  #        xlab = 'Umbral', 
  #        xaxt = 'n')
  #   axis(1, 
  #        at = seq(1, length(umbral)), 
  #        labels = umbral)
  #   for (i in seq(2, length(list_err))) {
  #     lines(list_err[[i]], col = col_vec[i])
  #   }
  #   legend("bottomleft", 
  #          cex = 0.5,
  #          legend = seq(1, length(list_err)), 
  #          col = col_vec,
  #          lty = rep(1, length(list_err)))
  # }
  # 
  # plot_umbral_err()
  

#plot_umbral_err(h.gbm_test_pred_err, main = 'Error de GBM', umbral = h.umbral)
  
  
  
#Plot error
plot(h.rF_cv_err)
points(h.rFTrain_err, col = 'blue')
points(h.rF_cv_err, col = 'magenta')
# , type = 'l', col = 'red'),
#      main = 'Error de prediccion Rpart',
#      ylab = 'Error de clasificacion',
#      xlab = 'Hipotesis', xaxt = 'n')
# lines(h.rFTrain_err, col = 'blue')
# lines(h.rF_cv_err, col = 'magenta')
# axis(1, at = seq(1, length(1))) # axis(1, at = seq(1, length(h.ctrl)))
#  # legend("topright", legend = c('train', 'test', 'cv'),
#  #       col = c('blue', 'red', 'magenta'), lty = c(1, 1, 1))
# 


