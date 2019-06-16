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
  h.mtry <- sqrt(length(h.train)) #h.mtry = 7 
  h.ntree <- c(100)
  
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
  importance(h.randomForest_fit[[1]])        
  varImpPlot(h.randomForest_fit[[1]])  
  
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
  h.rFTrain_pred <- h.rFtrain_pred_err$pred
  h.rFTrain_err <- h.rFtrain_pred_err$err

  #Cross Validation
  h.rF_cv_err <- randomForest_cv_err(cv_part = h.cv_part,
                                        formula = h.formula,
                                        y = 'Churn',
                                        pNtree = h.ntree,
                                        pMtry = h.mtry)
 
# Plot error
plot(h.rFTest_err, type = 'l', col = 'red', ylim = c(0., 0.4),
     main = 'Error de prediccion Rpart',
     ylab = 'Error de clasificacion',
     xlab = 'Hipotesis', xaxt = 'n')
lines(h.rFTrain_err, col = 'blue')
lines(h.rF_cv_err, col = 'magenta')
axis(1, at = seq(1, length(h.ctrl)))
 # legend("topright", legend = c('train', 'test', 'cv'),
 #       col = c('blue', 'red', 'magenta'), lty = c(1, 1, 1))


