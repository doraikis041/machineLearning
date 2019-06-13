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
  h.mtry <- sqrt(length(h.train)) 
  
  # Prueba de la llamada al algoritmo
  a <-randomForest(as.factor(Churn)~.,data = h.train, ntree= 100, mtry = h.mtry)

# # Clasificacion con rpart
# h.tree_fit <- rpart_fit_ctrl(h.train, h.formula, h.ctrl, method = 'class', parms)
# 
# 
# 
# # Error en test
# h.rpart_test_pred_err <- rpart_pred_err(h.tree_fit, newdata = h.test, y = 'Churn')
# h.rpart_test_pred <- h.rpart_test_pred_err$pred
# h.rpart_test_err <- h.rpart_test_pred_err$err
# # 
# # Error en train
# h.rpart_train_pred_err <- rpart_pred_err(h.tree_fit, newdata = h.train, y = 'Churn')
# h.rpart_train_pred <- h.rpart_train_pred_err$pred
# h.rpart_train_err <- h.rpart_train_pred_err$err
# 
# 
# h.rpart_cv_err <- rpart_cv_err(h.cv_part, h.formula, h.ctrl, y = 'Churn')
# 
# 
# 
# h.rpart_mc_err <- rpart_pred_err_mc(h.tree_fit, newdata = h.train, y = 'Churn',type = 'class',umbral = umbral)
# 
# # Plot error
# plot(h.rpart_test_err, type = 'l', col = 'red', ylim = c(0., 0.3),
#      main = 'Error de prediccion Rpart',
#      ylab = 'Error de clasificacion',
#      xlab = 'Hipotesis', xaxt = 'n')
# lines(h.rpart_train_err, col = 'blue')
# lines(h.rpart_cv_err, col = 'magenta')
# axis(1, at = seq(1, length(h.ctrl)))
# # legend("topright", legend = c('train', 'test', 'cv'),
# #        col = c('blue', 'red', 'magenta'), lty = c(1, 1, 1))
# 
# 
