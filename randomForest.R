  source("Utility/utils.R")
  
  #Vector de umbrales para la predición
  h.umbral <- seq(0.1, to = 0.9, by = 0.1)
  
  #Se carga los datos con las ETL generales
  h.data <- loadData()
  
  #Generar la particion de test & train
  h.ntrain <- nTrain
  h.part <- partition_train_test(h.data, ntrain = h.ntrain)
  h.train <- h.part$train
  h.test <- h.part$test
  

  # Particion para cross validation
  h.cv_part <- partition_cv(df = h.train)
  
  # Formulas
  h.formula <- 'as.factor(Churn) ~ .'
  fn_err <- fn_err_cost #fn_err_cla
  
  #Definir el valor de mtry y ntree
  h.rf_ctrl <- list(ctrl1 = list(ntree = 100, mtry = 7)
                   #,ctrl2 = list(ntree = 200, mtry = 5)
                   # ,ctrl3 = list(ntree = 300, mtry = 5)
                   # ,ctrl4 = list(ntree = 100, mtry = 7)
                    #,ctrl5 = list(ntree = 200, mtry = 7)
                    #,ctrl6 = list(ntree = 300, mtry = 7)
  )
  
  
  # Entrenamiento de randomforest train, formula, ctrl
  h.randomForest_fit <- rf_fit_ctrl(h.train,
                                    h.formula,
                                    h.rf_ctrl)
  

  # Importancia de las variables para el primer control
  # importance(h.randomForest_fit[[1]])
  # varImpPlot(h.randomForest_fit[[1]])
  
  #Probabilidad en test
  h.randomForest_prob <- rf_prob(h.randomForest_fit,
                                 h.test)
  
  #Predicciones utilizando el umbral
  h.randomForest_pred <- rf_pred(h.randomForest_prob,
                                 h.umbral)
  
  #Error en test
  h.randomForest_predErr <- rf_pred_err(h.randomForest_pred,
                                        h.test$Churn)
  
  #Plot del error en RamdomForest
  plot_umbral_err(h.randomForest_predErr, main = 'Error (Random Forest)', umbral = h.umbral)
  
  
  #Mejores resueltados para cada kipotesis

  
  
  # Croos Validation
  # Hipotesis 1 con umbral 0.6
  h.rf_test_err_1 <- h.randomForest_predErr[[1]][6]
  h.rf_cv_ctrl_1 <- list(h1 = h.rf_ctrl[[1]])
  h.rf_cv_umbral_1 <- h.umbral[6]
  h.rf_cv_err_1 <- rf_cv_err(h.cv_part, 
                             formula = h.formula, 
                             ctrl = h.rf_cv_ctrl_1, 
                             umbral = h.rf_cv_umbral_1,
                             var_y = 'Churn') 
  print(paste('Error h1 -', 
              'umbral:', h.rf_cv_umbral_1, 
              'test:', h.rf_test_err_1, 
              'cv:', h.rf_cv_err_1))
  
  # Hipotesis 2 con umbral 0.5
  h.rf_test_err_2 <- h.randomForest_predErr[[2]][5]
  h.rf_cv_ctrl_2 <- list(h2 = h.rf_ctrl[[2]])
  h.rf_cv_umbral_2 <- h.umbral[5]
  h.rf_cv_err_2 <- rf_cv_err(h.cv_part, 
                             formula = h.formula, 
                             ctrl = h.rf_cv_ctrl_2, 
                             umbral = h.rf_cv_umbral_2,
                             var_y = 'Churn')
  print(paste('Error h2 -', 
              'umbral:', h.rf_cv_umbral_2, 
              'test:', h.rf_test_err_2, 
              'cv:', h.rf_cv_err_2))
  
  
  print('Generacion de la prediccion sobre test sample')
  
  h.test_sample <- read.csv('data/test_sample.csv')
  h.CustomerID <- h.test_sample$CustomerID
  h.test_sample$CustomerID <- NULL
  h.test_sample$ServiceArea <- NULL
  
  h.hip_prob <- ... # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  h.hip_pred <- ... # calcular la prediccion para el umbral seleccionado
  h.Churn <- as.logical(h.hip_pred[[1]][[1]]) # convertir 1->TRUE / 0->FALSE
  
  print('Generar salida')
  
  h.output <- data.frame(CustomerID = h.CustomerID, 
                         Churn = h.Churn)
  write.csv(h.output, 
            file = "test_sample_pred.csv", 
            row.names = FALSE)
  
  print('Done')
  
 