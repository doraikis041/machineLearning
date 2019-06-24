  source("Utility/utils.R")
  
  #Vector de umbrales para la predición
  h.umbral <- seq(0.32, to = 0.42, by = 0.01)
  
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
  h.rf_ctrl <- list(ctrl1 = list(ntree = 500, mtry = 5)
                    ,ctrl2 = list(ntree = 500, mtry = 6)
                    ,ctrl3 = list(ntree = 500, mtry = 7)
                    ,ctrl4 = list(ntree = 500, mtry = 8)
                    ,ctrl5 = list(ntree = 525, mtry = 5)
                    ,ctrl6 = list(ntree = 525, mtry = 6)
                    ,ctrl7 = list(ntree = 525, mtry = 7)
                    ,ctrl8 = list(ntree = 525, mtry = 8)
                    ,ctrl9 = list(ntree = 550, mtry = 5)
                    ,ctrl10 = list(ntree = 550, mtry = 6)
                    ,ctrl11 = list(ntree = 550, mtry = 7)
                    ,ctrl12 = list(ntree = 550, mtry = 8)
                    ,ctrl13 = list(ntree = 575, mtry = 5)
                    ,ctrl14 = list(ntree = 575, mtry = 6)
                    ,ctrl15 = list(ntree = 575, mtry = 7)
                    ,ctrl16 = list(ntree = 575, mtry = 8)
                    ,ctrl17 = list(ntree = 600, mtry = 5)
                    ,ctrl18 = list(ntree = 600, mtry = 6)
                    ,ctrl19 = list(ntree = 600, mtry = 7)
                    ,ctrl20 = list(ntree = 600, mtry = 8)
                    ,ctrl21 = list(ntree = 625, mtry = 5)
                    ,ctrl22 = list(ntree = 625, mtry = 6)
                    ,ctrl23 = list(ntree = 625, mtry = 7)
                    ,ctrl24 = list(ntree = 625, mtry = 8)  
                    
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
  plot_umbral_err(h.randomForest_predErr,
                  main = 'Error (Random Forest)',
                  umbral = h.umbral)
  
  
  #Mejores resueltados para cada kipotesis
  h.firstErr = 1
  h.first <- fn_order_error(listError = h.randomForest_predErr,
                            firstErr = h.firstErr)
  
  View(h.first$dfError)
  
  
  #Ejecución de las CV con los mejores errores
  h.rf_cv_automatic <- rfCVAutomatic(firstError = h.first,
                                       cv_part = h.cv_part, 
                                       formula = h.formula, 
                                       ctrl = h.gbm_ctrl, 
                                       umbral = h.umbral,
                                       var_y = 'Churn')
  
  print(h.rf_cv_automatic)
  
  
  # # Cross Validation
  # # Control 3 con umbral 0.38
  # h.rf_test_err_1 <- h.randomForest_predErr[[3]][4]
  # h.rf_cv_ctrl_1 <- list(h1 = h.rf_ctrl[[3]])
  # h.rf_cv_umbral_1 <- h.umbral[4]
  # h.rf_cv_err_1 <- rf_cv_err(h.cv_part, 
  #                            formula = h.formula, 
  #                            ctrl = h.rf_cv_ctrl_1, 
  #                            umbral = h.rf_cv_umbral_1,
  #                            var_y = 'Churn') 
  # print(paste('Error h1 -', 
  #             'umbral:', h.rf_cv_umbral_1, 
  #             'test:', h.rf_test_err_1, 
  #             'cv:', h.rf_cv_err_1))
  # 
  # # Control 5 con umbral 0.38
  # h.rf_test_err_2 <- h.randomForest_predErr[[5]][4]
  # h.rf_cv_ctrl_2 <- list(h2 = h.rf_ctrl[[5]])
  # h.rf_cv_umbral_2 <- h.umbral[4]
  # h.rf_cv_err_2 <- rf_cv_err(h.cv_part, 
  #                            formula = h.formula, 
  #                            ctrl = h.rf_cv_ctrl_2, 
  #                            umbral = h.rf_cv_umbral_2,
  #                            var_y = 'Churn')
  # print(paste('Error h2 -', 
  #             'umbral:', h.rf_cv_umbral_2, 
  #             'test:', h.rf_test_err_2, 
  #             'cv:', h.rf_cv_err_2))
  # 
  # 
  # print('Generacion de la prediccion sobre test sample')
  # 
  # 
  # 
  # 
  # h.test_sample <- read.csv('data/test_sample.csv')
  # h.CustomerID <- h.test_sample$CustomerID
  # h.test_sample$CustomerID <- NULL
  # h.test_sample$ServiceArea <- NULL
  # 
  # h.hip_prob <- ... # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  # h.hip_pred <- ... # calcular la prediccion para el umbral seleccionado
  # h.Churn <- as.logical(h.hip_pred[[1]][[1]]) # convertir 1->TRUE / 0->FALSE
  # 
  # print('Generar salida')
  # 
  # h.output <- data.frame(CustomerID = h.CustomerID, 
  #                        Churn = h.Churn)
  # write.csv(h.output, 
  #           file = "test_sample_pred.csv", 
  #           row.names = FALSE)
  # 
  # print('Done')
  # 
 