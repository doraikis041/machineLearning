  source("Utility/utils.R")
  
  #Vector de umbrales para la predición
  h.umbral <- seq(0.30, to = 0.42, by = 0.01)
  
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
  h.formula <- 'Churn ~ .'
  fn_err <- fn_err_cost #fn_err_cla
  
  h.gbm_ctrl <- list(ctrl1 = list(ntree = 600, depth = 20, shrinkage = 0.01)
                     ,ctrl2 = list(ntree = 600, depth = 25, shrinkage = 0.01)
                     ,ctrl3 = list(ntree = 600, depth = 30, shrinkage = 0.01)
                     ,ctrl4 = list(ntree = 625, depth = 20, shrinkage = 0.01)
                     ,ctrl5 = list(ntree = 625, depth = 25, shrinkage = 0.01)
                     ,ctrl6 = list(ntree = 625, depth = 30, shrinkage = 0.01)
                     ,ctrl7 = list(ntree = 650, depth = 20, shrinkage = 0.01)
                     ,ctrl8 = list(ntree = 650, depth = 25, shrinkage = 0.01)
                     ,ctrl9 = list(ntree = 650, depth = 30, shrinkage = 0.01)
                     ,ctrl10 = list(ntree = 675, depth = 20, shrinkage = 0.01)
                     ,ctrl11 = list(ntree = 675, depth = 25, shrinkage = 0.01)
                     ,ctrl12 = list(ntree = 675, depth = 30, shrinkage = 0.01)
                     
  )
  
  # Entrenamiento de boosting train, formula, ctrl
  h.gbm_fit <- gbm_fit_ctrl(h.train,
                            h.formula,
                            ctrl = h.gbm_ctrl)
  
  # Probabilidad en test. Parametros de entradalist_fit, newdata, ctrl
  h.gbm_test_prob <- gbm_prob(list_fit = h.gbm_fit,
                              newdata = h.test,
                              ctrl = h.gbm_ctrl)
  
  #Predicciones utilizando el umbral
  h.gbm_test_pred <- gbm_pred(h.gbm_test_prob,
                              h.umbral)
  
  # Error en test
  h.gbm_test_pred_err <- gbm_pred_err(h.gbm_test_pred,
                                      h.test$Churn)
  
  #Plot del error en Boosting (GBM)
  plot_umbral_err(h.gbm_test_pred_err,
                  main = 'Error de GBM',
                  umbral = h.umbral)
  
  #Mejores resueltados para cada kipotesis
  h.firstErr = 5
  h.first <- fn_order_error(listError = h.gbm_test_pred_err,
                            firstErr = h.firstErr)
  
  View(h.first$dfError)
  
  
  #Ejecución de las CV con los mejores errores
  h.gbm_cv_automatic <- gbmCVAutomatic(firstError = h.first,
                                         cv_part = h.cv_part, 
                                         formula = h.formula, 
                                         ctrl = h.gbm_ctrl, 
                                         umbral = h.umbral,
                                         var_y = 'Churn')
  
  print(h.gbm_cv_automatic)
  

  # print('Generacion de la prediccion sobre test sample')
  # 
  # h.test_sample <- read.csv('Dataset/test_sample.csv')
  # h.CustomerID <- h.test_sample$CustomerID
  # h.test_sample$CustomerID <- NULL
  # h.test_sample$ServiceArea <- NULL
  # h.hip_fit <- list(h.gbm_fit[[4]])
  # h.hip_umbral <- c(h.umbral[3])
  # h.hip_ctrl <- list(h.gbm_ctrl[[4]])
  # 
  # 
  # # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  # h.hip_prob <- gbm_prob(list_fit = h.hip_fit
  #                          ,newdata = h.test_sample 
  #                          ,ctrl = h.hip_ctrl)
  # 
  # # calcular la prediccion para el umbral seleccionado
  # h.hip_pred <- fn_pred(test_prob = h.hip_prob
  #                       ,umbral = h.hip_umbral)
  # 
  # h.Churn <- as.logical(h.hip_pred[[1]][[1]]) # convertir 1->TRUE / 0->FALSE
  # 
  # print('Generar salida')
  # 
  # h.output <- data.frame(CustomerID = h.CustomerID,
  #                        Churn = h.Churn)
  # write.csv(h.output,
  #           file = "test_sample_pred2.csv",
  #           row.names = FALSE)
  # 
  # print('Done')
  # 
  
  