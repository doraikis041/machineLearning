  source("Utility/utils.R")
  
  #Vector de umbrales para la predición
  h.umbral <- seq(0.32, to = 0.42, by = 0.01)
  
  #Se carga los datos con las ETL generales
  h.data <- loadDataRF()
  
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
  h.rf_ctrl <- list(
                    ctrl1 = list(ntree = 500, mtry = 6)
                    ,ctrl2 = list(ntree = 500, mtry = 7)
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

                    
  )
  

  # Entrenamiento de randomforest (train, formula, ctrl)
  h.randomForest_fit <- rf_fit_ctrl(train = h.train,
                                    formula = h.formula,
                                    ctrl = h.rf_ctrl)
  

  #Probabilidad en test (list_fit, newdata)
  h.randomForest_prob <- rf_prob(list_fit = h.randomForest_fit,
                                 newdata = h.test)
  
  #Predicciones utilizando el umbral (test_prob, umbral)
  h.randomForest_pred <- rf_pred(test_prob = h.randomForest_prob,
                                 umbral = h.umbral)
  
  #Error en test (test_pred, y)
  h.randomForest_predErr <- rf_pred_err(test_pred = h.randomForest_pred,
                                        y = h.test$Churn)
  
  #Plot del error en RamdomForest
  plot_umbral_err(h.randomForest_predErr,
                  main = 'Error (Random Forest)',
                  umbral = h.umbral)
  
  
  #Mejores resueltados para cada kipotesis
  h.iniErr = 1
  h.lastErr = 3
  h.first <- fn_order_error(listError = h.randomForest_predErr,
                            iniErr = h.iniErr,
                            lastErr = h.lastErr)

  #Ejecución de las CV con los mejores errores. Se identifica el minimo error por cada control
  h.rf_cv_automatic <- rfCVAutomatic(firstError = h.first,
                                       cv_part = h.cv_part, 
                                       formula = h.formula, 
                                       ctrl = h.rf_ctrl, 
                                       umbral = h.umbral,
                                       var_y = 'Churn')
  
  print(h.rf_cv_automatic)
  
  
  # print('Generacion de la prediccion sobre test sample') 
  # h.test_sample <- read.csv('data/test_sample.csv')
  # h.test_sample <- na.roughfix(h.test_sample)
  # h.CustomerID <- h.test_sample$CustomerID
  # h.test_sample$CustomerID <- NULL
  # h.test_sample$ServiceArea <- NULL
  #
  # h.hip_fit <- list(h.rf_fit[[4]])
  # h.hip_umbral <- c(h.umbral[3])
  # h.hip_ctrl <- list(h.rf_ctrl[[4]])
  # 
  # 
  # # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  # h.hip_prob <- fr_prob(list_fit = h.hip_fit
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
  #           file = "test_sample_pred_RF.csv",
  #           row.names = FALSE)
  # 
  # print('Done')
  # 
  
 