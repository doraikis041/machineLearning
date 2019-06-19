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
  h.gbm_formula <- 'Churn ~ .'
  fn_err <- fn_err_cost #fn_err_cla
  
  h.gbm_ctrl <- list(ctrl1 = list(ntree = 100, depth = 1, shrinkage = 0.1)
                    # ,ctrl2 = list(ntree = 200, depth = 1, shrinkage = 0.01)
                    # ,ctrl3 = list(ntree = 300, depth = 1, shrinkage = 0.001)
                    # ,ctrl4 = list(ntree = 100, depth = 2, shrinkage = 0.1)
                     ,ctrl5 = list(ntree = 200, depth = 2, shrinkage = 0.01)
                    # ,ctrl6 = list(ntree = 300, depth = 2, shrinkage = 0.001)
  )
  
  # Entrenamiento de boosting train, formula, ctrl
  h.gbm_fit <- gbm_fit_ctrl(h.train,
                            h.gbm_formula,
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
  
  # Error en Croos Validation
  # Hipotesis 1 con umbral 0.5
  h.gbm_test_err_1 <- h.gbm_test_pred_err[[1]][5]
  h.gbm_cv_ctrl_1 <- list(h1 = h.gbm_ctrl[[1]])
  h.gbm_cv_umbral_1 <- h.umbral[5]
  h.gbm_cv_err_1 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_1, 
                               umbral = h.gbm_cv_umbral_1,
                               var_y = 'Churn') 
  print(paste('Error h1 -', 
              'umbral:', h.gbm_cv_umbral_1, 
              'test:', h.gbm_test_err_1, 
              'cv:', h.gbm_cv_err_1))
  
  # Hipotesis 4 con umbral 0.3
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[2]][3]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[2]])
  h.gbm_cv_umbral_4 <- h.umbral[3]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h4 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))