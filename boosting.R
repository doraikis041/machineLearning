  source("Utility/utils.R")
  
  #Vector de umbrales para la predición
  h.umbral <- seq(0.34, to = 0.42, by = 0.01)
  
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
  
  # Error en Cross Validation
  # Control 1 con umbral 0.37 (4)
  h.gbm_test_err_1 <- h.gbm_test_pred_err[[1]][4]
  h.gbm_cv_ctrl_1 <- list(h1 = h.gbm_ctrl[[1]])
  h.gbm_cv_umbral_1 <- h.umbral[4]
  h.gbm_cv_err_1 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_1, 
                               umbral = h.gbm_cv_umbral_1,
                               var_y = 'Churn') 
  print(paste('Error h1 -', 
              'umbral:', h.gbm_cv_umbral_1, 
              'test:', h.gbm_test_err_1, 
              'cv:', h.gbm_cv_err_1))
  
  # Control 2 con umbral 0.37 (4)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[2]][4]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[2]])
  h.gbm_cv_umbral_4 <- h.umbral[4]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h2 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  # Control 3 con umbral 0.36 (3)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[3]][3]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[3]])
  h.gbm_cv_umbral_4 <- h.umbral[3]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h3 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  # Control 4 con umbral 0.36 (3)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[4]][3]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[4]])
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
  
  
  # Control 5 con umbral 0.36 (3)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[5]][3]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[5]])
  h.gbm_cv_umbral_4 <- h.umbral[3]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h5 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  # Control 6 con umbral 0.37 (4)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[6]][4]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[6]])
  h.gbm_cv_umbral_4 <- h.umbral[4]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h6 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  
  # Control 7 con umbral 0.38 (5)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[7]][5]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[7]])
  h.gbm_cv_umbral_4 <- h.umbral[5]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h7 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  
  # Control 8 con umbral 0.38 (5)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[8]][5]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[8]])
  h.gbm_cv_umbral_4 <- h.umbral[5]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h8 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  # Control 9 con umbral 0.36 (3)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[9]][3]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[9]])
  h.gbm_cv_umbral_4 <- h.umbral[3]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h9 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  # Control 10 con umbral 0.36 (3)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[10]][3]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[10]])
  h.gbm_cv_umbral_4 <- h.umbral[3]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h10 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  # Control 11 con umbral 0.37 (4)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[11]][4]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[11]])
  h.gbm_cv_umbral_4 <- h.umbral[4]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h11 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  # Control 12 con umbral 0.37 (4)
  h.gbm_test_err_4 <- h.gbm_test_pred_err[[12]][4]
  h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[12]])
  h.gbm_cv_umbral_4 <- h.umbral[4]
  h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                               formula = h.gbm_formula, 
                               ctrl = h.gbm_cv_ctrl_4, 
                               umbral = h.gbm_cv_umbral_4,
                               var_y = 'Churn')
  print(paste('Error h12 -', 
              'umbral:', h.gbm_cv_umbral_4, 
              'test:', h.gbm_test_err_4, 
              'cv:', h.gbm_cv_err_4))
  
  
  
  
  
  
  
  
  
  
  print('Generacion de la prediccion sobre test sample')
  
  h.test_sample <- read.csv('Dataset/test_sample.csv')
  h.CustomerID <- h.test_sample$CustomerID
  h.test_sample$CustomerID <- NULL
  h.test_sample$ServiceArea <- NULL
  h.hip_fit <- list(h.gbm_fit[[4]])
  h.hip_umbral <- c(h.umbral[3])
  h.hip_ctrl <- list(h.gbm_ctrl[[4]])
  
  
  # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  h.hip_prob <- gbm_prob(list_fit = h.hip_fit
                           ,newdata = h.test_sample 
                           ,ctrl = h.hip_ctrl)

  # calcular la prediccion para el umbral seleccionado
  h.hip_pred <- fn_pred(test_prob = h.hip_prob
                        ,umbral = h.hip_umbral)
  
  h.Churn <- as.logical(h.hip_pred[[1]][[1]]) # convertir 1->TRUE / 0->FALSE
  
  print('Generar salida')
  
  h.output <- data.frame(CustomerID = h.CustomerID,
                         Churn = h.Churn)
  write.csv(h.output,
            file = "test_sample_pred2.csv",
            row.names = FALSE)
  
  print('Done')
  
  
  