  #source("Utility/functions.R")
  source("Utility/utils.R")
 
  #Umbral para la ejecución de la matriz de confusión
  h.umbral <- seq(0.3, to = 0.7, by = 0.1)
  
  #Se carga los datos con las ETL generales
  h.data <- loadData()
  
  #Generar la particion de test & train
  h.ntrain <- nTrain
  h.part <- partition_train_test(h.data, ntrain = nTrain)
  h.train <- h.part$train
  h.test <- h.part$test
  
  
  # Particion para cross validation con k_folds = 5
  h.cv_part <- partition_cv(df = h.train)
  
  # Formulas
  h.formula <- 'as.factor(Churn) ~ .'
  fn_err <- fn_err_cost #fn_err_cla
  
  # cp mientras mas chico es cp mas complejo es
  h.rpart_ctrl <- list(ctrl1 = rpart.control(minsplit = 1, maxdepth = 5, cp = 0.0001)
                      ,ctrl2 = rpart.control(minsplit = 1, maxdepth = 5, cp = 0.001)
                      ,ctrl3 = rpart.control(minsplit = 1, maxdepth = 5, cp = 0.01)
                      )
  
  #Entrenamiento de Tree (Rpart) train, formula, ctrl
  h.rpart_fit <- rpart_fit_ctrl(train = h.train,
                               formula = h.formula,
                               ctrl = h.rpart_ctrl)

  
  
  # Probabilidad list_fit, newdata, ctrl
  h.rpart_prob <- rpart_prob(list_fit = h.rpart_fit
                  ,newdata = h.test)
  
 #Predicción a partir del umbral: Parametros test_prob, umbral
  h.rpart_pred <- fn_pred(test_prob = h.rpart_prob
                          ,umbral = h.umbral)
  
  #Error en test
  h.rpart_pred_err <- fn_pred_err(test_pred = h.rpart_pred
                               ,y = h.test$Churn)
  
  
  #Plot del error
  plot_umbral_err(h.rpart_pred_err,
                  main = 'Error de Rpart',
                  umbral = h.umbral)
  
  
  #Mejores resueltados para cada kipotesis
  
  # Error en Croos Validation
  # Hipotesis 1 con umbral 0.4
  h.rpart_test_err_1 <- h.rpart_pred_err[[1]][4]
  h.rpart_cv_ctrl_1 <- list(h1 = h.rpart_ctrl[[1]])
  h.rpart_cv_umbral_1 <- h.umbral[4]
  #cv_part, formula, ctrl, y
  h.rpart_cv_err_1 <- rpart_cv_err(cv_part = h.cv_part, 
                                  formula = h.formula, 
                                  ctrl = h.rpart_cv_ctrl_1, 
                                  umbral = h.rpart_cv_umbral_1,
                                  var_y = 'Churn') 
  print(paste('Error h1 -', 
              'umbral:', h.rpart_cv_umbral_1, 
              'test:', h.rpart_test_err_1, 
              'cv:', h.rpart_cv_err_1))
  
  # Hipotesis 4 con umbral 0.3
  h.rpart_test_err_2 <- h.rpart_pred_err[[2]][3]
  h.rpart_cv_ctrl_2 <- list(h1 = h.rpart_ctrl[[2]])
  h.rpart_cv_umbral_2 <- h.umbral[3]
  #cv_part, formula, ctrl, y
  h.rpart_cv_err_2 <- rpart_cv_err(cv_part = h.cv_part, 
                                   formula = h.formula, 
                                   ctrl = h.rpart_cv_ctrl_2, 
                                   umbral = h.rpart_cv_umbral_2,
                                   var_y = 'Churn') 
  print(paste('Error h1 -', 
              'umbral:', h.rpart_cv_umbral_2, 
              'test:', h.rpart_test_err_2, 
              'cv:', h.rpart_cv_err_2))
  
  
  print('Generacion de la prediccion sobre test sample')
  
  h.test_sample <- read.csv('Dataset/test_sample.csv')
  h.CustomerID <- h.test_sample$CustomerID
  h.test_sample$CustomerID <- NULL
  h.test_sample$ServiceArea <- NULL
  
  # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  h.hip_prob <- rpart_prob(list_fit = h.rpart_fit[[]]
                                           ,newdata = h.test_sample)
  # calcular la prediccion para el umbral seleccionado
  h.hip_pred <- h.rpart_pred <- fn_pred(test_prob = h.rpart_prob
                                        ,umbral = h.umbral[[2]])
# 
#   h.Churn <- as.logical(h.hip_pred[[1]][[1]]) # convertir 1->TRUE / 0->FALSE
#   
#   print('Generar salida')
#   
#   h.output <- data.frame(CustomerID = h.CustomerID, 
#                          Churn = h.Churn)
#   write.csv(h.output, 
#             file = "test_sample_pred.csv", 
#             row.names = FALSE)
#   
#   print('Done')
  
  
  
  
  
  
  
  
  
   ## Anterior codigo ####
  
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
  
