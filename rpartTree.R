  #source("Utility/functions.R")
  source("Utility/utils.R")
 
  #Umbral para la ejecución de la matriz de confusión
  h.umbral <- seq(0.32, to = 0.48, by = 0.02)
  
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
  h.rpart_ctrl <- list(ctrl1 = rpart.control(minsplit = 1, maxdepth = 15, cp = 0.00001)
                      ,ctrl2 = rpart.control(minsplit = 1, maxdepth = 15, cp = 0.0001)
                      ,ctrl3 = rpart.control(minsplit = 1, maxdepth = 15, cp = 0.001)
                      ,ctrl4 = rpart.control(minsplit = 1, maxdepth = 20, cp = 0.00001)
                      ,ctrl5 = rpart.control(minsplit = 1, maxdepth = 20, cp = 0.0001)
                      ,ctrl6 = rpart.control(minsplit = 1, maxdepth = 20, cp = 0.001)
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
  h.firstErr = 2
  h.first <- fn_order_error(listError = h.rpart_pred_err,
                            firstErr = h.firstErr)
  
  #Ejecución de las CV con los mejores errores
  rpartCVAutomatic.r <- rpartCVAutomatic(h.first = firstResult,
                                         cv_part = h.cv_part, 
                                         formula = h.formula, 
                                         ctrl = h.rpart_ctrl, 
                                         umbral = h.umbral,
                                         var_y = 'Churn')
  
  
  print('Generacion de la prediccion sobre test sample')
  
  h.test_sample <- read.csv('Dataset/test_sample.csv')
  h.CustomerID <- h.test_sample$CustomerID
  h.test_sample$CustomerID <- NULL
  h.test_sample$ServiceArea <- NULL
  h.hip_fit <- list(h.rpart_fit[[4]])
  h.hip_umbral <- c(h.umbral[4])

  # calcular la probabilidad de la hipotesis seleccionada sobre test_sample
  h.hip_prob <- rpart_prob(list_fit = h.hip_fit
                          ,newdata = h.test_sample)

  # calcular la prediccion para el umbral seleccionado
  h.hip_pred <- fn_pred(test_prob = h.hip_prob
                        ,umbral = h.hip_umbral)
  
  h.Churn <- as.logical(h.hip_pred[[1]][[1]]) # convertir 1->TRUE / 0->FALSE

  print('Generar salida')

  h.output <- data.frame(CustomerID = h.CustomerID,
                         Churn = h.Churn)
  write.csv(h.output,
            file = "test_sample_pred.csv",
            row.names = FALSE)

  print('Done')


