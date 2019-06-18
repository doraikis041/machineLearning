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
  
  #Definir el valor de mtry
  h.rf_ctrl <- list(ctrl1 = list(ntree = 100, mtry = 5)
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