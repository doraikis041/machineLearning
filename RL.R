  source("Utility/utils.R")

  
  #Se carga los datos con las ETL generales
  h.data <- loadData()
  
  #Generar la particion de test & train
  h.ntrain <- nTrain
  h.part <- partition_train_test_numeric(h.data, ntrain = h.ntrain)
  h.train <- h.part$train
  h.test <- h.part$test
  
  
  # Particion para cross validation
  h.cv_part <- partition_cv(df = h.train)
  
  # Formulas
  h.formulas <- c("as.factor(Churn) ~ .")
  fn_err <- fn_err_cost #fn_err_cla


  # Entrenamiento de GLM
  h.glm_fit <- glm_fit_formulas(train = h.train
                                ,formulas = h.formulas)

 
  # Calculo de error en test y matriz de confunsion utilizando 
  
  h.glm_pred_err_mc <- glm_pred_err_mc(list_fit = h.glm_fit,
                                       newdata = h.test,
                                       y = "Churn")
  
  #print de los resultados
  print(h.glm_pred_err_mc$err)
  print(h.glm_pred_err_mc$mc)
    

  # Cross Validation
  h.cv_err <- cv_err(cv_part = h.cv_part
                     ,formulas = h.formulas
                     ,y = "Churn")

  
  # Print del valor medio del error generado en CV
  print(mean(h.cv_err))