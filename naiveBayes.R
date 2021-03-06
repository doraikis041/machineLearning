  source("Utility/utils.R")

  #Se carga los datos con las ETL generales
  h.data <- loadData()
  
  #definición de las formulas a utilizar
  h.formula <- c('as.factor(Churn) ~ ThreewayCalls',
                 'as.factor(Churn) ~ CreditRating',
                 'as.factor(Churn) ~ .')
  fn_err <- fn_err_cost #fn_err_cla
  
  #Generar la particion de test & train
  h.part <- partition_train_test(h.data, ntrain = nTrain)
  h.train <- h.part$train
  h.test <- h.part$test
  
  #Entrenamiento del algoritmo
  h.fits <- naiveBayes_fit_formulas(train = h.train, formulas = h.formula)
  
  #Error de Prediccion del algoritmo
  h.error_predit <- nbayes_pred_err(list_fit = h.fits, newdata = h.test, 'Churn')
  
  
  #Cross Validation Naive Bayes
  h.cv_part <- partition_cv(df = h.train)
  h.cv <- cv_err_nBayes(cv_part = h.cv_part, formulas = h.formula,'Churn')
  names(h.cv) <- h.formula
  print(sort(h.cv))