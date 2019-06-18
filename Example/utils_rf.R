library(randomForest)

# Train random forest

rf_fit_ctrl <- function(train, formula, ctrl) {
  # train: datos de entrenamiento
  # formula: una formula (para clasificacion usar as.factor en la variable a predecir)
  # ctrl: es una lista de listas cada lista en ctrl tiene dos atributos: ntree y mtry
  list_fit <- list()
  for (i in seq(1, length(ctrl))) {
    list_fit[[i]] <- randomForest(as.formula(formula), 
                           data = train, 
                           ntree = ctrl[[i]]$ntree,
                           mtry = ctrl[[i]]$mtry)
  }
  return(list_fit)
}

# Prediccion

rf_prob <- function(list_fit, newdata) {
  # list_fit: lista retornada por rf_fit_ctrl
  # newdata: datos de test
  test_prob <- list()
  for (i in seq(1, length(list_fit))) {
    # prediccion
    test_prob[[i]] <- predict(list_fit[[i]], 
                              newdata = newdata, 
                              type = 'prob')[,2]
  }
  return(test_prob)
}

# Prediccion para un vector de umbrales

rf_pred <- function(test_prob, umbral) {
  # test_prob: lista retornada por rf_prob
  # umbral: vector de umbrales entre 0 y 1
  test_pred <- list()
  for (i in seq(1, length(test_prob))) {
    test_pred[[i]] <- list()
    for (j in seq(1, length(umbral))) {
      # prediccion
      test_pred[[i]][[j]] <- ifelse(test_prob[[i]] > umbral[j], 1, 0)
    }
  }
  return(test_pred)
}

# Error de prediccion para una lista de predicciones

rf_pred_err <- function(test_pred, y) {
  # test_pred: lista retornada por rf_pred
  test_pred_err <- list()
  for (i in seq(1, length(test_pred))) {
    test_pred_err[[i]] <- rep(0, length(test_pred[[i]]))
    for (j in seq(1, length(test_pred[[i]]))) {
      # error
      test_pred_err[[i]][j] <- fn_err(test_pred[[i]][[j]], y)
    }
  }
  return(test_pred_err)
}

# Error de CV

rf_cv_err <- function(cv_part, formula, ctrl, umbral = 0.5, var_y) {
  # cv_part: particion generada por partition_cv
  # formula: formula
  # ctrl: lista de controles de rf
  # umbral: umbral para calcular la prediccion
  # y: variable a predecir
  cv_test <- cv_part$test
  cv_train <- cv_part$train
  cv_err <- list()
  for (k in seq(1, cv_part$k_folds)) {
    # fit
    list_fit <- rf_fit_ctrl(cv_train[[k]], 
                            formula = formula, 
                            ctrl = ctrl)
    # prob
    list_prob <- rf_prob(list_fit, 
                         newdata = cv_test[[k]])
    # pred
    list_pred <- rf_pred(list_prob, 
                         umbral = umbral) 
    # error
    cv_err[[k]] <- rf_pred_err(list_pred, 
                             cv_test[[k]][[var_y]])
  }
  return(mean(unlist(cv_err)))
}
