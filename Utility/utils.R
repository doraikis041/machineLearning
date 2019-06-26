  #Librerias Principales
  library(class)
  library(e1071)
  #Tree
  library(rpart)
  library(rpart.plot)
  library(dplyr)
  #RandomForest
  library(randomForest)
  #Boosting
  library(gbm)
  
  #Definiendo el valor de see para los datos aleatorios generados
  set.seed(117)
  
  #Variables Generales
  nTrain <- 37445
  
  ## Carga de los datos principales con las ETL generales para todos los algoritmos
  loadData <- function()
  {
    T0 <- read.csv(file = "Dataset/dataset.csv")
    T0$CustomerID <- NULL
    T0$ServiceArea <- NULL
    T0$Churn <- ifelse(T0$Churn == 'Yes',1,0)
    df <- na.omit(T0)
  }
  
  ## Solo para RandomForest
  loadDataRF <- function()
  {
    T0 <- read.csv(file = "Dataset/dataset.csv")
    T0$CustomerID <- NULL
    T0$ServiceArea <- NULL
    T0$Churn <- ifelse(T0$Churn == 'Yes',1,0)
    df <- na.roughfix(T0)
  }
  
  
  #Error de clasificacion
  fn_err_cla <- function(yhat, y) { mean(yhat != y) }
  
  # costo
  fn_err_cost <- function(yhat, y) { 
    fp_cost <- 10 * sum( yhat == 1 & y == 0 )
    tp_cost <- -25.5 * sum( yhat == 1 & y == 1 )
    return((fp_cost + tp_cost) / length(y)) 
  }
  
  # Prediccion para un vector de umbrales
  fn_pred <- function(test_prob, umbral) {
    # test_prob: lista retornada para varios algoritmos
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
  fn_pred_err <- function(test_pred, y) {
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
  
  #Obtener las hipotesis mas bajas para usarlas en CV se inicializa firstErr=3
  fn_order_error <- function(listError,iniErr = 1, lastErr=3)
  {
    auxError <- list()
    
    for (i in seq(length(listError))) {
      auxError[[i]] <- c(error = min(listError[[i]]),
                         control = i,
                         umbral = match(min(listError[[i]]),listError[[i]]))
      
    }
    
    df <- as.data.frame(t(as.data.frame(auxError)))
    
    #Order los resultados
    df.order <- arrange(df, df$error)
    
    #Obtener los firstErr=2 primeros
    df.out <- df.order[iniErr:lastErr,]
    
    
    list(dfError=df.out,iniErr = iniErr, lastErr = lastErr)
    
  }
  
  ## Creación de la particion train-test
  partition_train_test <- function(df, ntrain = 100) {
    train_idx <- sample.int(nrow(df), size = ntrain)
    list(train = df[train_idx,], test = df[-train_idx,])
  }
  
  ## Creación de la particion train-test solo numeric conviertiendo Churn a númerico antes
  partition_train_test_numeric <- function(df, ntrain = 10) {
    df$Churn <- as.numeric(df$Churn)
    df_numeric <- which(sapply(df,is.numeric))
    dfN <- df[,df_numeric]
    train_idx <- sample.int(nrow(dfN), size = ntrain)
    list(train = dfN[train_idx,], test = dfN[-train_idx,])
  }
  
  
  # Particion en 5 folds
  partition_cv <- function(df, k_folds = 5) {
    cv_test <- split(df, seq(1, k_folds))
    cv_train <- list()
    for (k in seq(1, k_folds)) {
      cv_train[[k]] <- data.frame()
      for (i in seq(1, k_folds)) {
        if (i != k) cv_train[[k]] <- rbind(cv_train[[k]], cv_test[[i]])
      }
    }
    list(train = cv_train, test = cv_test, k_folds = k_folds)
  }
  
  # Plotear error para una lista de vectores de error
  plot_umbral_err <- function(list_err, umbral, main = '') {
    # list_err: lista de vectores de la misma longitud
    col_vec <- rainbow(length(list_err))
    indxMin <- vector()
    for (i in seq(1, length(list_err))) {
      indxMin[i] <- min(unlist(list_err[[i]]))
    }
    indexMin <- as.numeric(which.min(indxMin))
    plot(list_err[[indexMin]]
         ,type = 'l'
         ,col = col_vec[indexMin]
         ,main = main
         ,ylab = 'Error' 
         ,xlab = 'Umbral'
         ,xaxt = 'n'
         ,ylim = c(min(unlist(list_err)),max(unlist(list_err)))
    )
    axis(1, at = seq(1, length(umbral)), 
         labels = umbral)
    for (i in seq(2, length(list_err))) {
      lines(list_err[[i]], col = col_vec[i])
    }
    legend("bottomleft", 
           cex = 0.5,
           legend = seq(1, length(list_err)), 
           col = col_vec,
           lty = rep(1, length(list_err)))
  }
 
  ##################################################################################################
  #############################        RandomForest RF             #################################
  ################################################################################################## 

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
  
  #Ejecuta el CV para las primeras mejores resultados
  rfCVAutomatic <- function(firstError, cv_part, formula, ctrl, umbral, var_y)
  {
    result <- list()
    for (i in seq(nrow(firstError$dfError))) {
      h.control <- list(h1 = ctrl[[firstError$dfError$control[i]]])
      h.humbral <- h.umbral[firstError$dfError$umbral[i]]
      
      #cv_part, formula, ctrl, y
      h.cv_error <- rf_cv_err(cv_part = h.cv_part,
                              formula = h.formula,
                              ctrl = h.control,
                              umbral = h.humbral,
                              var_y = 'Churn') 
      
      
      result[[i]] <- c(mensaje = paste(paste('Error H', i, " - "), 
                                       'umbral:', h.humbral, 
                                       'test:', firstError$dfError$error[i], 
                                       'cv:', h.cv_error),
                       errorTest = firstError$dfError$error[i],
                       errorCV = h.cv_error
      )
    }
    
    return(result) 
  }
  
  #************** Boosting ******************
  gbm_fit_ctrl <- function(train, formula, ctrl) {
    # train: datos de entrenamiento
    # formula: una formula (para clasificacion usar as.factor en la variable a predecir)
    # ctrl: es una lista de listas cada lista en ctrl tiene dos atributos: ntree y mtry
    list_fit <- list()
    for (i in seq(1, length(ctrl))) {
      list_fit[[i]] <- gbm(formula = as.formula(formula), 
                           data = train, 
                           distribution = 'bernoulli',
                           n.trees = ctrl[[i]]$ntree,
                           interaction.depth = ctrl[[i]]$depth,
                           shrinkage = ctrl[[i]]$shrinkage)
    }
    return(list_fit)
  }
  
  # Prediccion
  gbm_prob <- function(list_fit, newdata, ctrl) {
    # list_fit: lista retornada por gbm_fit_ctrl
    # newdata: datos de test
    test_prob <- list()
    for (i in seq(1, length(list_fit))) {
      # prediccion
      test_prob[[i]] <- predict(list_fit[[i]], 
                                newdata = newdata, 
                                n.trees = ctrl[[i]]$ntree,
                                type = 'response')
    }
    return(test_prob)
  }
  
  # Prediccion para un vector de umbrales
  gbm_pred <- function(test_prob, umbral) {
    # test_prob: lista retornada por gbm_prob
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
  gbm_pred_err <- function(test_pred, y) {
    # test_pred: lista retornada por gbm_pred
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
  gbm_cv_err <- function(cv_part, formula, ctrl, umbral = 0.5, var_y) {
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
      list_fit <- gbm_fit_ctrl(cv_train[[k]], 
                               formula = formula, 
                               ctrl = ctrl)
      # prob
      list_prob <- gbm_prob(list_fit, 
                            newdata = cv_test[[k]],
                            ctrl = ctrl)
      # pred
      list_pred <- gbm_pred(list_prob, 
                            umbral = umbral) 
      # error
      cv_err[[k]] <- gbm_pred_err(list_pred, 
                                  cv_test[[k]][[var_y]])
    }
    return(mean(unlist(cv_err)))
  }
  
  #Ejecuta el CV para las primeras mejores resultados
  gbmCVAutomatic <- function(firstError, cv_part, formula, ctrl, umbral, var_y)
  {
    result <- list()
    for (i in seq(nrow(firstError$dfError))) {
      h.control <- list(h1 = ctrl[[firstError$dfError$control[i]]])
      h.humbral <- h.umbral[firstError$dfError$umbral[i]]
      
      #cv_part, formula, ctrl, y
      h.cv_error <- gbm_cv_err(cv_part = h.cv_part,
                               formula = h.formula,
                               ctrl = h.control,
                               umbral = h.humbral,
                               var_y = 'Churn') 
      
      
      result[[i]] <- c(mensaje = paste(paste('Error H', i, " - "), 
                                       'umbral:', h.humbral, 
                                       'test:', firstError$dfError$error[i], 
                                       'cv:', h.cv_error),
                       errorTest = firstError$dfError$error[i],
                       errorCV = h.cv_error
      )
    }
    
    return(result) 
  }
  
  #######################################################################
  ##################       Rpart                       ##################
  #######################################################################
  # Clasificacion con rpart para una lista de formulas
  rpart_fit_formulas <- function(train, formulas, method = 'class') {
    list_fit <- list()
    for (i in seq(1, length(formulas))) {
      list_fit[[i]] <- rpart(as.formula(formulas[i]), 
                             data = train, 
                             method = method)
    }
    return(list_fit)
  }
  
  # Clasificacion con rpart para una lista de cp
  rpart_fit_ctrl <- function(train, formula, ctrl, method = 'class') {
    list_fit <- list()
    for (i in seq(1, length(ctrl))) {
      list_fit[[i]] <- rpart(as.formula(formula), 
                             data = train, 
                             control = ctrl[[i]],
                             method = method)
    }
    return(list_fit)
  }
  
  
  rpart_prob <- function(list_fit, newdata) {
    # list_fit: lista retornada por rpart_fit_ctrl || rpart_fit_formulas
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
  
  
  # Plot de lista de arboles
  rpart_plot_fit <- function(list_tree) {
    for (i in seq(1, length(list_tree))) {
      rpart.plot(list_tree[[i]], roundint = FALSE)
    }
  }
  
  # Prediccion y error de clasificacion con rpart
  rpart_pred_err <- function(list_fit, newdata, y) {
    test_pred <- list()
    test_err <- rep(0, length(list_fit))
    for (i in seq(1, length(list_fit))) {
      # prediccion
      test_pred[[i]] <- predict(list_fit[[i]], newdata = newdata, type = 'class')
      # error de clasificacion
      test_err[i] <- fn_err(test_pred[[i]], newdata[[y]])
    }
    list(pred = test_pred, err = test_err)
  }
  
  
  # Prediccion y error de cv con rpart
  # Error de CV
  rpart_cv_err <- function(cv_part, formula, ctrl, umbral = 0.5, var_y) {
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
      list_fit <- rpart_fit_ctrl(cv_train[[k]], 
                                 formula = formula, 
                                 ctrl = ctrl)
      # prob
      list_prob <- rpart_prob(list_fit, 
                              newdata = cv_test[[k]])
      # pred
      list_pred <- fn_pred(list_prob, 
                           umbral = umbral) 
      # error
      cv_err[[k]] <- fn_pred_err(list_pred, 
                                 cv_test[[k]][[var_y]])
    }
    return(mean(unlist(cv_err)))
  }
  
  
  
  #Ejecuta el CV para las primeras mejores resultados
  rpartCVAutomatic <- function(firstError, cv_part, formula, ctrl, umbral, var_y)
  {
    result <- list()
    for (i in seq(nrow(firstError$dfError))) {
      h.control <- list(h1 = h.rpart_ctrl[[firstError$dfError$control[i]]])
      h.humbral <- h.umbral[firstError$dfError$umbral[i]]
      
      #cv_part, formula, ctrl, y
      h.cv_error <- rpart_cv_err(cv_part = h.cv_part, 
                                 formula = h.formula, 
                                 ctrl = h.control, 
                                 umbral = h.humbral,
                                 var_y = 'Churn') 
      
      
      result[[i]] <- c(mensaje = paste(paste('Error H', i, " - "), 
                                       'umbral:', h.humbral, 
                                       'test:', firstError$dfError$error[i], 
                                       'cv:', h.cv_error),
                       errorTest = firstError$dfError$error[i],
                       errorCV = h.cv_error
      )
    }
    
    return(result) 
  }
  
  
  ##################################################################################################
  #####################        Regresion Logistica GLM             #################################
  ##################################################################################################
  glm_fit_formulas <- function(train, formulas) {
    list_fit <- list()
    for (i in seq(1, length(formulas))) {
      list_fit[[i]] <- glm(as.formula(formulas[i]), data = train, family = 'binomial')
    }
    return(list_fit)
  }
  
  
  
  # Prediccion y matriz de confusión  
  glm_pred_err_mc <- function(list_fit, newdata, y, umbral=0.5) {
    test_prob <- list()
    test_pred <- list()
    test_err  <- list ()
    test_mc <- list()
    for (i in seq(1, length(list_fit))) {
      # probabilidad
      test_prob[[i]] <- predict(list_fit[[i]], newdata = newdata, type = 'response') #para que de la probabilidad
      # prediccion 
      test_err[[i]] <- rep(0, length(umbral))
      test_pred[[i]] <- list ()
      test_mc[[i]] <- list ()
      for (j in seq(1, length(umbral))) {
        # prediccion 
        test_pred[[i]][[j]] <- ifelse(test_prob[[i]] > umbral[j],1,0)
        # error 
        test_err[[i]][j] <- fn_err(test_pred[[i]][[j]], newdata[[y]])
        # mc 
        test_mc[[i]][[j]] <- table(test_pred[[i]][[j]],newdata[[y]])
      }
    }
    list(prob = test_prob, pred = test_pred, err = test_err, mc = test_mc)
  }
  
  # Prediccion y error CV
  cv_err <- function(cv_part, formulas, y) {
    cv_test <- cv_part$test
    cv_train <- cv_part$train
    cv_matrix_err <- matrix(0, nrow = cv_part$k_folds, ncol = length(formulas))
    for (k in seq(1, cv_part$k_folds)) {
      list_fit <- glm_fit_formulas(cv_train[[k]], formulas = formulas)
      list_pred_err <- glm_pred_err(list_fit, newdata = cv_test[[k]], y = y)
      cv_matrix_err[k, ] <- list_pred_err$err  
    }
    apply(cv_matrix_err, 2, mean)
  }
  
  
  
  ##################################################################################################
  #########################################        KNN             #################################
  ##################################################################################################
  # Prediccion 
  knn_pred <- function(k, train, test, y)
  {
    h.listPred <- list()
    for (i in seq(k)) {
      h.listPred[[i]] <- knn(k = 1,
                               train = train,
                               test = test,
                               cl = y)
    }
    return(h.listPred)
  }
  
  
  # Error de KNN
  knn_pred_err <- function (list_pred, newdata, y, k) 
  {
    test_err <- rep (0, length(k))
    for (i in seq(k)) {
      test_err[i] <- fn_err(list_pred[[i]], newdata[[y]])
    }
    list(err = test_err, k = k)
  }
  
  
  # CV en KNN
  knn_cv_err <- function(cv_part, cl, vars, k) {
    cv_err <- rep(0, cv_part$k_folds)
    cv_list_erro <- list()
    for (i in seq(k))
    {
      for (s in seq(1, cv_part$k_folds)) {
        cv_train <- cv_part$train[[s]][, vars]
        cv_test <- cv_part$test[[s]][, vars]
        cv_knn_class <- cv_part$train[[s]][[cl]]
        cv_pred <- knn(cv_train , cv_test , cl = cv_knn_class , k = i)
        cv_err[s] <- fn_err(cv_pred, cv_part$test[[s]][[cl]])
      }
      cv_list_erro[[i]] <- mean(cv_err)
    }
    return(cv_list_erro)
  }
  
  
