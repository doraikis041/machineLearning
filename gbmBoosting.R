source("Utility/functions.R")

#Se carga los datos con las ETL generales
h.data <- loadData()

#Generar la particion de test & train
h.part <- partition_train_test(h.data, ntrain = nTrain)
h.train <- h.part$train
h.test <- h.part$test

# Particion para cross validation con k_folds = 5
h.cv_part <- partition_cv(df = h.train,k_folds = 5 )

# Formulas
h.formula <- 'as.factor(Churn) ~ .'

#Definir los parametros del modelo
h.ctrl <- list(list(depth=2, shrinkage =0.01,ntree = 10))

# # Clasificacion con randomforest
 boosting_fit <- gbm_fit_ctrl(train = h.train,
                              formula = h.formula,
                              ctrl = h.ctrl)
 
 
