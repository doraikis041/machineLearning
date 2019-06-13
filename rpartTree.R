source("Utility/functions.R")

#Umbral para la ejecución de la matriz de confusión
umbral <- seq(0.3, to = 0.7, by = 0.1)

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

# cp mientras mas chico es cp mas complejo es
h.ctrl <- list(ctrl1 = rpart.control(minsplit = 1, maxdepth = 5, cp = 0.0001)
              ,ctrl2 = rpart.control(minsplit = 1, maxdepth = 5, cp = 0.001)
              ,ctrl3 = rpart.control(minsplit = 1, maxdepth = 5, cp = 0.1)
)
parms = list(split = "gini")
#a <-rpart(as.factor(Churn)~.,data = h.train,control = h.ctrl[[1]],parms = parms )

# Clasificacion con rpart
h.tree_fit <- rpart_fit_ctrl(h.train, h.formula, h.ctrl, method = 'class', parms)

# Plot de los arboles
#rpart_plot_fit(h.tree_fit)


# Error en test
h.rpart_test_pred_err <- rpart_pred_err(h.tree_fit, newdata = h.test, y = 'Churn')
h.rpart_test_pred <- h.rpart_test_pred_err$pred
h.rpart_test_err <- h.rpart_test_pred_err$err
# 
# Error en train
h.rpart_train_pred_err <- rpart_pred_err(h.tree_fit, newdata = h.train, y = 'Churn')
h.rpart_train_pred <- h.rpart_train_pred_err$pred
h.rpart_train_err <- h.rpart_train_pred_err$err

 
h.rpart_cv_err <- rpart_cv_err(h.cv_part, h.formula, h.ctrl, y = 'Churn')



h.rpart_mc_err <- rpart_pred_err_mc(h.tree_fit, newdata = h.train, y = 'Churn',type = 'class',umbral = umbral)

# Plot error
plot(h.rpart_test_err, type = 'l', col = 'red', ylim = c(0., 0.3),
     main = 'Error de prediccion Rpart',
     ylab = 'Error de clasificacion',
     xlab = 'Hipotesis', xaxt = 'n')
lines(h.rpart_train_err, col = 'blue')
lines(h.rpart_cv_err, col = 'magenta')
axis(1, at = seq(1, length(h.ctrl)))
# legend("topright", legend = c('train', 'test', 'cv'),
#        col = c('blue', 'red', 'magenta'), lty = c(1, 1, 1))
