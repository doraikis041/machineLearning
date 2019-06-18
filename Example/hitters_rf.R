source('utils_gen.R')
source('utils_plot.R')
source('utils_rf.R')

set.seed(117)

# Cargar datos

raw_data <- read.csv('Hitters.csv')

# Tratamiento de los datos

# Eliminar NA
h.df <- na.omit(raw_data)
# Crear una variable $class = 1 si Salary > que la media, 0 si no.
h.df$class <- ifelse(h.df$Salary>mean(h.df$Salary), 1, 0)
# Eliminar X
h.df$X <- NULL
# Eliminar Salary
h.df$Salary <- NULL

# Particion train - test

h.part <- partition_train_test(h.df, ntrain = 200)
h.train <- h.part$train
h.test <- h.part$test

# Particion para cross validation con k_folds = 5

h.k_folds <- 5
h.cv_part <- partition_cv(h.train, k_folds = h.k_folds)
h.cv_train <- h.cv_part$train
h.cv_test <- h.cv_part$test

# Formula

h.rf_formula <- as.formula('as.factor(class) ~ .')

# Funcion de error

fn_err <- fn_err_cost # fn_err_cla

# Random Forest

# Meta parametros de random forest
h.rf_ctrl <- list(
  ctrl1 = list(ntree = 100, mtry = 5),
  ctrl2 = list(ntree = 200, mtry = 5),
  ctrl3 = list(ntree = 300, mtry = 5),
  ctrl4 = list(ntree = 100, mtry = 7),
  ctrl5 = list(ntree = 200, mtry = 7),
  ctrl6 = list(ntree = 300, mtry = 7)
)

# Train random forest
h.rf_fit <- rf_fit_ctrl(h.train, h.rf_formula, h.rf_ctrl)

# Probabilidad en test
h.rf_test_prob <- rf_prob(h.rf_fit, newdata = h.test)

# Predicciones en test para un vector de umbrales
h.umbral <- seq(0.1, to = 0.9, by = 0.1)
h.rf_test_pred <- rf_pred(h.rf_test_prob, h.umbral)

# Error en test
h.rf_test_pred_err <- rf_pred_err(h.rf_test_pred, h.test$class)

# Plot error
plot_umbral_err(h.rf_test_pred_err, main = 'Error de RF', umbral = h.umbral)

# CV error
# Hipotesis 1 con umbral 0.6
h.rf_test_err_1 <- h.rf_test_pred_err[[1]][6]
h.rf_cv_ctrl_1 <- list(h1 = h.rf_ctrl[[1]])
h.rf_cv_umbral_1 <- h.umbral[6]
h.rf_cv_err_1 <- rf_cv_err(h.cv_part, 
                           formula = h.rf_formula, 
                           ctrl = h.rf_cv_ctrl_1, 
                           umbral = h.rf_cv_umbral_1,
                           var_y = 'class') 
print(paste('Error h1 -', 
            'umbral:', h.rf_cv_umbral_1, 
            'test:', h.rf_test_err_1, 
            'cv:', h.rf_cv_err_1))

# Hipotesis 2 con umbral 0.5
h.rf_test_err_2 <- h.rf_test_pred_err[[2]][5]
h.rf_cv_ctrl_2 <- list(h2 = h.rf_ctrl[[2]])
h.rf_cv_umbral_2 <- h.umbral[5]
h.rf_cv_err_2 <- rf_cv_err(h.cv_part, 
                           formula = h.rf_formula, 
                           ctrl = h.rf_cv_ctrl_2, 
                           umbral = h.rf_cv_umbral_2,
                           var_y = 'class')
print(paste('Error h2 -', 
            'umbral:', h.rf_cv_umbral_2, 
            'test:', h.rf_test_err_2, 
            'cv:', h.rf_cv_err_2))
