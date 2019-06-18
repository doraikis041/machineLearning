source('utils_gen.R')
source('utils_plot.R')
source('utils_gbm.R')

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

h.gbm_formula <- as.formula('class ~ .')

# Funcion de error

fn_err <- fn_err_cost # fn_err_cla

# Random Forest

# Meta parametros de random forest
h.gbm_ctrl <- list(
  ctrl1 = list(ntree = 100, depth = 1, shrinkage = 0.1),
  ctrl2 = list(ntree = 200, depth = 1, shrinkage = 0.01),
  ctrl3 = list(ntree = 300, depth = 1, shrinkage = 0.001),
  ctrl4 = list(ntree = 100, depth = 2, shrinkage = 0.1),
  ctrl5 = list(ntree = 200, depth = 2, shrinkage = 0.01),
  ctrl6 = list(ntree = 300, depth = 2, shrinkage = 0.001)
)

# Train random forest
h.gbm_fit <- gbm_fit_ctrl(h.train, h.gbm_formula, ctrl = h.gbm_ctrl)

# Probabilidad en test
h.gbm_test_prob <- gbm_prob(h.gbm_fit, newdata = h.test, ctrl = h.gbm_ctrl)

# Predicciones en test para un vector de umbrales
h.umbral <- seq(0.1, to = 0.9, by = 0.1)
h.gbm_test_pred <- gbm_pred(h.gbm_test_prob, h.umbral)

# Error en test
h.gbm_test_pred_err <- gbm_pred_err(h.gbm_test_pred, h.test$class)

# Plot error
plot_umbral_err(h.gbm_test_pred_err, main = 'Error de GBM', umbral = h.umbral)

# CV error
# Hipotesis 1 con umbral 0.5
h.gbm_test_err_1 <- h.gbm_test_pred_err[[1]][5]
h.gbm_cv_ctrl_1 <- list(h1 = h.gbm_ctrl[[1]])
h.gbm_cv_umbral_1 <- h.umbral[5]
h.gbm_cv_err_1 <- gbm_cv_err(h.cv_part, 
                           formula = h.gbm_formula, 
                           ctrl = h.gbm_cv_ctrl_1, 
                           umbral = h.gbm_cv_umbral_1,
                           var_y = 'class') 
print(paste('Error h1 -', 
            'umbral:', h.gbm_cv_umbral_1, 
            'test:', h.gbm_test_err_1, 
            'cv:', h.gbm_cv_err_1))

# Hipotesis 4 con umbral 0.3
h.gbm_test_err_4 <- h.gbm_test_pred_err[[4]][3]
h.gbm_cv_ctrl_4 <- list(h2 = h.gbm_ctrl[[4]])
h.gbm_cv_umbral_4 <- h.umbral[3]
h.gbm_cv_err_4 <- gbm_cv_err(h.cv_part, 
                           formula = h.gbm_formula, 
                           ctrl = h.gbm_cv_ctrl_4, 
                           umbral = h.gbm_cv_umbral_4,
                           var_y = 'class')
print(paste('Error h4 -', 
            'umbral:', h.gbm_cv_umbral_4, 
            'test:', h.gbm_test_err_4, 
            'cv:', h.gbm_cv_err_4))