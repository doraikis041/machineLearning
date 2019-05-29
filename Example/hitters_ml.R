set.seed(113)

source('utils.R')

# Cargar datos

raw_data <- read.csv('Hitters.csv')

# Eliminar NA

df <- na.omit(raw_data)

# Transformar Salary en log(Salary)

df$LogSalary <- log(df$Salary)

# Particion train - test

h.part <- partition_train_test(df, ntrain = 200)
h.train <- h.part$train
h.test <- h.part$test

# Particion para cross validation con k_folds = 5

h.k_folds <- 5
h.cv_part <- partition_cv(h.train, k_folds = h.k_folds)
h.cv_train <- h.cv_part$train
h.cv_test <- h.cv_part$test

# Formulas

h.formulas <- c('LogSalary ~ AtBat', 
                'LogSalary ~ AtBat + Hits', 
                'LogSalary ~ AtBat + Hits + HmRun',
                'LogSalary ~ AtBat + Hits + HmRun + Runs',
                'LogSalary ~ CAtBat',
                'LogSalary ~ CAtBat + CHits', 
                'LogSalary ~ CAtBat + CHits + CHmRun',
                'LogSalary ~ CAtBat + CHits + CHmRun + CRuns')

# Regresion

h.lm_fit <- glm_fit_formulas(h.train, h.formulas)

# MSE en test

h.lm_test_pred_err <- glm_pred_err(h.lm_fit, newdata = h.test, y = 'LogSalary')
h.lm_test_pred <- h.lm_test_pred_err$pred
h.lm_test_err <- h.lm_test_pred_err$err

# MSE en train

h.lm_train_pred_err <- glm_pred_err(h.lm_fit, newdata = h.train, y = 'LogSalary')
h.lm_train_pred <- h.lm_train_pred_err$pred
h.lm_train_err <- h.lm_train_pred_err$err

# CV error

h.lm_cv_err <- cv_err(h.cv_part, h.formulas, 'LogSalary')

# Plot

plot(h.lm_test_err, type = 'l', col = 'red', ylim = c(0.4, 0.7),
     main = 'Error de prediccion LM', ylab = 'MSE LogSalary', xlab = 'Hipotesis', xaxt = 'n')
lines(h.lm_train_err, col = 'blue')
lines(h.lm_cv_err, col = 'magenta')
axis(1, at = seq(1, length(h.formulas)))
legend("topright", legend = c('train', 'test', 'cv'), 
       col = c('blue', 'red', 'magenta'), lty = c(1, 1, 1))

