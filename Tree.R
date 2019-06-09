library(rpart)
library(rpart.plot)
library(dplyr)
set.seed(113)

source('Example/utils.R')
source('Example/utils_rpart.R')


# Cargar datos

raw_data <- read.csv(file = "Dataset/dataset.csv")

# Eliminar NA

df <- na.omit(raw_data)

# Crear una variable $class = 1 si Salary > que la media, 0 si no.
df$Churn <- as.numeric(df$Churn) 
df$class <- ifelse(df$Churn>mean(df$Churn), 1, 0)

# Eliminar X de df

df$CustomerID <- NULL
df$ServiceArea <- NULL

# Particion train - test

h.part <- partition_train_test(df, ntrain = 37445)
h.train <- h.part$train
h.test <- h.part$test

# Particion para cross validation con k_folds = 5

h.k_folds <- 5
h.cv_part <- partition_cv(h.train, k_folds = h.k_folds)
h.cv_train <- h.cv_part$train
h.cv_test <- h.cv_part$test

# Formula

h.formula <- c('Churn ~ .')
# 
# # cp
# 
# h.ctrl <- list(
#   ctrl1 = rpart.control(minsplit = 1, maxdepth =4 , cp = 0.0001 )
# # ctrl2 = rpart.control(minsplit = 1, maxdepth = 3 , cp = 0.001),
# # ctrl3 = rpart.control(minsplit = 1, maxdepth = 4),
# #   # ctrl4 = rpart.control(minsplit = 1, maxdepth = 5),
# #   # ctrl4 = rpart.control(minsplit = 1, maxdepth = 6)
# )
# 
# # Clasificacion con rpart
# 
# h.tree_fit <- rpart_fit_ctrl(h.train, h.formula, h.ctrl, method = 'class')
# 
# # Plot de los arboles
# 
# rpart_plot_fit(h.tree_fit)



# Build decision tree
ctrl = rpart.control(minsplit = 1 , maxdepth = 30, cp=0.000001) # Si tienen menos de 10 elementos entonces no particiona.maxdepth es la profundidad maxima del arbol
parms = list(split = "information") # Si no ponemos Information usa el metodo por defecto.


tree = rpart(Churn ~ .,  
             data = h.train, method = "class", control = ctrl, parms = parms)

#rpart.plot(tree, type = 3, extra = 0)

rpart.plot(tree)










# 
# # error en test
# 
# h.rpart_test_pred_err <- rpart_pred_err(h.tree_fit, newdata = h.test, y = 'class')
# h.rpart_test_pred <- h.rpart_test_pred_err$pred
# h.rpart_test_err <- h.rpart_test_pred_err$err
# 
# # error en train
# 
# h.rpart_train_pred_err <- rpart_pred_err(h.tree_fit, newdata = h.train, y = 'class')
# h.rpart_train_pred <- h.rpart_train_pred_err$pred
# h.rpart_train_err <- h.rpart_train_pred_err$err
# 
# # CV error
# 
# h.rpart_cv_err <- rpart_cv_err(h.cv_part, h.formula, h.ctrl, 'class')
# 
# # Plot
# 
# plot(h.rpart_test_err, type = 'l', col = 'red', ylim = c(0., 0.3),
#      main = 'Error de prediccion Rpart', 
#      ylab = 'Error de clasificacion', 
#      xlab = 'Hipotesis', xaxt = 'n')
# lines(h.rpart_train_err, col = 'blue')
# lines(h.rpart_cv_err, col = 'magenta')
# axis(1, at = seq(1, length(h.ctrl)))
# #legend("topright", legend = c('train', 'test', 'cv'), 
# #       col = c('blue', 'red', 'magenta'), lty = c(1, 1, 1))
# 
