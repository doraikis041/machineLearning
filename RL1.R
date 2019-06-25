source("Utility/functions.R")


# Cargar datos
raw_data <- loadData()

h.data <- partition_train_test_numeric (raw_data, nTrain)
h.train <- h.data$train
h.test <- h.data$test


#Formula
h.formulas <- c("as.factor(Churn) ~ .")


# Particion para cross validation
h.cv_part <- partition_cv(df = h.train)

# Entreno, predigo y calculo error 

#fit
h.glm_fit <- glm_fit_formulas_cla (h.train, h.formulas)

#prediccion
h.glm_pred_err_cla <- glm_pred_err_cla(h.glm_fit, newdata = h.test, y = "Churn")

#matrix de confunsion
h.glm_pred_err_mc <- glm_pred_err_mc (h.glm_fit, newdata = h.test, y = "Churn") 
  

#CV
h.cv_err <- cv_err(cv_part = h.cv_part
                   , formulas = h.formulas
                   , y = "Churn")


print(mean(h.cv_err))




