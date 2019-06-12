


source("Utility/functions.R")


# Cargar datos

raw_data <- loadData()

h.data <- partition_train_test_numeric (raw_data, nTrain)
h.train <- h.data$train
h.test <- h.data$test

h.formulas <- c("as.factor(Churn) ~ .")


# Entreno, predigo y calculo error 
h.glm_fit <- glm_fit_formulas_cla (h.train, h.formulas)
h.glm_pred_err_cla <- glm_pred_err_cla(h.glm_fit, newdata = h.test, y = "Churn") 
h.glm_pred_err_mc <- glm_pred_err_mc (h.glm_fit, newdata = h.test, y = "Churn") 
  





# Calculo de Cross Validation

error_train_rlCV <- vector(length = 5)

h.cv_folds <- split(train, seq(1,5))
h.cv_err_logit <- rep(0, 2)
h.cv_test <- list()
h.cv_train <- list()

for (k in seq(1, 5)) 
{
  h.cv_test[[k]] <- h.cv_folds[[k]]
  h.cv_train[[k]] <- data.frame()
  
  for (i in seq(1, 5)) {
    if (i != k) h.cv_train[[k]] <- rbind(h.cv_train[[k]], h.cv_folds[[i]])
  }
}




for (k in seq(1, 5))
{
 h.fitRLCV <- glm(Churn ~ . , data = h.cv_train[[k]], family = binomial)
 h.cv_test[[k]]$pred_CVrl <- predict(h.fitRLCV, newdata = h.cv_test[[k]], type = 'response')
 h.cv_test[[k]]$yhat_CVrl <- ifelse(h.cv_test[[k]]$pred_CVrl > 0.5, 1, 0)
 error_train_rlCV[k] <- fn_error(h.cv_test[[k]]$yhat_CVrl,h.cv_test[[k]]$Churn)
}

print(mean(error_train_rlCV))




