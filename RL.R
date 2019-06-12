

source("Utility/head.R")
source("Utility/funciones.R")


# Cargar datos

raw_data <- loadData()


df <- raw_data

str(df)


# Eliminar variables de df CustomerID por ser Unica y ServiceArea por ser factor con demasiados niveles


df$CustomerID <- NULL
df$ServiceArea <- NULL


# Eliminar filas con NA 

df <- na.omit(df)


# particion train 80% -test 20%

ntrain <- 37445
idx <- sample.int(nrow(df),ntrain)
train <- df[idx,]
test <- df[-idx,]

test$Churn <- ifelse(test$Churn == "Yes", 1, 0)
train$Churn <- ifelse(train$Churn == "Yes", 1, 0)


# Entreno y predigo con regresion logistica 


h.fit_rl <- glm(Churn ~ . , data = train, family = binomial)
test$pred_rl <- predict(h.fit_rl, newdata = test, type = 'response') 
test$yhat_rl <- ifelse(test$pred_rl > 0.5, 1, 0) 

# Calculo de error

fn_error <- function(yhat, y){mean(ifelse(yhat == y, 0, 1))} 
h.error_rl <- fn_error(test$yhat_rl,test$Churn)
h.error_rl

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




