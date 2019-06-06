
source("Utility/head.R")
source("Utility/funciones.R")

#Genero la grilla de visualización
h.gridDF <- dfGridMetada()

#Se carga los datos con las ETL generales
h.data <- loadData()

#Generar la partición de test & train
h.part <- partition_train_test(h.data, ntrain = nTrain)
h.train <- h.part$train
h.test <- h.part$test

#formulas (Revisar si mantener...) ojooooooooo
#h.formula <- c('as.factor(y) ~ x1')

h.fit <- naiveBayes(as.factor(Churn) ~ ., data = h.train)

h.test$yhat <- predict(h.fit, newdata = h.test, type = 'class')


### Plot de la prediccion sobre el dataset T0
plot(x2 ~ x1, h.T0, xlim = c(h.min, h.max), col = h.col(yhat), 
     main = "Plot de la prediccion de NaiveBayes para T0")  # plot de los puntos de T0
lines(h.grid, h.f(h.grid), col = 'black', lwd = 2)    # funcion f

print(bayes_error(h.f, h.T0))
print(fn_err(h.T0$yhat, h.T0$y))

#Mostrar donde estan los errores con puntos cyan
plot(x2 ~ x1, h.T0, xlim = c(h.min, h.max),col = h.col(yhat),
     main = "Plot de la prediccion de NaiveBayes para T0")  # plot de los puntos de T0
lines(h.grid, h.f(h.grid), col = 'black', lwd = 2)    # funcion f

#Mostrar donde estan los errores con puntos cyan
h.new_color <- function(yhat,y) ifelse(yhat== y, 'black', 'cyan')
plot(x2 ~ x1, h.T0, xlim = c(h.min, h.max), col = h.new_color(yhat,y), 
     main = "Plot de la prediccion de NaiveBayes para T0")  # plot de los puntos de T0
lines(h.grid, h.f(h.grid), col = 'black', lwd = 2)    # funcion f


#Mostrar donde estan los errores con puntos cyan
h.pch <- function(yhat,y) ifelse(yhat== y,1,19) # Utilizar la funcion points para ver los tipos de puntos
plot(x2 ~ x1, h.T0, xlim = c(h.min, h.max), 
     col = h.new_color(yhat,y), pch = h.pch(yhat,y),
     main = "Plot de la prediccion de NaiveBayes para T0")  # plot de los puntos de T0
lines(h.grid, h.f(h.grid), col = 'black', lwd = 2)    # funcion f