library(class)
set.seed(117)

### Distribucion condicional
pcond <- function(u, alpha = 0.15) { 1 / (1 + exp(-alpha * u)) }

### Clase de un punto en funcion de la probabilidad condicional
rclass <- function(u) 
  {mapply(function(i){rbinom(1,1,i)},pcond(u))
}

### KNN
### En KNN el conjunto de hipotesis H_k esta dado por el numero de vecinos k.
### La funcion knn calcula la prediccion en tu conjunto de test usando los
### datos del conjunto de train. El parametro cl es la clase (en train).
h.A <- function(k, train, test) {knn(train, test, cl = train$y, k)}


# 3.1: Defina una función “dataset” que genere datasets de un tamaño “size” con elementos de la forma (x1, x2, y), con las variables predictoras “x1” y “x2” distribuidas de manera
# uniforme en el intervalo [min, max] y la variable a predecir “y” toma valores 0 o 1
# (clasificación binaria) con una distribución P[ Y = y | X = x ] tal que P[ Y = 1 | x2 - f(x1) >0 ] es mayor a 0.5. Es decir, para un punto (x1, x2) que está estrictamente por encima
# de la curva “f”, el valor y será 1 con probabilidad mayor a 0.5. Para esto último, use la
# función “rclass” dada con el parámetro adecuado.
### Funcion para generar un dataset random 
dataset <- function(f, min = 0, max = 1, size = 100) {
  x1 <- runif(size, min = min, max = max)
  x2 <- runif(size, min = min, max = max)
  y <- rclass(x2 - f(x1))
  data.frame(x1,x2,y)
}


### Bayes classifier
bayes_classifier <- function(f, T) {ifelse(pcond(T$x2 - f(T$x1))> 0.5,1,0)}


### Bayes error .### ...
bayes_error <- function(f, T) { mean(bayes_classifier(f,T)!= T$y)}


### Loss
fn_loss <- function(yhat, y) { ifelse(yhat != y,1,0)}



### fn_err ...
fn_err <- function(yhat, y) { mean(fn_loss(yhat, y))}


# 3.4 Cree un dataset T0 de 1000 elementos usando f(x) = x + 2 sin(1.5 x)  ---- OK
h.u <- function(x) x + 2*sin(1.5*x)
h.min <- 0
h.max <- 20
h.size <- 1000
T0 <- dataset(h.u,h.min,h.max,h.size)


# Ejercicio 3.5: Haga un plot del dataset coloreando en azul los puntos con y = 1 y en rojo los restantes.
color <- function(y){ifelse(y==1,"blue","red")}
plot(x2~x1,T0, col= color(T0$y))


#3.6: Cree 20 datasets de entrenamiento T1 .. T20 de tamaño 50
# Generar ds_t dataset de tamaño h.ds_size elementos cada uno
h.ds_t <- 20
h.ds_size <- 50


h.T <- list()
for (s in seq(h.ds_t)) {
  h.T[[s]] <- dataset(h.u,h.min,h.max,h.ds_size)
}


#3.7 Respuesta: Defina la complejidad máxima igual al tamaño de los datasets de entrenamiento
# La complejidad máxima de entrenamiento es 50 que es el valor maximo del dataset. 
h.k <- 50


#3.8 Use knn como algoritmo de aprendizaje para cada uno de los datasets de entrenamiento usando T0 como test.
  h.t_predTrain <- list()
  h.t_predTest <- list()
  for (k in seq(h.k)) {
    h.t_predTrain[[k]] <- list()
    h.t_predTest[[k]] <- list()
    for (s in seq(h.ds_t)) {
      h.t_predTest[[k]][[s]] <- h.A(k,h.T[[s]], T0)
      h.t_predTrain[[k]][[s]] <- h.A(k,h.T[[s]], h.T[[s]])
    }
  }


#3.9: Calcule los errores de entrenamiento y de test para cada valor de complejidad. 
#Nota: el valor de complejidad para knn es 1/k para un vecindario de k.

  h.error_train <- list()
  h.error_test <- list()
  for (k in seq(h.k)) {
    errorsTrain <- list()
    errorsTest <- list()
    for (s in seq(h.ds_t)) {
      errorsTrain[[s]] <- fn_err(h.t_predTrain[[k]][[s]],h.T[[s]]$y)
      errorsTest[[s]] <- fn_err(h.t_predTest[[k]][[s]],T0$y)
    }
    h.error_train[[k]] <- mean(as.vector(unlist(errorsTrain)))
    h.error_test[[k]] <- mean(as.vector(unlist(errorsTest)))
  }
  
  
bayes_clasT0 <- bayes_classifier(h.u,T0)
bayes_errorT0 <- bayes_error(h.u,T0)
  
#3.10: Haga un plot comparativo de los errores de entrenamiento, de test y bayesiano.
# Observe y explique las curvas. ¿Qué valor de complejidad corresponde al mínimo del error?
ejex <- c(1:50)
ejex1 <- rev(ejex)/50
ejey_bayes_errors <- c(rep(bayes_errorT0,50))


plot(ejex1, h.error_test, type = 'l', ylim = c(0, 1),
        col = 'red', xaxt = "n", main = 'error_Tr_Te_Ba', xlab = "complexity", ylab = "error")
   axis(1,ejex1, labels = ejex1)
  lines(ejex1, h.error_train, col = 'green')
  lines(ejex1, ejey_bayes_errors, col = 'blue')

h.min_errorT <- which.min(unlist(h.error_test))
print(h.min_errorT)


  
######### CONCLUSIONES DEL EJERCICIO EN PDF ################
  