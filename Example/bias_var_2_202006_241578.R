##################################################################
# El objetivo de este script R es estudiar la descomposicion del
# error de un algoritmo de aprendizaje. Para eso vamos a plantear
# un problema de regresion, generar datasets aleatorios y usar 
# regresion lineal como algoritmo de aprendizaje
##################################################################

##################################################################
## Funciones auxiliares
##################################################################

### Generar un dataset random de la form y = f(x) + eps
dataset <- function(f, min = 0, max = 1, size = 100, sd = 1) {
  x <- runif(size, min = min, max = max)
  eps <- rnorm(size,sd)
  y <- f(x) + eps
  data.frame(x, y)
}

### Generar una hipotesis de la forma y ~ a0 + a1 x + ... + ak x^k
poly_formula <- function(k) {
  if (k == 0) {
    as.formula("y ~ 1")
  } else {
    as.formula(paste("y ~ poly(x, degree = ", paste(k, ")")))
  }
}

### Definimos bias (sesgo) y el error considerado MSE (Minimum Square Error)
### y_hat es el estimador, y es el valor verdadero
fn_bias <- function(y_hat, y) { (mean(y_hat) - y)^2 }
fn_mse <- function(y_hat, y) { mean((y_hat - y)^2) }

##################################################################
# En esta primera parte vamos a ver que pasa cuando aplicamos un
# algoritmo al mismo dataset variando el conjunto de hipotesis H
##################################################################

set.seed(117)

### Fijamos el algoritmo (regresion lineal)
h.A <- glm
h.A_predict <- predict.glm

### Valores minimos y maximos de x
h.min <- 0
h.max <- 10

### Grilla para visualizacion
h.grid <- seq(h.min, h.max, by = 0.01)
h.grid_df <- data.frame(x = h.grid)

### Funcion f
h.f <- function(x) x*sin(1.5*x)

### Cantidad de elementos del dataset
h.size <- 200

### Generar valor: Ejecutamos el script para cada h.sd obteniendo la graficas para cada valor
h.sd <- 0.5
#h.sd <- 2
#h.sd <- 5


### Generar el dataset
h.df <- dataset(h.f, min = h.min, max = h.max, size = h.size, sd=h.sd)


### H esta definido por el grado de un polinomio (complejidad)
h.max_complexity <- 20

### Vector de colores para visualizacion
h.col <- rainbow(h.max_complexity)


### HS es una lista de conjuntos de hipotesis H_k, k = 1 ... max_complexity
### H_k esta definido por el grado k del polinomio (complejidad)
h.HS <- list()
for (k in seq(h.max_complexity)) {
  h.HS[[k]] = poly_formula(k-1)
}

### Aplicamos el algoritmo de aprendizaje A a cada conjunto H_k con el dataset df
### fit es la lista de hipotesis h_k retornada por A(H_k, df)
h.fit <- list()
for (k in seq(h.max_complexity)) {
  h.fit[[k]] = h.A(h.HS[[k]], data = h.df)
}

### Prediccion en la grilla (solo para visualizacion)
h.predgrid <- list()
for (k in seq(h.max_complexity)) {
  h.predgrid[[k]] <- h.A_predict(h.fit[[k]], newdata = h.grid_df)
}

### Plot de las hipotesis aprendidas
plot(y ~ x, h.df, xlim = c(h.min, h.max),main = paste("Hipotesis Aprendida con SD = ",h.sd))# plot de los puntos de df
abline(0, 0, lty = 3)                                 # eje x punteado
lines(h.grid, h.f(h.grid), col = 'black', lwd = 2)    # funcion f
for (k in seq(h.max_complexity)) {
  lines(h.grid, h.predgrid[[k]], col = h.col[k])      # hipotesis h_k evaluada en la grilla
}

 ##################################################################
 # Ahora estudiemos la descomposicion del error en sesgo y varianza
 # en un punto aleatorio (x0, y0) generado siguiendo la misma
 # distribucion que el (los) dataset(s)
 ##################################################################

 # Cantidad de datasets T_1 ... T_sims generados aleatoriamente
 h.sims <- 100
 
 #Punto (x0, y0)
  h.x0 <- runif(1, min = h.min, max = h.max)
  h.f0 <- h.f(h.x0)
  h.y0 <- h.f0 + rnorm(1,sd=h.sd)
  h.x0_df <- data.frame(x = h.x0)

 # # Generar T_1 ... T_sims de h.size elementos cada uno
   h.T <- list()
   for (s in seq(h.sims)) {
     h.T[[s]] <- dataset(h.f, min = h.min, max = h.max, size = h.size,sd=h.sd)
   }

  ### Para cada conjunto de hipotesis H_k ejecutar el algoritmo
  ### A en cada dataset T_s en { T_1 ... T_sims }, es decir: A(H_k, T_s)
  ### y evaluar la hipotesis resultante en x0
  h.T_fit <- list()   # T_fit[k][s] es A(H_k, T_s)
  h.T_pred <- list()  # T_pred[k][s] es T_fit[k][s](x0)
  for (k in seq(h.max_complexity)) {
    h.T_fit[[k]] <- list()
    h.T_pred[[k]] <- rep(0, h.sims)
  for (s in seq(h.sims)) {
      h.T_fit[[k]][[s]] <- h.A(h.HS[[k]], data = h.T[[s]])
  h.T_pred[[k]][s] <- h.A_predict(h.T_fit[[k]][[s]], newdata = h.x0_df)
 }
  }
 
 
##################################################################
# Ahora estudiemos la descomposicion del error en sesgo y varianza
# en un dataset aleatorio (X0, Y0) generado siguiendo la misma
# distribucion que el (los) dataset(s)
##################################################################

h.size <- 70

h.X0 <- runif(h.size, min = h.min, max = h.max)
h.F0 <- h.f(h.X0)
h.Y0 <- h.F0 + rnorm(h.size)
h.X0_df <- data.frame(x = h.X0)

### Evaluar A(H_k, T_s) en X0
for (k in seq(h.max_complexity)) {
  h.T_pred[[k]] <- matrix(0, nrow = h.sims, ncol = h.size)
  for (s in seq(h.sims)) {
    h.T_pred[[k]][s, ] <- h.A_predict(h.T_fit[[k]][[s]], newdata = h.X0_df)
  }
}



### Calcular bias, var y mse para cada x0 en X0
h.bias <- matrix(0, nrow = h.max_complexity, ncol = h.size)
h.var <- matrix(0, nrow = h.max_complexity, ncol = h.size)
h.mse <- matrix(0, nrow = h.max_complexity, ncol = h.size)
for (k in seq(h.max_complexity)) {
  for (i in seq(h.size)) {
    h.bias[k,i] <- fn_bias(h.T_pred[[k]][,i], h.F0[i])
    h.var[k,i] <- var(h.T_pred[[k]][,i])
    h.mse[k,i] <- fn_mse(h.T_pred[[k]][,i], h.Y0[i])
  }
}

### Plot de la media de bias, var y mse sobre X0
plot(apply(h.mse, 1, mean), type = 'l', ylim = c(-0.1, 50),
     col = 'red', xaxt = "n", main = paste('bias-var-mse:',h.sd), xlab = "complexity", ylab = "error")
axis(1, at = seq(h.max_complexity), labels = seq(0, h.max_complexity - 1))
lines(apply(h.var, 1, mean), col = 'green')
lines(apply(h.bias, 1, mean), col = 'blue')

##################################################################
##
##    CONCLUSIONES DEL EJERICIO EN PDF 
##
##################################################################


  