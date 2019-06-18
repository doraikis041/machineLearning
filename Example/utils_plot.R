# Plotear error para una lista de vectores de error

plot_umbral_err <- function(list_err, umbral, main = '') {
  # list_err: lista de vectores de la misma longitud
  col_vec <- rainbow(length(list_err))
  plot(list_err[[1]], 
       type = 'l', 
       col = col_vec[1], 
       main = main, 
       ylab = 'Error', 
       xlab = 'Umbral', 
       xaxt = 'n')
  axis(1, 
       at = seq(1, length(umbral)), 
       labels = umbral)
  for (i in seq(2, length(list_err))) {
    lines(list_err[[i]], col = col_vec[i])
  }
  legend("bottomleft", 
         cex = 0.5,
         legend = seq(1, length(list_err)), 
         col = col_vec,
         lty = rep(1, length(list_err)))
}
