PSLP <- function(f, grad_f, x_0, g = NULL, h = NULL, grad_g = NULL, grad_h = NULL, rho = 1000, 
                 epsilon = 0.01, max_it = 100) {
  
  x_t <- x_0
  iteracions <- data.frame(
    x1 = numeric(),
    x2 = numeric(),
    rho = numeric(),
    f_valor = numeric(),
    violacion = numeric(),
    iteracions = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (t in 1:max_it) {
    # Resolución do subproblema
    x_t1 <- LP(x_t, f, g, h, grad_f, grad_g, grad_h, rho)
    
    # Calcular valores para almacenar
    f_valor <- f(x_t1)
    violacion <- P_g(x_t1, g, grad_g) + P_h(x_t1, h, grad_h)
    
    # Engadir os valores ao data frame
    iteracions <- rbind(iteracions, data.frame(
      x1 = x_t1[1],
      x2 = x_t1[2],
      rho = rho,
      f_valor = f_valor,
      violacion = violacion,
      iteracions = t
    ))
    
    # Criterio de parada
    condicion <- sqrt(sum((x_t1 - x_t)^2)) / (sqrt(sum(x_t^2)) + epsilon)
    if (condicion <= epsilon) {
      cat("\nConverxencia acadada en", t, "iteracions\n")
      return(iteracions)
    }
    
    # Actualización de \(x_t\)
    x_t <- x_t1
  }
  return(iteracions)
}

# Penalizacións linealizadas
P_g <- function(x, g, grad_g) {
  if (is.null(g)) return(0)
  sum(sapply(seq_along(g), function(i) {
    max(0, g[[i]](x) + sum(grad_g[[i]](x) * (x - x)))^2
  }))
}

P_h <- function(x, h, grad_h) {
  if (is.null(h)) return(0)
  sum(sapply(seq_along(h), function(i) {
    (h[[i]](x) + sum(grad_h[[i]](x) * (x - x)))^2
  }))
}



##########################################################
# Subproblema LP
LP <- function(x_t, f, g, h, grad_f, grad_g, grad_h, rho){
  f_obx <- function(x){
       
       m <- length(g)
       l <- length(h)
    
       f_lineal <- f(x_t) + sum(grad_f(x_t) * (x-x_t))
       
       g_lineal <- 0
       if (!is.null(g)){
         for (i in 1:m){
           g_t <- g[[i]](x_t)
           grad_gt <- grad_g[[i]](x_t)
           g_lineal <- g_lineal + max(0,  g_t + sum(grad_gt * (x - x_t)))
         }
       }
       h_lineal <- 0
       if (!is.null(h)){
         for (j in 1:l){
           h_t <- h[[j]](x_t)
           grad_ht <- grad_h[[j]](x_t)
           h_lineal <- h_lineal + abs(h_t + sum(grad_ht * (x - x_t)))
         }
       }
       
       f_lineal + rho * (g_lineal + h_lineal)
       # rho * (g_lineal + h_lineal)
  }
  return (optim(x_t, fn = f_obx, method = "BFGS")$par)
}



######################################################
#### Exemplo de uso:

## Función obxectivo
f <- function(x) (x[1]-4)^2 + (x[2]-3)^2
grad_f <- function(x) c(2 * (x[1]-4), 2 * (x[2]-3))


## Restricións de desigualdade:

# Enumerar as restricions
g1 <- function(x) x[1]^2 + x[2] - 4
g2 <- function(x) x[1] - 2 * x[2] + 3
# Enumerar os gradientes das restricions
grad_g1 <- function(x) c(2, 1)
grad_g2 <- function(x) c(1, -2)
# Agrupar os gradientes e restricions en listas
#g <- list(g1)
#grad_g <- list(grad_g1)
g <- list(g1, g2)
grad_g <- list(grad_g1, grad_g2)


## Restricions afins:

# Enumerar as restricions
h1 <- function(x) x[1]^2 - x[2] + 1
h2 <- function(x) x[2] - 2
#Enumerar os gradientes
grad_h1 <- function(x) c(2, -1)
grad_h2 <- function(x) c(0, 1)
# Agrupar os gradientes e restricions en listas
#h <- list(h1)
#grad_h <- list(grad_h1)
h <- list(h1, h2)
grad_h <- list(grad_h1, grad_h2)

## Punto inicial
x_0 <- c(0, 0)


## Execucion coa chamada á funcion principal
rhos <- c(5, 10, 50, 100, 1000)
for (rho in rhos){
  resultado <- PSLP(f, grad_f, x_0, g, h, grad_g, grad_h, rho = rho, epsilon = 0.001)
  ultima_it <- tail(resultado, 1)
  cat("Rho =", rho, "; Resultado:", cbind(ultima_it$x1, ultima_it$x2))
}

rho <- 10000
resultado <- PSLP(f, grad_f, x_0, g, h, grad_g, grad_h, rho = rho, epsilon = 0.001)
ultima_it <- tail(resultado, 1)
cat("Rho =", rho, "; Resultado:", cbind(ultima_it$x1, ultima_it$x2))



## Representación da sucesión de convnerxencia
x1_vals <- seq(0, 1.5, length.out = 200)
x2_vals <- seq(0, 3, length.out = 200)

x1_grid <- outer(x1_vals, x2_vals, function(x1, x2) x1)
x2_grid <- outer(x1_vals, x2_vals, function(x1, x2) x2)


g1_vals <- x1_grid^2 + x2_grid - 4
g2_vals <- x1_grid - 2 * x2_grid + 3
h1_vals <- x1_grid^2 - x2_grid + 1
h2_vals <- x2_grid - 2

f_vals <- (x1_grid - 4)^2 + (x2_grid - 3)^2

contour(x1_vals, x2_vals, g1_vals, levels = 0, drawlabels = FALSE, 
        col = "red", lty = 2, lwd = 1, xlab = "X1", ylab = "X2", 
        main = "Sucesión de puntos")
contour(x1_vals, x2_vals, g2_vals, levels = 0, drawlabels = FALSE, 
        col = "red", lty = 2, lwd = 1, add = TRUE)
contour(x1_vals, x2_vals, h1_vals, levels = 0, drawlabels = FALSE, 
        col = "green", lty = 1, lwd = 2, add = TRUE)
contour(x1_vals, x2_vals, h2_vals, levels = 0, drawlabels = FALSE, 
        col = "green", lty = 1, lwd = 2, add = TRUE)
contour(x1_vals, x2_vals, f_vals, levels = pretty(f_vals, n = 10), 
        drawlabels = FALSE, col = "blue", lty = 3, lwd = 1, add = TRUE)

x <- cbind(x_0 [1], resultado$x1)
y <- cbind(x_0 [1], resultado$x2)

lines(x, y, type = "o", col = "purple", pch = 16, lwd = 2)
