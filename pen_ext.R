penalizacion_exterior <- function(f, g = NULL, h = NULL, x_inicial = c(0, 0), rho_inicial = 0.1, gamma = 2, epsilon = 0.01, max_iter = 100) {
  x_t <- x_inicial
  rho <- rho_inicial
  t <- 1
  
  # Inicializar o data frame
  iteracions <- data.frame(
    x1 = numeric(),
    x2 = numeric(),
    rho = numeric(),
    f_valor = numeric(),
    violacion = numeric(),
    iteracions = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (t in 1:max_iter) {
    # Definimos a función penalizada
    f_penalizada <- function(x) {
      f(x) + rho * (P_g(x, g) + P_h(x, h))
    }
    
    # Resolución do problema minimizando a función penalizada
    x_t1 <- optim(par = x_t, fn = f_penalizada, method = "BFGS")$par
    
    # Calcular valores para almacenar
    f_valor <- f(x_t1)
    violacion <- P_g(x_t1, g) + P_h(x_t1, h)
    
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
    if (violacion < epsilon) return(iteracions)
    
    # Actualización de rho e x_t
    rho <- gamma * rho
    x_t <- x_t1
  }
  
  return(iteracions)
}

## Penalizacións
P_g <- function(x, g) {
  if (is.null(g)) return(0)
  sum(sapply(g, function(g_i) max(0, g_i(x))^2))
}
P_h <- function(x, h) {
  if (is.null(h)) return(0)
  sum(sapply(h, function(h_i) h_i(x)^2))
}


## Exemplo de uso, cos valores 
# Función obxectivo
f <- function(x) (x[1]-4)^2 + (x[2]-3)^2

# Restricións de desigualdade (g(x) ≤ 0)
g1 <- function(x) x[1]^2 + x[2] - 4
g2 <- function(x) x[1] - 2 * x[2] + 3
g <- list(g1, g2)

# Restricións de igualdade (h(x) = 0)
h1 <- function(x) x[1]^2 - x[2] + 1
h2 <- function(x) x[2] - 2
h <- list(h1, h2)

# Punto inicial
x_inicial <- c(-5, -5)

## Probas con varios valores:
valores_rho <- c(0.1, 1, 10, 100, 1000, 10000)
valores_gamma <- c(2, 8, 10, 100)

## Inicializar o data frame para almacenar resultados
resultados <- data.frame(rho = numeric(),
                         gamma = numeric(),
                         resultado_x = numeric(),
                         resultado_y = numeric(),
                         iteracions = integer(),
                         stringsAsFactors = FALSE)

## Iterar sobre as combinacións de rho e gamma
for (rho_inicial in valores_rho) {
  for (gamma in valores_gamma) {
    result <- penalizacion_exterior(f, g, h, x_inicial, rho_inicial = rho_inicial, gamma = gamma)
    ultima_it <- tail(result, 1)
    resultados <- rbind(resultados, 
                        data.frame(rho = rho_inicial, 
                                   gamma = gamma, 
                                   resultado_x = ultima_it$x1, 
                                   resultado_y = ultima_it$x2, 
                                   iteracions = ultima_it$iteracions))
  }
}


print(resultados)

## Iteración concreta
resultado <- penalizacion_exterior(f, g, h, x_inicial, rho_inicial = 1, gamma = 2)
punto <- cbind(tail(resultado$x1, 1), tail(resultado$x2, 1))
cat("Solución final:", punto, "\n")


# Representación da sucesión de converxencia
x1_vals <- seq(-5, 2, length.out = 200)
x2_vals <- seq(-5, 3, length.out = 200)

x1_grid <- outer(x1_vals, x2_vals, function(x1, x2) x1)
x2_grid <- outer(x1_vals, x2_vals, function(x1, x2) x2)

# Valores das restricións de desigualdade
g1_vals <- x1_grid^2 + x2_grid - 4
g2_vals <- x1_grid - 2 * x2_grid + 3

# Valores das restricións de igualdade
h1_vals <- x1_grid^2 - x2_grid + 1
h2_vals <- x2_grid - 2

# Valores da función obxectivo
f_vals <- (x1_grid - 4)^2 + (x2_grid - 3)^2

# Debuxar as restricións na gráfica
contour(x1_vals, x2_vals, g1_vals, levels = 0, drawlabels = FALSE, 
        col = "red", lty = 2, lwd = 1, xlab = "X1", ylab = "X2", 
        main = "Sucesión de puntos")
contour(x1_vals, x2_vals, g2_vals, levels = 0, drawlabels = FALSE, 
        col = "red", lty = 2, lwd = 1, add = TRUE)

contour(x1_vals, x2_vals, h1_vals, levels = 0, drawlabels = FALSE, 
        col = "green", lty = 1, lwd = 2, add = TRUE)
contour(x1_vals, x2_vals, h2_vals, levels = 0, drawlabels = FALSE, 
        col = "green", lty = 1, lwd = 2, add = TRUE)

# Engadir as curvas de nivel da función obxectivo
contour(x1_vals, x2_vals, f_vals, levels = pretty(f_vals, n = 10), 
        drawlabels = FALSE, col = "blue", lty = 3, lwd = 1, add = TRUE)

# Traxectoria da sucesión
x <- c(x_inicial[1], resultado$x1)
y <- c(x_inicial[2], resultado$x2)

lines(x, y, type = "o", col = "purple", pch = 16, lwd = 2)
