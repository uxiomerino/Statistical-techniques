###### REGRESIÓN XERALIZADA E MODELOS MIXTOS

# Borja Souto, Xoel Montes e Uxío Merino

#---------------------------------------------------------#
# Simulación de datos multinivel con variable explicativa #
#---------------------------------------------------------#

simulacions <- function(N, J, mu, beta, sigmau, sigmav, sigmae, B, pendiente_aleatoria = TRUE){
  
  resultados <- matrix(NA, ncol=5, nrow=B)
  
  for (b in 1:B) {
    
    u0 <- rnorm(J, sd = sigmau)
    u1 <- rnorm(J, sd = sigmav)
    eps <- rnorm(N, sd = sigmae)
    
    grupo <- sample(1:J, N, rep=TRUE) # Asignación de individuos a grupos
    
    X <- rnorm(N, mean=0, sd=2) # Variable explicativa continua
    
    # Resposta
    ifelse(pendiente_aleatoria, Y <- mu + beta * X + u0[grupo] + u1[grupo] * X + eps, 
           Y <- mu + beta * X + u0[grupo] + eps)
   
    library(nlme)
    datos <- data.frame(Y = Y, X = X, grupo = factor(grupo))
    datos_grupos <- groupedData(Y ~ X | grupo, data = as.data.frame(datos))
    ifelse(pendiente_aleatoria, 
           modelo <- lme(Y ~ X, random = ~ X | grupo, data = datos_grupos), # Modelo con intercept e pendente aleatoria
           modelo <- lme(Y ~ X, random = ~ 1 | grupo, data = datos_grupos)) # Modelo só con intercept aleatorio
    
    # Gardar estimadores
    resultados[b, 1] <- modelo$coefficients$fixed["(Intercept)"] # Intercepto
    resultados[b, 2] <- modelo$coefficients$fixed["X"] # Beta
    resultados[b, 3] <- modelo$sigma^2 # Varianza residual
    resultados[b, 4] <- (as.numeric(VarCorr(modelo)[, "StdDev"])[1])^2
    if(pendiente_aleatoria == T) resultados[b, 5] <- (as.numeric(VarCorr(modelo)[, "StdDev"])[2])^2
  }
  
  #-----------------------#
  # Análise de resultados #
  #-----------------------#
  
  # Sesgo e ECM
  estimadores <- data.frame(
    Beta0 = resultados[,1],
    Beta1 = resultados[,2],
    VarE = resultados[,3], 
    VarU = resultados[,4]
  )
  

  
  # Contrastes
  library(nortest)
  print(shapiro.test(estimadores$Beta0))
  print(shapiro.test(estimadores$Beta1))
  print(shapiro.test(estimadores$VarE))
  print(shapiro.test(estimadores$VarU))
  
  # Representación gráfica
  par(mfrow=c(2,3))
  hist(estimadores$Beta0, main="Distribución do Intercepto", col = "skyblue", xlab="Valor estimado")
  abline(v = mu, col = "red", lwd = 2, lty = 2) # Verdadeiro valor
  hist(estimadores$Beta1, main="Distribución de Beta", col = "lightgreen", xlab="Valor estimado")
  abline(v = beta, col = "red", lwd = 2, lty = 2) # Verdadeiro valor
  hist(estimadores$VarE, main="Distribución da Varianza Residual", col = "lightpink", xlab="Valor estimado")
  abline(v = sigmae^2, col = "red", lwd = 2, lty = 2) # Verdadeiro valor
  hist(estimadores$VarU, main = "Distribución da Varianza Do Intercept Aleatorio", col = "honeydew", xlab = "Valor estimado")
  abline(v = sigmau^2, col = "red", lwd = 2, lty = 2) # Verdadeiro valor
  
  # Calcular métricas
  true_values <- c(mu, beta, sigmae^2, sigmau^2)
  
  
  if (pendiente_aleatoria == T) {
    estimadores$VarV <- resultados[, 5]
    hist(estimadores$VarV, main = "Distribución da Varianza Da Pendente Aleatoria", col = "peachpuff", xlab = "Valor estimado")
    abline(v = sigmav^2, col = "red", lwd = 2, lty = 2) # Verdadeiro valor
    true_values <- c(true_values, sigmav^2)
    print(shapiro.test(estimadores$VarV))
  }
  
  sesgos <- colMeans(estimadores) - true_values
  ecm <- colMeans((estimadores - true_values)^2)
  
  return (list(Sesgos = sesgos, ECM = ecm))
}

# Parámetros iniciais
N <- 100       # Número de individuos
J <- 20        # Número de grupos
mu <- 0        # Intercept
beta <- 2      # Coeficiente de tendencia para X
sigmau <- 1    # Varianza do intercept aleatorio
sigmav <- 1    # Varianza das pendentes aleatorias
sigmae <- 1  # Varianza do erro
B <- 500       # Número de réplicas

set.seed(123456) 

sim1 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B)
sim1
# Os estimadores son insesgados, pero algúns presenta un elevado ECM debido á súa alta
# varianza. Ademáis, os estimadores non seguen unha distribución normal, con pvalores
# moi baixos para as var dos efectos aleatorios e cercanos a 0.05 para a parte fixa (salvo Beta0)


## Podemos probar a variar algúns params do modelo e comprobar o seu efecto

# Variando o numero de individuos e de grupos
N <- 1000       # n -> Inf
                # Mesmos grupos

sim2 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B) 
# Aumentando o número de individuos, mellora a normalidade dos estimadores da parte fixa (os pvalores 
# toman cantidades moi elevadas) pero non das varianzas dos efectos aleatorios
# Comparamos os sesgos e o ECM coa simulación anterior
sim1$Sesgos
sim2$Sesgos

sim1$ECM
sim2$ECM
# Este aumento do número de individuos (mantendo o número de grupos) non se reflicte nun
# descenso do ECM, e os estimadores continúan a ser insesgados

# Modificamos as varianzas, deixando o resto de parámetros:
# N <- 1000 
# J <- 20       
# mu <- 0   
# beta <- 2
sigmau <- 2
sigmav <- 0.2
sigmae <- 0.2

sim3 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B)
# Os estimadores seguen a ser insesgados, pero os ECM dos estimadores crecen considerablemente
# ao aumentar tamén a varianza global. Non observamos ningún efecto concreto debido aos cambios
# de cada varianza específicamente
sim3

# Ao introducir un valor de sigmav tan baixo, poderíamos probar cun modelo máis sinxelo, 
# sen pendente aleatoria e só co intercept. Isto pode facerse coa nosa función, co parámetro
# pendiente_aleatoria = F. Para realizar comparacións entre dous modelos, utilizaríamos
# un ANOVA, pero ao estar realizando 500 réplicas temos 500 modelos de cada tipo.

sim4 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B, pendiente_aleatoria = F)
sim4
# Vemos como utilizando un modelo sixelo os valores do sesgo e ECM non empeoran 
# (de feito, para algún estimador mellora, por exemplo a varianza do intercept aleatorio),
# polo que preferimos o modelo máis sinxelo neste escenario.


# Probamos a variar o número de grupos:
J <- 4
sim5 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B, pendiente_aleatoria = F)

sim5
sim4
# Ao reducir o número de grupos mantendo os mesmos params que na simulación anterior, 
# observamos un claro aumento da varianza do intercept aleatorio

sim6 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B)
sim6
sim3

# Aínda utilizando o modelo con pendentes aleatorias, o aumento na varianza do intercept
# segue sendo moi notable ao reducir o número de grupos de 20 a tan só 4

# Probamos o caso extremo, N = J
J <- 1000
sim7 <- simulacions(N, J, mu, beta, sigmau, sigmav, sigmae, B, pendiente_aleatoria = F)
sim7
# Pese a existir grupos sen datos, o modelo RANOVA é capaz de realizar o axuste e predicións
# sobre eles. Ao contar cuha parte fixa, produce estimacións para todos os grupos indepentemente
# do seu tamaño e pode extrapolarse a grupos non observados, ao contrario que un modelo
# clásico. Estes modelos clásicos que introducen os grupos como variable explicativa categórica,
# necesitan estimar un beta para cada grupo, o cal sería imposible se hai grupos sen datos.



