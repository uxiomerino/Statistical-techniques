# RLM
# Uxio Francisco Merino Curras, Xoel Montes varela, Andrea Varela V?zquez

# Haremos uso de las siguientes librerías:

library(dplyr)    # Gestion de datos 
library(ggplot2)  # Algunos graficos
library(MASS)
library(car)
library(moments)  # Simetria y kurtosis - ver tambien library(e1071) 
library(DAAG)     # Validacion cruzada 
library(nortest)  # Pruebas GOF a una normal de Lilliefors, 
# Cramer-von Mises, Anderson Darling y Shapiro-Wilk 
#library(normtest) # Pruebas GOF a una normal basadas en momentos
library(lmtest)   # Prueba de Breusch-Pagan
library(tseries)  # Prueba de rachas para aleatoriedad

# Librerias adicionales respecto a las empleadas en RLS
# Para graficos completos de dispersion bivariante
# varias opciones ademas de la funcion pairs() de graphics:
library(psych)    
library(corrplot) 
library(GGally)
# Test de correlaciones simultaneamente para varias variables:
library("Hmisc")
# Calculo correlaciones parciales
library(ggm) 
# Contrastes e intervalos en modelos lineales
library(gmodels)  
# Para dibujar regiones de confianza 
library(ellipse)
# Para multicolinealidad
library(mctest)


#Cargamos el fichero de datos
load("D:/Users/monte/OneDrive/Documentos/GCED/2SEGUNDO_GCED/MR/MR - 2op/Datos/Datos/Datos_RLM.RData")

#Comprobamos que se ha cargado
ls()
Datos <- Basket

#Exploramos el contenido del conjunto de datos.
head(Datos)
str(Datos)
dim(Datos)
attach(Datos)

#Trabajamos con 6 variables cuantitativas
# Conviene siempre realizar un análisis descriptivo 
# unidimensional de todas las variables 
summary(Datos)
apply(Datos,2,sd)
apply(Datos,2,skewness, na.rm = FALSE)
apply(Datos,2,kurtosis, na.rm = FALSE)

# An?lisis gr?fico
#Histograma y ajuste normal

windows();par(mfrow=c(2,3))
for (i in 1:ncol(Datos) ) 
{
  
  #Histograma y ajuste normal
  hist(Datos[,i], freq=FALSE, main = "", col ="lightblue",
       xlab=names(Datos)[i], cex.lab=1.4, ylab = "Histograma")
  curve(dnorm(x,mean=mean(Datos[,i]),sd=sd(Datos[,i])),
        col="magenta", lwd=2, add=TRUE)
}

windows();par(mfrow=c(2,3))
for (i in 1:ncol(Datos) ) 
{
  
  #Diagrama de cajas
  boxplot(Datos[,i], main = "", xlab=names(Datos)[i],
          cex.lab=1.4, border = "blue", col= "lightblue", 
          pch="+", horizontal = TRUE, cex=3)
}

windows();par(mfrow=c(2,3))
for (i in 1:ncol(Datos) ) 
{
  #Ajuste no paramétrico (kernel)
  plot(density(Datos[,i]),main="",lwd=3,col="blue",
       ylab="Densidad variable puntos estimada", cex.lab=1.4)
  polygon(density(Datos[,i]), col="lightblue")
  
}


for (i in 1:ncol(Datos) ) 
{
  print(colnames(Datos)[i])
  print(shapiro.test(Datos[,i]))
  print('---------------------------------------------')
}



# Analizando las graficas podemos ver como es posible falta de normalidad en las
# variables altura, puntos, minutos, perdidas y porcentaje.
# Con la ayuda del test de normalidad, en este caso hemos usado el de shapiro debido a que
# no tenemos demasiadas observaciones (62), podemos ver como en todas las variables 
# citadas rechazamos normalidad a un 5% de significacion.
 
windows();par(mfrow=c(2,3))
for (i in 1:ncol(Datos) ) 
{
  plot(Datos[,i])
  
}
# Observando las nube de puntos no parece haber ninguna variable con heterocedasticidad


# Analisis de correlacion lineal bivariante
# Grafico matricial de dispersion bivariante sencillo
ggpairs(Datos)


# Analisis de correlacion
#An?lisis num?rico estrictamente
cor(Datos) 

#Analizamos ahora las correlaciones parciales
correlations(cov(Datos))

#CONCLUSIONES

# Las correlaciones bivariantes se ubican en la triangular inferior 
# y las parciales en la triangular superior.

# Las correlaciones mas significantes son puntos con minutos, la cual es alta y significativa
# tanto en la triangular inferior (0.868) como en la triangular superior (0.746).

# Perdidas con puntos tiene una correlacion bivariante alta (0.714) pero 
# una correlacion parcial mucho menor (0.2535), indicios de presencia de multicolinealidad.
# Esto tambien sucede con puntos y personales (0.5412 , -0,1928), 
# perdidas y minutos (0.7582 , 0.2108), personales y minutos (0.605, 0.35),
# personales y perdidas (0.560 , 0.2626).

# En conclusion, el análisis de las correlaciones bivariantes y parciales sugiere
# que hay indicios de multicolinealidad.



## El archivo Basket.rda contiene datos relativos a partidos de la liga ACB de baloncesto. Los datos
## corresponden a 62 jugadores elegidos al azar y han sido obtenidos de la Gu´ıa Oficial de la Liga 1989-
## 1990 de la ACB (Asociaci´on de Clubs de Baloncesto). En base a esta muestra se desea estudiar si existe
## una relaci´on lineal entre la variable puntos por partido anotados por un jugador (puntos) con respecto
## a las siguientes regresoras: altura del jugador (altura), minutos que juega por partido (minutos),
## balones que pierde por partido (perdidas), faltas personales cometidas por partido (personales) y
## porcentaje de ´exito en tiros de dos y tres puntos por partido (porcentaje). En base a estos datos:

### (a) Ajustar un modelo de RLM con todas las regresoras del archivo. Evaluar la calidad del ajuste.
# Interpretar los resultados.

ajuste <- lm(puntos~., data=Datos)
summary(ajuste)

# Observando el F-test para chequear:
# H0: Modelo con intercept vs H1: Modelo con todas las covariables
# p-valor < 2.2e-16 -> Rechazamos H0
# Vemos que un modelo ajustado con todas las regresoras explica más que un modelo
# con sólo el intercept.
# Además observamos una alta capacidad predictiva, R^2 = 0.8336

# Observando los coeficientes vemos que todas las variables ayudan a explicar la respuesta
# salvo personales y quizás tambien perdidas.
# Sobre la respuesta, la variable que más peso tiene es el porcentaje de ´exito en tiros de
# dos y tres puntos por partido.

### (b) Analizar la existencia de multicolinealidad en el modelo ajustado.


# Examinar los factores de inflación de varianza de los coeficientes estimados (FIV)

( FIV <- vif(ajuste) )
# Todos los FIV < 10. Aunque se trata de un criterio general 
# que hay que tomar con cautela, no sugiere presencia de multicolinealidad
# Es útil comparar los FIV con el valor 1/(1-R^2), con R^2 el 
# coeficiente de determinación del ajuste:
( umbral <- 1/(1-summary(ajuste)$r.squared) )
print(FIV[FIV>umbral])
# No hay covariables que muestren mayor correlación 
# con el resto de covariables que la propia respuesta. De nuevo 
# no sugiere presencia de multicolinealidad.
# Veamos qué ocurre con los autovalores de la matriz de 
# correlaciones y el índice de condicionamiento IC:
rc <- eigen(cor(Datos))
( lambda <- rc$values ) 
# El índice de condicionamiento de la matriz de correlación: 
( IC <- sqrt(max(lambda)/min(lambda) ) )  # Mayor que 10
# Segun el indice de condicionamiento no tenemos multicolinealidad IC<10


# La librería "mctest" proporciona un análisis completo de multicolinealidad.

mctest(ajuste, type="i", method = "VIF") 
# Segun el metodo vif llega a la misma conclusion que nosotros, no hay multicolinealidad

# Diagnóstico general conforme a diferentes criterios: 
mctest(ajuste, type="o")  

# Sin embargo , a traves de otros metodos si se detecta presencia de multicolinealidad


### (c) Predecir y proporcionar un intervalo de predicci´on al 90% para los puntos que obtendr´a un jugador
#      de dos metros, que juega 25 minutos por partido, pierde cinco balones por partido, comete cuatro
#      personales y tiene un porcentaje de ´exito en lanzamientos de campo del 50%. Atipico? 

new.player <- data.frame(altura=200, minutos = 25, perdidas= 5, personales=4, porcentaje=0.5)

IC.90 <- predict(ajuste, se.fit = TRUE, newdata = new.player,
          interval = "prediction", level = 0.90)

#ic.90 <- predict(ajuste, se.fit = TRUE,interval = "prediction", level = 0.90)

# Estimacion:
(puntos_new.player <- IC.90$fit[1]) 

# Intervalos de predicción al 90%:
IC.90$fit[c(2,3)]

# Atipico? :

# Añadimos en nuevo jugador y vemos si es o no un dato atipico
# Se corresponde con la última observacion, la 63.
Datos[nrow(Datos) + 1,] = new.player



### (d) Identificar observaciones at´ıpicas y/o influyentes?

ajuste <- lm(puntos~., data=Datos)

# influence() y measures.influence()
influ <- influence(ajuste)
lev <- influ$hat            # Leverage

# which.max(lev) el mayor leverage, la observacion 63, es la 1ra candidata a generar problemas en la regresion.

# La observacion 63 se corresponde con el jugador que hemos añadido , por lo tanto, 
# un jugador de dos metros, que juega 25 minutos por partido, pierde cinco balones por 
# partido, comete cuatro personales y tiene un porcentaje de ´exito en lanzamientos de campo del 50%
# es un dato atipico.

which.max(lev)
#la media de los lev + 3*desv tipic
lev > mean(lev)+3*sd(lev)  
lev > 2*mean(lev)



#1/lev   ????
windows()
plot(lev, xlab = "Indice en la muestra", ylab = "Leverage", 
     cex = 1.2, pch=19, col=4, cex.lab=1.4)
# Para identificar los puntos que se desee:
identify(lev, labels=row.names(Datos)) 
# Viendo ese plot podemos ver como la observacion es claramente un dato atipico
# debido a su alto leverage.

# Eliminamos la observacion del dataset:

Datos <- Basket
ajuste <- lm(puntos~., data=Datos)

# influence() y measures.influence()
influ <- influence(ajuste)
lev <- influ$hat            # Leverage

which.max(lev)
# which.max(lev) el mayor leverage, ahora la observacion 45, es la 1ra candidata a generar problemas en la regresion.

#la media de los lev + 3*desv tipic
lev > mean(lev)+3*sd(lev)  
lev > 2*mean(lev)



#1/lev   ????
windows()
plot(lev, xlab = "Indice en la muestra", ylab = "Leverage", 
     cex = 1.2, pch=19, col=4, cex.lab=1.4)
# Para identificar los puntos que se desee:
identify(lev, labels=row.names(Datos)) 

# La observacion 45 presenta un elevado leverage
# por tanto es un potencial punto influyente.


# influence.measures() que proporciona diferentes 
# medidas diagnÃ³sticas (DFBETAS, DFFITS, COVRATIO y 
# Distancia de Cook), indicando con un asterisco los  
# puntos influyentes en la columna "inf":
PI <- influence.measures(ajuste)
#PI
# Resumen de puntos influyentes:
summary(PI)   

# Analizando los resultados de la primera columna no hay datos que influyan 
# significativamente en la estimacion  de beta_0.

# Analizando los resultados de las columnas dfb (DFBETAS) de cada variable concluimos que no 
# hay datos que influyan significativamente en la estimacion del coeficiente que acompaña 
# a cada variable.

# Por otra parte, en la columna dffit (DFFITS) las observaciones 3 y 5 se consideran 
# significativamente influyentes. Esto es, la prediccion de los puntos 3 y 5 cambia signficativamente
# con o sin ellos.

# Un criterio en la literatura es declarar influyente
# la observación si su covratio satisface:
# |covratio-1| >= 3*p/n (p=parámetros del modelo) 
p = dim(Datos)[2]
n = dim(Datos)[1]

abs(1.36-1) >= 3*p/n ; cat('Es influyente')
abs(0.58-1) >= 3*p/n ; cat('Es influyente')
abs(0.62-1) >= 3*p/n ; cat('Es influyente')
abs(1.41-1) >= 3*p/n ; cat('Es influyente')
abs(1.61-1) >= 3*p/n ; cat('Es influyente')
abs(0.65-1) >= 3*p/n ; cat('Es influyente')

# Por parte de la distancia de cook(cook.d) no tenemos observaiones 
# significativamente influyentes.

# Veamos una representaciÃ³n grÃ¡fica
n<-nrow(Datos)
estadistico <- colnames(PI$infmat)
nstat <- length(estadistico)

# DFBETAS:
for(i in 1:6){
  dotchart(PI$infmat[,i],main=estadistico[i],xlim=c(-2,2))
  # lÃ­neas para el umbral 2/sqrt(n):
  abline(v=2/sqrt(n),lty=2)
  abline(v=-2/sqrt(n),lty=2)
  # En rojo los significativos (R no emplea el mismo umbral)
  puntos.i<-which(PI$is.inf[,i]==T)  # localizaciÃ³n influyente
  vpuntos.i<-PI$infmat[puntos.i,i]
  points(vpuntos.i,puntos.i,pch=21,bg="red",col="red")
}


# No hay puntos rojos, es decir, no hay observaciones significativamente influyentes
# segun el criterio DFBETAS

# DFFITS:
i<-7
dotchart(PI$infmat[,i],main=estadistico[i])
puntos.i<-which(PI$is.inf[,i]==T)
vpuntos.i<-PI$infmat[puntos.i,i]
points(vpuntos.i,puntos.i,pch=21,bg="red",col="red")
# y valores criticos:
p <-length(ajuste$coef) # coeficientes en el ajuste
abline(v=2*sqrt(p/n),lty=2)
abline(v=-2*sqrt(p/n),lty=2)

# hay dos observaciones consideradas significativamente influyentes (3 y 5)


# Distancia D de Cook
plot(ajuste) # El cuarto plot
ols_plot_cooksd_bar(ajuste) 
# Hay varias observaciones por encima del humbral, sin embargo no son consideradas significativas


### (e) ¿Cu´al ser´ıa el modelo m´as adecuado? Proporciona su ajuste y evaluaci´on.

# Probaremos desde el modelo nulo hasta el modelo completo:
# Modelo nulo conteniendo solo el intercept:
MOD_NULL <- lm(puntos ~ 1, data = Datos)
# Modelo completo conteniendo todas las covariables:
MOD_FULL <- ajuste

stepMod <- step(MOD_NULL, direction = "both", trace = 1,
                scope = list(lower = MOD_NULL, 
                             upper = MOD_FULL) )

# El modelo final incluye todas las variables.
# Esto confirmaria que no hay presencia de multicolinealidad. Lo cual concuerda con lo 
# dicho en apartado b.
# Tambien concuerda con que las variables que tienen que salir del modelo son las que
# cuyo t valor^2 < 1.
summary(MOD_FULL)
# En este caso ninguna variable es candidata a salir.

# No obstante el Aic no dista más de tres unidades que el modelo que incluye
# solo a minutos + porcentaje.

MOD_min <- lm(puntos ~ minutos) 
summary(MOD_min)
anova(MOD_min, MOD_FULL)
# Un modelo solo con minutos es significativamente peor que el modelo completo
# p-valor <<< 0.05


MOD_min_porcentaje <- lm(puntos ~ minutos+porcentaje) 
summary(MOD_min_porcentaje)
anova(MOD_min_porcentaje, MOD_FULL)

# Un modelo con minutos y porcentaje es significativamente peor que el modelo completo
# a un nivel de significacion del 5%, sin embargo esto ya no es así para un nivel
# de significacion del 4%. Por lo tanto no podríamos quedar con este segundo modelo
# con la idea de conseguir un modelo válido pero sencillo.

# Bondad del ajuste
summary(MOD_min_porcentaje)$adj.r.squared
summary(MOD_FULL)$adj.r.squared

# Además seguimos obteniendo una alta capacidad predictiva (0.80), poco distante 
# de la que obteniamos con el modelo completo (0.81)

# Coeficiente de robustez (cuanto mas cerca a uno mejor)

sum(residuals(MOD_min_porcentaje)^2)/press(MOD_min_porcentaje)
sum(residuals(MOD_FULL)^2)/press(MOD_FULL)

# Obtenemos un mejor coeficiente de robustez que con el modelo completo.


# Nuestro ajuste por lo tanto es el siguiente:

(ajuste <- lm(puntos~minutos+porcentaje))
# Puntos = -12.3711 + 0.5631*Minutos + 17.1156*Porcentaje

summary(ajuste)

### (f) Analizar los residuos del modelo que se considere m´as adecuado.

MSSR <- summary(ajuste)$sigma^2

# Residuos del ajuste
res <- residuals(ajuste)    
res.est1 <- res/sqrt(MSSR)    
res.est  <- stdres(ajuste)    
# igual a rstandard() en stats
res.stud <- studres(ajuste) 
# Todas son formas diferentes de definir los residuos:
cbind(res,res.est1,res.est,res.stud)
# standarizados vs studentizados
plot(ajuste$fitted.values, res.est, 
     type="p",col=4,pch=19, xlab="Ajustados",ylab="Residuos", 
     ylim=range(cbind(res.est,res.stud)))
points(ajuste$fitted.values,res.stud, col=2,pch=17)
legend("topleft",c("Estandarizados","Studentizados"),
       pch=19,col=c(4,2),lty=c(-1,-1))

# LINEALIDAD.
# Gráfico:
scatter.smooth(ajuste$fit, res.est, main="Residuos ~ Ajustados", 
               xlab="Ajustados",ylab="Residuos", pch = 21, 
               bg = "green", cex.lab=1.5, cex=1.4, cex.main=1.5, 
               lpars = list(col = "magenta", lwd = 3) )
abline(h=0,lty=2,lwd=2)

# Como no obtenemos linealidad transformamos los datos con un polinomio.
# Con la funcion poly buscamos el orden apropiado.
f1 = lm(puntos~minutos+porcentaje)

f2 = lm(puntos~poly(minutos,2)+poly(porcentaje,2))
summary(f2)
# El término cuadrático de porcentaje no es significativo, 
# luego no se gana nada usando poly con porcentaje. Si que es significativo 
# con minutos. Buscamos el orden cuadrático de minutos.

f2 = lm(puntos~poly(minutos,2)+porcentaje)

f3 = lm(puntos~poly(minutos,3)+porcentaje)
f4 = lm(puntos~poly(minutos,4)+porcentaje)
f5 = lm(puntos~poly(minutos,5)+porcentaje)

# Chequeamos si el término cúbico debe o no de entrar:
anova(f1,f2,f3,f4,f5)

# A un 5% de significación, llegaríamos hasta un 
# ajuste de un polinomio de orden 3

anova(f2,f3)
# El modelo de orden cuadratico 3 es significativgamente mejor que el de orden 2

# Nuestro modelo transformado es el siguiente:

# Puntos = 3.026 + 40.985*minutos + 14.177*minutos + 5.231*minutos + 13.984*porcentaje

ajuste <- f3


MSSR <- summary(ajuste)$sigma^2

# Residuos del ajuste
res <- residuals(ajuste)    
res.est1 <- res/sqrt(MSSR)    
res.est  <- stdres(ajuste)    
# igual a rstandard() en stats
res.stud <- studres(ajuste) 
# Todas son formas diferentes de definir los residuos:
cbind(res,res.est1,res.est,res.stud)
# standarizados vs studentizados
plot(ajuste$fitted.values, res.est, 
     type="p",col=4,pch=19, xlab="Ajustados",ylab="Residuos", 
     ylim=range(cbind(res.est,res.stud)))
points(ajuste$fitted.values,res.stud, col=2,pch=17)
legend("topleft",c("Estandarizados","Studentizados"),
       pch=19,col=c(4,2),lty=c(-1,-1))

# LINEALIDAD.
# Gráfico:
scatter.smooth(ajuste$fit, res.est, main="Residuos ~ Ajustados", 
               xlab="Ajustados",ylab="Residuos", pch = 21, 
               bg = "green", cex.lab=1.5, cex=1.4, cex.main=1.5, 
               lpars = list(col = "magenta", lwd = 3) )
abline(h=0,lty=2,lwd=2)

# Ahora ya obtenemos linealidad en los residuos

# NORMALIDAD
# Gráficos
par(mfrow=c(2,2))
# Histograma y ajuste normal
hist(res.est, breaks=6,freq=FALSE, main = "", xlab="Residuos", cex.lab=1.4, 
     ylab = "Densidad", col = "lightblue", ylim=c(0,0.6))
curve( dnorm(x), col="magenta", lwd=3, add=TRUE)
etiquetas <- c("Histograma","Ajuste normal")
legend("topright",etiquetas, lwd=2, col=c("lightblue","magenta"), 
       lty=c(1,1), cex=1.3, inset=0.02, box.lty=0)
# Diagrama de cajas
boxplot(res.est, main = "", xlab="Residuos",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)
# Ajuste no paramétrico (kernel)
plot(density(res.est, bw=0.4),main="",lwd=3,col="blue",
     ylab="Densidad stimada residuos", cex.lab=1.4, cex.lab=1.4)
polygon(density(res.est,bw=0.4), col="lightblue")
curve( dnorm(x), col="magenta", lwd=3, add=TRUE)
# qqnorm(res.est) ;  qqline(res.est) o alternativamente:
qqPlot(res.est)     # en la librería car

# Parece que tenemos normalidad, tambien aparece algun dato atipico


# Analíticos:
# H0: errores normales  vs errores no normales
nortest::lillie.test(res.est) # Prueba de Lilliefors 
nortest::cvm.test(res.est)    # Prueba de Cramer Von-Mises
nortest::ad.test(res.est)     # Prueba de Anderson-Darling
shapiro.test(res.est)         # Prueba de Shapiro-Wilks
moments::agostino.test(res.est)         # Prueba de D'Agostiino

# todos los p-valores >> 0.05
# Aceptamos que los residuos siguen una distribucion normal

# HOMOSCEDASTICIDAD
plot(ajuste, which=3)
# H0: sigma^2=cte  vs H1: sigma^2!=cte
# Test de Breusch-Pagan
ncvTest(ajuste)  # equivale a bptest(ajuste,studentize = FALSE)
bptest(ajuste)   # en por defecto studentize = TRUE

# Aceptamos homocedasticidad para un nivel de significacion del 5%

# ALEATORIEDAD
Box.test(res.est, lag = 5, type = "Ljung-Box")	   	 # Prueba de Ljung-Box 
runs.test(as.factor(sign(res.est)))	                 # Prueba de rachas 

# En ambos test de aleatoriedad obtenemos un p-valor alto, por lo que aceptamos 
# aleatoriedad en los residuos




