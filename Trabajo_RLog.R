# Haremos uso de las siguientes librer√≠as:


library(car)
library(ggplot2)
library(pscl)           # Medidas de bondad de ajuste
library(vcd)            # Gr√°fico matriz de confusi√≥n
library(caret)          # Estad√≠sticos matriz de confusi√≥n
library(dplyr)    # Gesti√≥n de datos 
library(ggplot2)  # Algunos gr√°ficos
library(MASS)
library(car)
library(moments)  # Simetr√≠a y kurtosis - ver tambi√©n library(e1071) 
library(olsrr)    # An√°lisis de influencia
library(psych)    



###Ejercicios Propuestos: ejercicio 9 
# Estimar la probabilidad de que un estudiante sea mujer cuando se conoce su peso
# y su altura en base a los datos del fichero Estatura.rda que, adem¬¥as de la 
# estatura, peso y sexo del estudiante (1: mujer, 0: hombre) contiene variables
# con las alturas de sus padres. Realizar un an¬¥alisis detallado del modelo
# ajustado, de la bondad del ajuste y de la conducta de los residuos

Estatura

# Exploracion del archivo de datos
###########################################################
head(Estatura)
str(Estatura) 
summary(Estatura)
dim(Estatura)

# Estatura es un data.frame con 75 registros de 5 variables, todas ellas continuas
# excepto la variable sexo que se trata de una variable tipo binaria
# Se trata de la altura y peso de 75 estudiantes junto con las alturas de sus
# respectivos padres

### Planteamiento del problema

# Nuestro analisis se va a centrar en el estudio de 
# Y = Sexo (respuesta) y X = Altura,Peso,Altmadre,Altpadre (regresoras)
attach(Estatura)

# Entendiendo que el exito es "ser mujer" (Estatura$sexo=1), 
# Como se distribuyen exitos y fracasos?
table(Estatura$sexo); table(Estatura$sexo)/nrow(Estatura)

# Luego, sin ajuste de modelo alguno, la probabilidad de 
# exito (de ser mujer) se estima en 0.24 (18/(18+57))

################################################################################
################################################################################

# ESTUDIO ANALITICO Y GRAFICO:
# ESTUDIO ANALATICO: ALTURA

summary(altura) 

# Mediante este estudio analitico concluimos que el valor minimo de la altura es 
# de 148.9 cm, la media de 172.7 cm y el valor m√°ximo de 192.1 cm.

cat("sd = ",sd(altura),"\nIQR = ", IQR(altura), "\nasimetria = ", skewness(altura, na.rm = FALSE), "\nkurtosis = ", kurtosis(altura, na.rm = FALSE))

#' A desv tÌpica È de 9.707383, o rango intercuartil È de 12.7, o coef de asimetrÌa 
#' È negativo pero non elevado, o cal indica que hai unha lixeira asimetrÌa negativa,
#' e a kurtosis, que indica como se concentra unha distribuciÛn ao redor da s˙a media, 
#' È menor que 3, o que quere dicir que È unha distribuciÛn platicurtica.

# ESTUDIO ANALITICO: PESO

summary(peso) 
# Mediante este estudio analitico concluimos que el valor minimo de peso es de
# 36.5 kg, la media de 67.26 kg y el valor maximo de 99.5 kg.

cat("sd = ",sd(peso),"\nIQR = ", IQR(peso), "\nasimetria = ", skewness(peso, na.rm = FALSE), "\nkurtosis = ", kurtosis(peso, na.rm = FALSE))
#' A desv tÌpica È de 13.68404, o rango intercuartil È de 19.55, o coef de asimetrÌa 
#' È negativo pero prÛximo a 0, o cal indica que hai unha moi lixeira asimetrÌa negativa,
#' e a kurtosis, que indica como se concentra unha distribuciÛn ao redor da s˙a media, 
#' È menor que 3, o que quere dicir que È unha distribuciÛn platicurtica.


# ESTUDIO GR√ÅFICO: ALTURA

# Histograma y ajuste normal
x11()
par(mfrow=c(1,2))
hist(altura,freq=FALSE, main = "", xlab="Altura",
     ylab = "Densidad altura", col = "lightblue")

curve( dnorm(x,mean=mean(altura),sd=sd(altura)), 
       col="magenta", lwd=3, add=TRUE) 

# Como no gr·fico non se observa con claridade se segue unha normal ou non, imos realizar o test de Shapiro Wilk:
shapiro.test(altura) # Cun p-valor de 0.1801, non cabe rexeitar a normalidade cun nivel de significaciÛn 0.05

# Diagrama de cajas
boxplot(altura, main = "", xlab="Altura",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3) # Detecta los datos at√≠picos




# A travÈs del histograma, su ajuste normal y el test de Shapiro Wilk podemos observar que existe normalidad 
# en la altura de los estudiantes.

# En el diagrama de cajas podemos ver que no hay puntos atipicos; 
# por tanto, todos los estudiantes tienen una altura dentro de la normalidad
# y no hay ning˙n caso extremo que afecte, en principio, al futuro ajuste.

# Comprobamos la inexistencia de datos atipicos:
length(boxplot.stats(altura)$out)


# ESTUDIO GRAFICO: PESO

# Histograma y ajuste normal
par(mfrow=c(1,2))
hist(peso,freq=FALSE, main = "", xlab="Peso",
     ylab = "Densidad del peso", col = "lightblue")

curve(dnorm(x,mean=mean(peso),sd=sd(peso)), 
       col="magenta", lwd=3, add=TRUE) 

# Como no gr·fico non se observa con claridade se segue unha normal ou non, imos realizar o test de Shapiro Wilk:
shapiro.test(peso) # Cun p-valor de 0.7116, non cabe rexeitar a normalidade cun nivel de significaciÛn 0.05


# Diagrama de cajas
boxplot(peso, main = "", xlab="Peso",
         border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3) # Detecta los datos at√≠picos


# A trav√©s del histograma y de su ajuste normal podemos observar que existe  
# normalidad en los pesos de los estudiantes.

# En el diagrama de cajas podemos ver que no hay puntos atipicos; 
# por tanto, todos los estudiantes tienen un peso similar
# y no hay ning√∫n caso que afecte, en principio, al futuro ajuste.
# Comprobamos la inexistencia de datos atipicos:
length(boxplot.stats(peso)$out)


# Realizamos una matriz de disperion de las variables 
# dos a dos y obtenemos la matriz de correlaciones para observar si hay alguna 
# dependencia entre las variables
pairs(Estatura[,-3]) # O [,-3] sirve para deixar fora a variable binaria, que non teria sentido neste grafico

# Na matriz de dispersion vemos como altura e peso teÒen una correlaciÛn 
# positiva moi forte, asÌ que imos confirmalo na matriz de correlaciones
# A priori, no hai m·is relaciÛns importantes, o que tamen imos comprobar

cor(Estatura[,-3])


#' Efectivamente, altura e peso teÒen unha forte correlacion (cor = 0.8569989 ), 
#' e as demais son moderadas

### ANALISIS EXPLORATIVO MULTIVARIABLE

# Como se reparten homes e mulleres segundo a altura?
x11()
boxplot(altura ~ sexo, main = "", xlab="Altura",
        cex.lab=1.4, border = "blue", col= "lightblue", pch="+",
        horizontal = TRUE, cex=3)


# En este grafico de cajas vemos claramente que los hombres tienden a tener una 
# altura superior que las mujeres. Los hombres tienen alturas entre los 165 y 192 
# centimetros, mientras que las mujeres tienen alturas entre los 149 y 170 cent√≠mentros
# Comprobamos que no hay datos at√≠picos 

mulleres <- Estatura[Estatura$sexo=="1",]
homes <- Estatura[Estatura$sexo=="0",]
length(boxplot.stats(mulleres$altura)$out)
length(boxplot.stats(homes$altura)$out)
# 0 en ambos casos

# Como se reparten hombres y mujeres segun el peso?

boxplot(peso ~ sexo, main = "", xlab="Peso",
        border = "blue", col= "lightblue", pch="o",
        horizontal = TRUE, cex=1) # Detecta los datos at√≠picos


# En este grafico de cajas vemos algo parecido a lo que ocurria con las alturas.
# Los hombres tienen pesos distribuidos entre los 53 y los 91 kilogra os, habiendo  
# un valor at√≠pico que llega a los 100 kilogramos, mientras que las mujeres tienen
# pesos claramente menores, entre los 38 y los 60 kilogramos
# Comprobanos que no hay datos at√≠picos en las mujeres pero si en los hombres
length(boxplot.stats(mulleres$peso)$out)
length(boxplot.stats(homes$peso)$out) # Aqui existe un dato atipico


#TeÒen influencia os pais na altura e peso de cada un?

pairs(Estatura[,-3], col = ifelse(sexo==1,"red","green"), pch=1)


# En esta matriz de dispersion de las variables dos a dos vemos:
# Lo que anteriormente observamos, los hombres (puntos verdes) tienden a alturas
# y a pesos superiores que las mujeres (puntos rojos).
# Los hombres tienden a tener alturas similares a la de sus padres, por otro lado
# las mujeres tienden a tener alturas similares a la de sus madres. 
# Esto lo podemos ver anal√≠ticamente si comparamos las medias:
mean(homes$altura);mean(homes$altpadre)
mean(mulleres$altura);mean(mulleres$altmadre)

##############################################################################
##############################################################################
##############################################################################


# AJUSTE LOGISTICO : 
###########################################################

logistico.1 <- glm(sexo ~ ., data = Estatura, family=binomial)
summary(logistico.1)
# Axuste non significativo, xa que ningunha das variables È significativa ao 5%


# Observamos la significacion estadistica de cada covariable en base al descenso de las deviance 
# (razon de verosimilitudes):
drop1(logistico.1, test ="Chisq")

# Probamos a ver si mejora eliminando altpadre,a variable con menor significacion do axuste
logistico.2 <- glm(sexo ~ .-altpadre, data = Estatura, family=binomial)

anova(logistico.2, logistico.1, test="Chisq")

# Concluimos que la deviance (-2 ln L(beta)) al quitar "altpadre" se incrementa en LRT=0.98493
# Deviance(modelo sin altpadre)-Deviance(modelo completo) = 0.98493
# O p-valor non indica rexeitar o modelo m·is sinxelo en favor do saturado a un
# 5% de significacion, polo que imos quedarnos co modelo sen a variable altpadre

summary(logistico.2) # Imos repetir o proceso

drop1(logistico.2, test ="Chisq") # Todas as variables son significativas ao 5%

#Probamos a ver si mejora eliminando altmadre, que ten o p-valor m·is elevado
logistico.3 <- glm(sexo ~ .-(altmadre+altpadre),data = Estatura, family=binomial)
anova(logistico.3, logistico.2, test="Chisq")

# O test do anova non È significativo ao 5%, polo que se rexeita eliminar esta variable
# Imos quedar co modelo logistico.2
(coef <- summary(logistico.2)$coefficient)



# Interpretacion y significacion de los coeficientes
###########################################################
(coef <- summary(logistico.2)$coefficient)

# Coeficientes significativos al 10%
# Por un lado altura y peso negativos, o que indica que as posibilidades de ser muller
# dimin˙en canta m·is altura e peso se ten, ao contrario que altmadre, ainda que esta 
# influe en menor medida ao ser m·is proxima a 0
 

### ODDs
# Odds:
( odds <- exp(coef[,1]) )

# Intervalos de confianza para os odds:
exp(confint.default(logistico.2))

# Resumen de coeficientes estimados:
resumen.coef <- cbind(odds, coef)
print(resumen.coef, digits=3)

# Interpretacion:

# Mantendo constante o valor das demais variables, 
# por incremento unitario en altmadre o ln(odds) de ser muller
# incrementase en 0.164, È dicir, os odds de ser 
# muller multiplicase por exp(0.164)=1.178214, 
#indicando que a posibilidade de ser muller vs non selo crece coa variable altmadre


# Mantendo constante o valor das demais variables, 
# por incremento unitario en peso o ln(odds) de ser muller
# diminue en 0.287 (-0.287), È dicir, a razon entre ser 
# muller e no selo multiplicase por 
# exp(-0.287)=0.7505117, indicando que a posibilidade de ser 
# muller vs non selo diminue coa variable peso

# Mantendo constante o valor das demais variables, 
# por incremento unitario en altura o ln(odds) de ser muller
# diminue en 0.367 (-0.367), È dicir, a razon entre ser 
# muller e no selo multiplicase por 
# exp(-0.367)=0.6928096, indicando que a posibilidade de ser 
# muller vs non selo diminue coa variable altura

# Bondad de ajuste del modelo
###########################################################
( Dev <- summary(logistico.2)$deviance )
( Dev.0 <- summary(logistico.2)$null.deviance )
# Deviance moito menor na deviance do modelo non nulo, o que quere dicir que explica mellor a resposta
# Test de razon de verosimilitudes:
with(logistico.2, pchisq(null.deviance - deviance, 
                         df.null - df.residual, lower.tail = FALSE))
# Significativo, 6.236238e-14
# Alternativamente:
anova(glm(sexo~1,data=Estatura,family=binomial),logistico.2, 
      test="Chisq")
# Ao seren significativas ambas probas, a razÛn entre ambas È significativamente 
# distinta de 1, È dicir, son significativamente distintas e polo tanto podemos
# afirmar que o modelo que mellor explica de estos dous È de menor deviance


# R^2 de McFadden:
1-Dev/Dev.0
# A porcentaxe de verosimilitude explicada polo noso modelo con respecto a do modelo nulo e de 0.780986

# AIC y BIC
AIC(logistico.2)
BIC(logistico.2)
# Valores de AIC e BIC non elevados, o que quere dicir que a verosimilitude en 
# relacion co numero do parametros do modelo È boa, È un bo modelo pero tamen parsimonioso

# Prueba de Hosmer-Lemeshown

library(generalhoslem)
logitgof(Estatura$sexo, fitted(logistico.2)) 
# p-valor non significativo ao 5%, logo non cabe rexeitar a H0, o cal quere dicir
# que o modelo axustase · realidade en canto ·s frecuencias obseervadas de exitos Y = 1
# e non hai un erro de predicion significativo

# Matriz de confusion.
# Establecemos el umbral en hat(p)=0.5. Es decir, si 
# hat(p)>0.5 se predice hat(Y)=1 (mujer) y en otro caso 
# hat(Y)=0 (hombre). 
pred <- ifelse(test = logistico.2$fitted.values > 0.5, 
               yes = 1, no = 0)
( m_confusion <- xtabs(~ pred + sexo) )

# Falsos positivos: Hombres predichos como mujeres
FP <- 2
# Verdaderos positivos: Mujeres predichos como mujeres
VP <- 16
# Falsos negativos: Mujeres predichos como hombres
FN <- 2
# Verdaderos negativos: Hombres predichos como Hombres
VN <- 55

# Con la librer√≠a "caret" se obtienen un conjunto importante 
# de estad√≠sticos asociados a una matriz de confusi√≥n:
confusionMatrix(table(pred, Estatura$sexo), positive="1")

# De especial interes es:
# Sensitividad: 
#     proporcion de verdaderos unos predichos con unos, 
#     probabilidad de predecir correctamente un 1
# En nuestro caso, bastante elevada (0.8888888)
( sens <- m_confusion[2,2]/apply(m_confusion,2,sum)[2]  )

# Especificidad: 
#    proporcion de verdaderos ceros predichos con ceros
#    probabilidad de predecir correctamente un 0
# En nuestro caso muy alta: 0.9649123
( espec <- m_confusion[1,1]/apply(m_confusion,2,sum)[1] )

# En general, estas medidas estan sesgadas y deben de evaluarse 
# por validacion cruzada. La funcion cv.glm() de la libreria
# boot hace esto de forma automatica:
library(boot)
set.seed(10203)
# Ejecutamos k-fold CV usando k=5
# primer argumento el data.frame de los datos
# segundo argumento el modelo ajustado
1-cv.glm(Estatura,logistico.2,K=5)$delta[2]
# EstimaciÛn do cross-validation axustada moi elevada:  0.9511703

# Tasa de clasificacion correcta (precision o accuracy) 
# es razonablemente alta:
TCC <- sum(diag(m_confusion))/sum(m_confusion)
TCC

# Dian√≥stico del modelo
# Como con lm, plot.glm() genera los gr√°ficos de residuos,
# considerando los residuos estandarizados de Pearson y 
# enfrent√°ndolos a los logit (predicciones lineales), o que È dicir
# al vector que resulta de ejecutar:

par(mfrow=c(1,1))
plot(logistico.2) # Ese È o bo

plot(logistico.2, which = 4) # Vemos como hai 2 datos con distancia de Cook >0.5, 
# cal quere dicir que o cambio no vector de coeficientes estimados sen esos datos e con eles È elevado
# Son puntos moi influintes no modelo

library(car)
# Para linealidad (eligiendo el tipo de residuos)

car::residualPlots(logistico.2, terms=~1, rank,type="deviance")
# Para ausencia de linealidad respecto a las distintas regresoras 
# Ajustes de regresion parciales
car::avPlots(logistico.2,terms=~.)
# Residuos parciales
crPlots(logistico.2)


## Se o modelo È correcto, os residuos deviance deben seguir aproximadamente unha normal N(0, 1)



# Multicolinealidad
# La multicolinealidad puede examinarse tambi√©n en un GLM.
# Al fin y al cabo se trata de examinar la existencia de 
# correlaci√≥n entre las regresoras. Ahora bien, cuando la 
# varianza del modelo no es constante, se propone emplear 
# la INFLACCI√ìN DE LA VARIANZA GENERALIZADA que trata de 
# corregir el efecto de la heteroscedasticidad. 

vif(logistico.2) # Todos < 10, non hai multicoliÒalidade


### Analisis de influencia: 

im <- influence.measures(logistico.2)
summary(im)

# Os datos mais influintes son, como xa vimos antes coa distancia de Cook, o 38 
# e o 73, xa que son significativamente influintes en pr·ticamente todas as probas