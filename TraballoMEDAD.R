load("C:/Users/uxiom/Downloads/vote.RData")
load("C:/Users/claralado/Downloads/vote.RData")

vote #Base de datos inicias
head(vote)
n<-16
anos <- 18

#Sacamos as columnas utiles
porcentaxe_voto <- vote[, 3:20]
porcentaxe_voto

#Sacamos os códigos dos estados como identificador das filas
rownames(porcentaxe_voto) <- vote[, 1]
porcentaxe_voto
rownames(porcentaxe_voto)

#Creamos a matriz de distancias:

d1 <- diag(16)
d <- d1-d1 #Matriz con todo ceros

for (i in 1:n){
  for (t in 1:n){
    resta <- abs(porcentaxe_voto[i,]-porcentaxe_voto[t,])
    prob <- rowMeans(resta)
    d[i,t] <- d[t,i] <-prob
    
  }
}

d  #Obtida a matriz de distancias traballaremos sobre ela

# Exercicio 1:
A <- (-1/2)*d^2
A

# Exercicio 2:
H <- diag(rep((n-1)/n,length.out=n))  # Na diagonal 15/16
H[which(H==0)] <- -1/n   # Nas outras posicións, -1/16
H

# Exercicio 3:
B <- H %*% A %*% H
B

# Exercicio 4:
eigen(B)
lambda <- eigen(B)$values      # Autovalores
lambda

autovec <- eigen(B)$vectors    # Autovectores
autovec

#Xustificación 2 autovectores
autoval <- prcomp(B, center = TRUE, scale. = TRUE)
windows()
screeplot(autoval, type="lines")

# Exercicio 5:
vec12 <- autovec[,1:2] # Traballamos cos dous primeiros autovectores
lambda_12 <- diag(lambda[1:2])

result <- vec12 %*% sqrt(lambda_12) #Cadrado do seu módulo = autovalor
result


# Exercicio 6: (Representación gráfica)

names.c <- vote[, 1]
windows()
plot(result, pch=18, main="Resultado del MDS clásico", xlab="x",ylab="y",
     xlim=c(-25,20),ylim=c(-7,7))
text(result-c(-0.25,0.25), labels=names.c,cex=0.9, font=2)


# Exercicio 11: (Representación coa función propia de R)
MDS_R <- cmdscale(d, eig = TRUE, k = 2)
x <- MDS_R$points[, 1]
y <- MDS_R$points[, 2]
windows()
plot(x, y, pch = 16, xlim = c(-25, 20), ylim = c(-7, 7), main = "Resultado MDS con cmdscale")
text(result-c(-0.25,0.25), labels=names.c,cex=0.9, font=2)


# Exercicio 12:
 # Cálculo da matriz de distancias:
d2<- diag(anos) 
dm <- d2-d2
for (i in 1:anos){
  for (t in 1:anos){
    resta <- abs(porcentaxe_voto[,i]-porcentaxe_voto[,t])
    prob <- mean(resta)
    dm[i,t] <- dm[t,i] <-prob
    
  }
}
dm

 # Imos utilizar a función propia de R para realizar o MDS sobre as variables
MDSR <- cmdscale(dm, eig = TRUE, k = 2)
x <- MDSR$points[, 1]
y <- MDSR$points[, 2]
windows()
plot(x, y, pch = 19, xlim = c(-20,15), ylim = c(-7, 17), main = "MDS con cmdscale sobre as variables")
names.m <- colnames(porcentaxe_voto)
text(x, y, pos = 3, labels = names.m)