# Example of multivariate correction PCA of drift in data
# @ http://doi.wiley.com/10.1002/1099-128X(200009/12)14:5/63.0.CO;2-4
#REFERENCE: https://github.com/variani/drift-multicomp/blob/master/pca/03-ref-class.R
# Notation for PCA: X = T t(P) + E
#  - X: data matrix
#  - T: scores matrix
#  - P: loadings matrix
#  - E: error matrix

### include
library(pls)
library(reshape) 
library(ggplot2)
library(grid) 

### parameters
# -X:  matriz UNIMANshort$dat 200 filas 17 columnas  - 200 medidas 17 sensores
# -Y:  matriz de concetración UNIMANshort$C 200 filas 3 columnas - 3 gases 8 concentraciones

load("/home/susana/Documents/projetcs/01-component-correction/Data/UNIMANshort.XY.RData")
X
Y

ref <- "A 0.05" # reference class

### center/scale data

X <- scale(X, center = TRUE, scale = TRUE) # See `attributes(X)`
colnames(X)<-paste("S", 1:17, sep = "")

### Plot Original date for two columns

plot(X[,1], X[,2], col= Y, main= "UNIMANshort Original Data with DRIFT", type="p",cex = .9, pch=18, xlab='S1', ylab='S2')
legend("bottomright", fill=1:8, border="BLUE", levels(Y), bty="n", ) 
grid(lty=4,lwd=2)


### PCA model #0: original data UNIMANshort

M <- prcomp(X, center = FALSE, scale = FALSE) # `X` are already scaled

T <- M$x

summary(T)

### Plot to PCA, PC1 Vs PC2

scoreplot(M, col=Y,main= "PCA SCORES UNIMANshort Original Data with DRIFT", type="p",cex = .5, pch=10)
legend("bottomright", fill=1:8, border="BLUE", levels(Y), bty="n") 
grid(lty=3,lwd=1, col=4)


## PCA model #1: reference class
ind <- which(Y == ref)

Xr <- X[ind, ] # note: we the data are scaled, but for all three classes

Xr <- scale(Xr, center = TRUE, scale = FALSE) # we do centering (to capture PC1, 

# the major variance direction), but we do not do scaling


plot(Xr[,1], Xr[,2], main= "GAS REFERENCE UNIMANshort with DRIFT", col=4, type="p",cex = .9, pch=18, xlab='S1', ylab='S2')
grid(lty=3,lwd=1, col=4) ### revisar para poner el legend??????

## PCA to Gass Reference

Mr <- prcomp(Xr, center = FALSE, scale = FALSE) 
Tr <- Mr$x

## To plot PC1 y PCE the gass Reference

scoreplot(Mr, main= "PCA UNIMANshort Class Reference", col="green",type="p",cex = 0.9, pch=18) 
grid(lty=3,lwd=1, col=4)


## DIRECCIÓN DE LAS DERIVAS

reg<- lsfit(Xr[,1], Xr[,2])
#abline(reg)
plot(Xr[,1], Xr[,2], main= "GAS REFERENCE UNIMANshort DIRECCIÓN DEL DRIFT", col=4, type="p",cex = .9, pch=18, xlab='S1', ylab='S2')
grid(lty=3,lwd=1, col=4) ### revisar para poner el legend??????
arrows(0, 0, Xr[2,1], Xr[2,2])


E <- Mr$rotation 
E

# columns of `E` orthogonal?
E[, 1] %*% E[, 2]


# select just one PC, en forma de vector columna
E1 <- E[, 1, drop = FALSE]

plot(E1, main= "PC1 DE GASS REFERENCE", col=4, type="p",cex = .9, pch=18)
grid(lty=3,lwd=1, col=4)

## UNIMANshort Dataset CORREGIDO GAS DE REFERENCIA A 0.05

Xc <- X - (X %*% E1) %*% t(E1)

plot(Xc[,1], Xc[,2], col= Y, main= "UNIMANshort Data without DRIFT", type="p",cex = .9, pch=18, xlab='S1', ylab='S2')
legend("bottomright", fill=1:8, border="BLUE", levels(Y), bty="n") 
grid(lty=4,lwd=2)

## PCA UNIMANshort Dataset CORREGIDO GAS DE REFERENCIA A 0.05

Mc <- prcomp(Xc, center = FALSE, scale = FALSE) # `X` are already scaled

## PLOT PCA UNIMANshort Dataset CORREGIDO GAS DE REFERENCIA A 0.05

scoreplot(Mc, col=Y,main= "PCA SCORES UNIMANshort Data without DRIFT, (A 0.05)", type="p",cex = .5, pch=10)
legend("bottomleft", fill=1:8, border="BLUE", levels(Y), bty="n") 
grid(lty=3,lwd=1, col=4)


##guardar las matriz Xc de datos corregidos, gas de referencia A 0.05
save(Xc, file = "/home/susana/Documents/projetcs/01-component-correction/Data/UNIMANshort.Xc.RData")