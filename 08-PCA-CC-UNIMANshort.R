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

load("data/UNIMANshort.XY.RData")
X
Y

ref <- "A 0.05" # reference class

### center/scale data
X <- scale(X, center = TRUE, scale = TRUE) # See `attributes(X)`

p1 <- qplot(X[,10], X[,12], data = as.data.frame(X), color = Y) + 
  ggtitle("UNIMANshort Original Data")
p1

### PCA model #0: original data UNIMANshort
M <- prcomp(X, center = FALSE, scale = FALSE) # `X` are already scaled
T <- M$x

p2 <- qplot(PC1, PC2, data = as.data.frame(T), color = Y) + ggtitle("Total PCA UNIMANshort: Scores")
p2

## PCA model #1: reference class
ind <- which(Y == ref)
Xr <- X[ind, ] # note: we the data are scaled, but for all three classes

Xr <- scale(Xr, center = TRUE, scale = FALSE) # we do centering (to capture PC1, 
# the major variance direction), but we do not do scaling

p3 <- qplot(Xr[,11], Xr[,12], data = as.data.frame(Xr), color = ref) + ggtitle("UNIMANshort: Class Reference")
p3

Mr <- prcomp(Xr, center = FALSE, scale = FALSE) 
Tr <- Mr$x

p4 <- qplot(PC1, PC2, data = as.data.frame(Tr), color = ref) + ggtitle("PCA to class Reference -- UNIMANshort")
p4

E <- Mr$rotation 
# centering is not necessary, as that doesn't change the directions of PCs
colnames(E) <- paste("S", 1:ncol(E), sep = "")
# closer look to `E`
E

var.projected <- apply(E, 2, function(e) sum((X %*% e)^2))
var.total <- sum(apply(X, 2, function(x) sum((x)^2)))

var.projected / var.total

# plot `E`
Et <- t(E) %*% P # `E` in space of model `M`, i.e. scores of `E`

# select just first two CPC
Et <- Et[1:2, ]

p7 + geom_segment(data = as.data.frame(Et), aes(x = 0, xend = PC1, 
                                                y = 0, yend = PC2, color = rownames(Et)), arrow = arrow())

p5 <- qplot(S1, S2, data = as.data.frame(Xr), color = ref) +
  geom_segment(data = as.data.frame(t(E)), aes(x = 0, xend =S1 , y = 0, yend = S2), arrow = arrow())
p5

# columns of `E` orthogonal?
E[, 1] %*% E[, 2]

# let's plot properly to see orthogonality of `E`
p5 + xlim(c(-3, 3)) + ylim(c(-3, 3)) + theme(legend.position = "none")

# select just one PC
E1 <- E[, 1, drop = FALSE]

###¿¿p1 + geom_segment(data = as.data.frame(t(E1)), aes(x = 0, xend = 3, y = 0, yend = 2, color = ref), arrow = arrow())

## Component Correction



Xc <- X - (X %*% E1) %*% t(E1)

p6 <- qplot(X[,10],X[,11], data = as.data.frame(Xc), color = Y)  
p6
###-----------------------------------------------------
## Let's repeat everything for `iris[, 1:4]`

X <- scale(X, center = TRUE, scale = TRUE) # See `attributes(X)`

M <- prcomp(X, center = FALSE, scale = FALSE)
T <- M$x
P <- M$rotation

p7 <- qplot(PC1, PC2, data = as.data.frame(T), color = Y) + ggtitle("PCA on Original Data")
p7

ind <- which(Y == ref)
Xr <- X[ind, ]

Xr <- scale(Xr, center = TRUE, scale = FALSE) 
Mr <- prcomp(Xr, center = FALSE, scale = FALSE) 

E <- Mr$rotation 
colnames(E) <- paste("CPC", 1:ncol(E), sep = "")

# closer look to `E`
E

var.projected <- apply(E, 2, function(e) sum((X %*% e)^2))
var.total <- sum(apply(X, 2, function(x) sum((x)^2)))

var.projected / var.total

# plot `E`
Et <- t(E) %*% P # `E` in space of model `M`, i.e. scores of `E`

# select just first two CPC
Et <- Et[1:2, ]

p7 + geom_segment(data = as.data.frame(Et), aes(x = 0, xend = PC1, 
                                                y = 0, yend = PC2, color = rownames(Et)), arrow = arrow())

# let's replot to ensure that commom components CPC1 and CPC2 are orhtogonal
p2 + xlim(c(-4, 4)) + ylim(c(-4, 4)) + theme(legend.position = "none")


# select just first PC1
E1 <- E[, 1, drop = FALSE]

Xc <- X - (X %*% E1) %*% t(E1)

Mc <- prcomp(Xc, center = TRUE, scale = TRUE)
Tc <- Mc$x

p8 <- qplot(PC1, PC2, data = as.data.frame(Tc), color = Y) + ggtitle("PCA on Corrected Data")
p8
# compare plots `p7` and `p8`. Where is better class-separaility?

## The last (not the least) step: un-scale data
Xcenter <- attr(X, "scaled:center")
Xscale <- attr(X, "scaled:scale")

if(!is.null(Xscale[1])) Xc <- sweep(Xc, 2, Xscale, "*")
if(!is.null(Xcenter[1])) Xc <- sweep(Xc, 2, Xcenter, "+")

head(Xc)