# Example of multivariate correction (CPC) of drift in data
#REFERENCE: https://github.com/variani/drift-multicomp/blob/master/pca/03-ref-class.R
# @ http://doi.wiley.com/10.1002/1099-128X(200009/12)14:5/63.0.CO;2-4
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

X <- scale(X, center = TRUE, scale = TRUE) # See `attributes(X)`

M <- prcomp(X, center = FALSE, scale = FALSE)
T <- M$x
P <- M$rotation


p1 <- qplot(PC1, PC2, data = as.data.frame(T), color = Y) + ggtitle("PCA on Original Data UNIMANshort")
p1

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

p2 + geom_segment(data = as.data.frame(Et), aes(x = 0, xend = 5+PC1, 
                                                y = 0, yend =5*PC2, color = rownames(Et)), arrow = arrow())



# select just first PC1
E1 <- E[, 1, drop = FALSE]

Xc <- X - (X %*% E1) %*% t(E1)

Mc <- prcomp(Xc, center = TRUE, scale = TRUE)
Tc <- Mc$x

p3 <- qplot(PC1, PC2, data = as.data.frame(Tc), color = Y) + ggtitle("PCA on Corrected Data")
p3
# compare plots `p2` and `p3`. Where is better class-separaility?

## The last (not the least) step: un-scale data
Xcenter <- attr(X, "scaled:center")
Xscale <- attr(X, "scaled:scale")

if(!is.null(Xscale[1])) Xc <- sweep(Xc, 2, Xscale, "*")
if(!is.null(Xcenter[1])) Xc <- sweep(Xc, 2, Xcenter, "+")

head(Xc)