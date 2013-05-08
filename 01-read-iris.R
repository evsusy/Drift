### data
data("iris")

X <- iris[, 1:4]
Y <- iris[, 5]

### print
cat("summary Y\n")
cat(summary(Y))

### save
save(X, Y, file = "irix.XY.RData")
