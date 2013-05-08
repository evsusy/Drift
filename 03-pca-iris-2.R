require(graphics)


### pca

prcomp(X, scale = TRUE)

plot(prcomp(X))
summary(prcomp(X))
biplot(prcomp(X, scale = TRUE))