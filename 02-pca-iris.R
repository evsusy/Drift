### data
load("data/irix.XY.RData")

mod <- prcomp(X)

scoreplot(mod, col = Y)