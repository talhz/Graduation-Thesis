library(InvariantCausalPrediction)
set.seed(1111)

# ICP in the presence of hidden variable

d2 = read.csv('/Users/lihaozepku/Desktop/毕业论文/code/data/data_hidden.csv')
y <- d2[, 2]
x <- d2[, 3:5]
x <- cbind(x[, 1], x[, 2], x[, 3])
h <- d2[, 6]
n <- 3000
p <- 3
Etrue <- c(rep(1, 1000), rep(2, 1000), rep(3, 1000))
icp_hidden1 <- ICP(x, y, Etrue)
print(icp_hidden1)
plot(icp_hidden1)