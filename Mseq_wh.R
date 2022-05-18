library(InvariantCausalPrediction)

set.seed(1111)
d1 = read.csv('/Users/lihaozepku/Desktop/毕业论文/code/data/data2.csv')
y <- d1[, 2]
x <- d1[, 3:4]
x <- cbind(x[, 1], x[, 2])
n <- 3000
p <- 2
E <- 1:3000
Exp1 <- sample(1:3000, 1000, replace=FALSE)
Er1 <- unique(c(E[!E %in% Exp1], E[duplicated(E)]))
Exp2 <- sample(Er1, 1000, replace=FALSE)
Exp3 <- unique(c(Er1[!Er1 %in% Exp2], Er1[duplicated(Er1)]))
E1 <- c(NA, 3000)
for(i in 1:3000){
    if(i %in% Exp1){
        E1[i] <- 1
    }
    if(i %in% Exp2){
        E1[i] <- 2
    }
    if(i %in% Exp3){
        E1[i] <- 3
    }
}
Efalse <- c(rep(1, 1500), rep(2, 1500))
Etrue <- c(rep(1, 1000), rep(2, 1000), rep(3, 1000))

icp <- ICP(x, y, Etrue)
print(icp)
plot(icp)

em <- mixtools::regmixEM(y, x)
h <- apply(em$posterior, 1, which.max)

Yh1 <- y[h==1]
Yh2 <- y[h==2]
Xh1 <- x[h==1,]
Xh2 <- x[h==2,]

seq1 <- seqICP(Xh1, Yh1)
seq2 <- seqICP(Xh2, Yh2)

summary(seq1)
summary(seq2)

