# library(icph)
library(seqICP)
library(mixtools)
set.seed(1234)
# load data

n <- 3000
ne <- 3
E <- rep(1:3, each = n/3)

H <- rep(1, n)
for(t in 1:n){
    H[t] <- sample(1:2, size=1, prob = c(0.5, 0.5))
}

X3 <- rnorm(n)
X1 <- c(rnorm(2000), rnorm(1000, 5, 2))
Y <- (H==1)*(1 + 2*X1 + rnorm(n)) + (H==2)*(-2 - X1 + rnorm(n))
X2 <- c(3*X1[1:1000] + X3[1:1000] + rnorm(1000), rnorm(1000), 3*X1[2001:3000] + X3[2001:3000] + rnorm(1000)) / 10

dd <- data.frame(H=H, Y=Y, X1=X1, X2=X2, X3=X3, E=E)

d <- data.frame(value = c(X1, X2, X3, Y), 
                variable = factor(rep(c("X1", "X2", "X3", "Y"), each=n),
                                  levels = c("Y", "X1", "X2", "X3")),
                H = factor(rep(H, 4)), 
                time = rep(1:n, 4), 
                E = factor(rep(E,4)))

Y <- dd$Y; X1 <- dd$X1; X2 <- dd$X2; X3 <- dd$X3; E <- dd$E; H <- dd$H

X <- cbind(X1, X2, X3)

# run EM to obtain h
em <- mixtools::regmixEM(Y, X, k=4)
h <- apply(em$posterior, 1, which.max)

# split data
Yh1 <- Y[h==1]
Yh2 <- Y[h==2]
Xh1 <- X[h==1,]
Xh2 <- X[h==2,]
Yh3 <- Y[h==3]
Xh3 <- X[h==3,]
Yh4 <- Y[h==4]
Xh4 <- X[h==4,]

# run seqICP
seq1 <- seqICP(Xh1, Yh1)
seq2 <- seqICP(Xh2, Yh2)
seq3 <- seqICP(Xh3, Yh3)
seq4 <- seqICP(Xh4, Yh4)


summary(seq1)
summary(seq2)
summary(seq3)
summary(seq4)