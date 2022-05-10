setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# devtools::install_github("runesen/icph/code")
library(icph)
library(ggplot2)
library(reshape2)
library(gridExtra)
theme_set(theme_bw())

set.seed(18)

n <- 3000
ne <- 3
E <- 1:3000
Exp1 <- sample(1:3000, 1000, replace=FALSE)
Er1 <- unique(c(E[!E %in% Exp1], E[duplicated(E)]))
Exp2 <- sample(Er1, 1000, replace=FALSE)
Exp3 <- unique(c(Er1[!Er1 %in% Exp2], Er1[duplicated(Er1)]))
for(i in 1:3000){
    if(i %in% Exp1){
        E[i] <- 1
    }
    if(i %in% Exp2){
        E[i] <- 2
    }
    if(i %in% Exp3){
        E[i] <- 3
    }
}


# one can choose whether to use Gamma
Gamma <- list(matrix(c(.98, .02,
                       .1, .9), 2,2,byrow=T),
              matrix(c(.9, .1,
                       .1, .9),2,2, byrow=T),
              matrix(c(.9, .1,
                       .02, .98),2,2, byrow=T)) 

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

switches <- which(H[-1]-H[-n] !=0)

rects <- data.frame(xstart = c(1,switches), xend = c(switches,n), 
                    col = factor(rep(c(1:2), n)[1:(length(switches)+1)]))

## 200 data points rolling window

step <- 200
preds <- c("X1", "X2", "X3")
beta <- c(sapply(preds, function(p){
    
    d <- data.frame(Y = dd$Y, X = dd[,p], H = dd$H)
    
    beta <- sapply((step/2+1):(n-step/2), function(i){
        #tryCatch({
        LM <- lm(Y ~ X*H, data = d[(i-step/2):(i+step/2),])
        coefficients(LM)[2] + d$H[i]*coefficients(LM)[4]
        #}, error = function(e) NA)
    })
    beta
}
))

dbeta <- data.frame(time = (step/2+1):(n-step/2), 
                    beta = beta, 
                    variable = rep(1:3, each = (n-step)),
                    H = dd$H[(step+1):n], 
                    col = rep(c("1","2","1"), each=n-step))


t <- test.equality.sr(Y, X1, E, plot = FALSE)

ne <- length(unique(E))
K <- 2

plot.frame <- data.frame(y = Y, x = X1, hhat = factor(t$hhat), e = E)

mu    <- c(sapply(1:ne, function(i) c(sapply(1:2, function(j) subset(t$estimates, parameter == "intercept" & environment == i & component == j)$estimate))))
beta  <- c(sapply(1:ne, function(i) c(sapply(1:2, function(j) subset(t$estimates, parameter == "beta" & environment == i & component == j)$estimate))))

linedata <- data.frame(e=rep(1:ne,each=K), 
                       component = factor(rep(1:K, ne)), intercept=mu, slope=beta)

plot.frame$e <- factor(plot.frame$e, labels = c("environment 1", "environment 2", "environment 3"))
linedata$e <- factor(linedata$e, labels = c("environment 1", "environment 2", "environment 3"))

pdf("wrong_env_hidden.pdf", width = 12, height = 4.3)
print(
    ggplot(plot.frame, aes(x, y)) + 
        geom_abline(data = linedata, aes(intercept=intercept, slope=slope, lty = component)) + 
        geom_point(aes(shape = hhat), alpha = 0.7, size = 2.5) + 
        facet_grid(.~e) + 
        xlab(expression(X^1)) + ylab("Y") + 
        scale_shape_manual(values = c(16, 2)) + 
        scale_linetype_manual(values = c("twodash", "dashed")) + 
        theme(legend.position = "none", 
              axis.text = element_text(size = 16),
              strip.text = element_text(size = 20),
              axis.title = element_text(size = 16))
)
dev.off()