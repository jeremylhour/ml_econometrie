### Heterogeneous treatment effects, illustration
### Used in Chapter 8
### Edited: 12/2022
library(hdm)

# quantile((exp(W[, 1])/(1+exp(W[, 1]))+ runif(n * p1))/2)
set.seed(1)
n = 500
p1 = 1
p2 = 800
seuil = 0.9
W = matrix(rnorm(n * p2), n, p2) # Controls
# D = matrix(1*( (exp(-W[, 1])/(1+exp(-W[, 1]))+ runif(n * p1))/2 > seuil), n, p1) # traitement
D = matrix(1*(  runif(n * p1) > seuil), n, p1) # traitement
X = cbind(D, W) # Regressors
Y = D[, 1] * 6 + W[, 1] * 1 + rnorm(n) #Outcome

sum(D)

# x11()
plot( W[D==0, 1], Y[D==0], col=2, xlim=c(-5,4), ylim=c(min(Y),max(Y)),  main="",
      xlab="", ylab="",cex.axis=2, lwd=2, pch=2)
points(W[D==1, 1], Y[D==1], col=4, lwd=3)

lasso.reg1 = rlasso(Y[D==1] ~ W[D==1,], post = FALSE)
sum.lasso <- summary(lasso.reg1, all = FALSE)

nb =200
grid = seq(min(W[,1]),max(W[, 1]), length.out=nb )
xn = cbind(rep(1,nb),grid)
fit1 = lasso.reg1$coefficients[1] + lasso.reg1$coefficients[2]*grid
lines(grid ,fit1 -6, col=4,lty=2, lwd=3)
lines(grid ,fit1 , col=4,lty=2, lwd=3)

lasso.reg0 = rlasso(Y[D==0] ~ W[D==0,], post = FALSE)
sum.lasso <- summary(lasso.reg0, all = FALSE)
fit0 = lasso.reg0$coefficients[1] + lasso.reg0$coefficients[2]*grid
lines(grid ,fit0 , col=2,lty=1, lwd=4)

legend(-5.2, 9.3, legend=c("Valeurs (X,Y) des traités", "Valeurs (X,Y) des non-traités", "Estimation et estimation décalée de E[Y|D=1,X=x]", "Estimation de E[Y|D=0,X=x]" ),
       col=c(4,2,4,2), lty=c(NA,NA,2,1), pch=c(1,2,NA,NA) ,  lwd=c(3,3,3,3) ,cex=1.3)


