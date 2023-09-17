### High dimension and IV, panel context, application 
### Used in Chapter 7
### Edited: 12/2022
########## Data is accessible as the ``Crime" dataset in the \textit{plm} package.

rm(list=ls())

library("mnormt")
library(hdm)
library(plm)
library(Ecdat)
data("Crime", package = "plm")

head(Crime, 30)

Crime1 <- pdata.frame(Crime,index = c("county", "year"), drop.index = FALSE)

Results  <- matrix(NA,3,3)
Results  <- as.data.frame(Results)

#################### within, small number of IV and controls #########################################
cr <- plm(lcrmrte ~  lpolpc + lprbarr + lprbconv + lprbpris + lavgsen +
            ldensity + lwcon + lwtuc + lwtrd + lwfir + lwser + lwmfg + lwfed +
            lwsta + lwloc + lpctymle + lpctmin  #+ region + smsa #+ factor(year)
          | .  - lpolpc + ltaxpc + lmix,
          data = Crime1, model = "within")
ss1 <- summary(cr)
Results[1,] <- c("Within", ss1$coefficients[1], sqrt(ss1$vcov[1,1]))

######################
cont <- c("lprbarr","lprbconv","lprbpris","lavgsen","ldensity","lwcon","lwtuc","lwtrd","lwfir","lwser","lwmfg","lwfed", "lwsta","lwloc","lpctymle","lpctmin")
dep <- c("lpolpc")
iv <- c("ltaxpc","lmix")

poly <- NULL
npoly <- NULL
for(i in 1:length(cont)){
  poly <- cbind(poly, Crime1[,cont[i]]^2)
  # poly <- cbind(poly, Crime1[,cont[i]]^3)
  npoly <- c(  npoly, c(paste0(cont[i],"2")))
}

############# generate polynomials of baseline controls and IV ########################################
for(i in 1:length(cont)){
  for(j in 1:length(cont)){
  poly <- cbind(poly, Crime1[,cont[i]]* Crime1[,cont[j]])
  poly <- cbind(poly, Crime1[,cont[i]]^2* Crime1[,cont[j]]^2)
  # poly <- cbind(poly, Crime1[,cont[i]]*Crime1[,cont[j]]^2)
  # poly <- cbind(poly, Crime1[,cont[i]]^3)
  npoly <- c(  npoly, c(paste0(cont[i],cont[j])),c(paste0(cont[i],cont[j],"2")))
  }
}
colnames(poly) <-  npoly 
Crime1 <-cbind(Crime1, poly)
dim(Crime1)

polyIV <- NULL
npolyIV <- NULL
for(k in 1:length(iv)){
  for(i in 1:length(cont)){
    polyIV <- cbind(polyIV, Crime1[,cont[i]]* Crime1[,iv[k]])
    polyIV <- cbind(polyIV, Crime1[,cont[i]]^2* Crime1[,iv[k]])
    polyIV <- cbind(polyIV, Crime1[,cont[i]]^3* Crime1[,iv[k]])
    npolyIV <- c(  npolyIV, c(paste0(cont[i],iv[k],"1"),paste0(cont[i],iv[k],"2"))) # paste0(cont[i],iv[k],"3")))
  }
}

length(npolyIV)
colnames(polyIV) <-  npolyIV
Crime1 <-cbind(Crime1, polyIV)

inte <- c(cont,iv,npoly,npolyIV,dep,"lcrmrte")

form <- paste0("lcrmrte ~", paste0(c(dep,cont,npoly), collapse = "+"),"| . - lpolpc", paste0(c(iv,npolyIV), collapse = "+"))
cr <- plm(as.formula(form ),data = Crime1, model = "within")
ss <- summary(cr)
attributes(ss)
Results[2,] <- c("Within, large", ss$coefficients[1], sqrt(ss$vcov[1,1]))


head(Crime1)
Crime0 <-  pdata.frame(Crime1,index = c("county", "year"), drop.index = FALSE)
id <- unique(Crime1$county)
for(k in 1:length(inte)){
  for(i in 1:length(id)){
  Crime0[Crime0$county==id[i],inte[k]] <- Crime0[Crime0$county==id[i],inte[k]] - mean( Crime0[Crime0$county==id[i],inte[k]])
  }
}

head(Crime0)
Crime0 <- na.omit(Crime0)

ff <- as.matrix(Crime0[,c(cont,npoly)])
head(Crime0)

x = as.matrix(Crime0[,c(cont,npoly)])
dim(x)
d= as.matrix(as.vector(Crime0[,dep]))
head(x)
y = as.matrix(as.vector(Crime0[,"lcrmrte"]))
z = as.matrix(Crime0[,c(iv,npolyIV)])
dim(z)
fit.lasso <-rlassoIV(x=x,d=d,y=y,z=z, select.X=TRUE, select.Z=TRUE)
summary(fit.lasso)

################### in an explicit way ##########################################
# Z <- cbind(z, x)
# lasso.d.zx <- rlasso(Z, d)
# lasso.y.x <- rlasso(x, y)
# lasso.d.x <- rlasso(x, d)
# 
# ind.dzx <- lasso.d.zx$index
# PZ <- as.matrix(predict(lasso.d.zx))
# lasso.PZ.x <- rlasso(x, PZ)
# ind.PZx <- lasso.PZ.x$index
# 
# if (sum(ind.PZx) == 0) {
#   Dr <- d - mean(d)
# } else {
#   Dr <- d - predict(lasso.PZ.x)
# }
# 
# if (sum(lasso.y.x$index) == 0) {
#   Yr <- y - mean(y)
# } else {
#   Yr <- lasso.y.x$residuals
# }
# 
# if (sum(lasso.PZ.x$index) == 0) {
#   Zr <- PZ - mean(x)
# } else {
#   Zr <- lasso.PZ.x$residuals
# }
# 
# result <- tsls(y = Yr, d = Dr, x = NULL, z = Zr, intercept = FALSE, homoscedastic = FALSE)
# coef <- as.vector(result$coefficient)
# se <- diag(sqrt(result$vcov))


Results[3,] <- c("cluster-Lasso",fit.lasso$coefficients,fit.lasso$se)

Results
