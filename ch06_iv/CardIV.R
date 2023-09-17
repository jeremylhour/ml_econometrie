### High dimension and IV, instrument Selection for Returns to schooling
### Used in Chapter 6
### Edited: 12/2022
####################################################

# library(ivpack)
rm(list=ls())
library(ivmodel)
library(hdm)
library(foreign)

data(card.data)
head(card.data)
dim(card.data)

# LWKLYWGE: Log weekly earnings 
data1 <- card.data
head(data1)
# rm(data)
data1$expersq <- data1$exper^2

############################### Using c("IQ","nearc2","nearc4") as baseline IV ###############################################
##############################################################################################################################
Result = matrix(ncol=5, nrow=3)
colnames(data0)
cont = c("KWW", "fatheduc","motheduc","exper", "expersq","smsa66","reg664","reg665","reg666","reg667","reg668","reg669","black",
           "smsa","south","momdad14","sinmom14","step14","reg661")

# cont = c("fatheduc","motheduc","exper", "expersq","smsa66","reg664","reg665","reg666","reg667","reg668","reg669","black",
#          "smsa","south","momdad14","sinmom14","step14","reg661")

iv0 = c("IQ","nearc2","nearc4")
data1 <- data1[,c("lwage","educ",iv0,cont)]
data1 <- na.omit(data1)
dim(data1)
x  = model.matrix(as.formula(paste0("~",paste0(cont,collapse="+"))),data=data1)
colnames(x)
dd=5
polys = NULL
npolys = NULL
for(i in 2:dd){
  polys <- cbind(polys,(data1$IQ-mean(data1$IQ))^i)
  npolys <- c(npolys,paste0("IQ_",i))
}
# colnames(polys) <- npolys
# data1 <- cbind(data1,polys)

for(i in 1:length(cont)){
  polys <- cbind(polys,(data1[,cont[i]]*data1$nearc2) )
  polys <- cbind(polys,(data1[,cont[i]]*data1$nearc4) )
  polys <- cbind(polys,(data1[,cont[i]]*data1$IQ) )
  npolys <- c(npolys,paste0(cont[i],"n2"),paste0(cont[i],"n4"),paste0(cont[i],"IQ")) #,paste0(cont[i],"n24") )
}

###################
colnames(polys) <- npolys
dim(data1)
dim(polys)
data1 <- cbind(data1,polys)
z  = model.matrix(as.formula(paste0("~IQ +  nearc2*IQ +  nearc4*IQ   +  nearc2  + nearc4  + ", paste0(colnames(polys),collapse=" + "))),data=data1)
z  = z[,-1]
cont <- colnames(x)
iv <- colnames(z)

dim(z)
dim(x)
# dim(d)

## Baseline 2SLS Selection
d = as.matrix(data1[,c("educ")])
y = as.matrix(data1[,"lwage"])

############ with only 3 instruments
#### OLS, TSLS, Fuller estimation
iv1_std = ivmodel(Y=y, D=d, Z=z[,1:3], X=x[,-c(1)])
ss = summary(iv1_std)
Result <- as.data.frame(Result)
Result[1,1:3] <-  c("OLS",ss$kClass$point.est[1], ss$kClass$std.err[1])
Result[2,] <-  c("IV, std",ss$kClass$point.est[2], ss$kClass$std.err[2], ss$Fuller$point.est, ss$Fuller$std.err)

#### OLS, TSLS, Fuller estimation
iv1 = ivmodel(Y=y, D=d, Z=z, X=x[,-c(1)])
ss1 = summary(iv1)
Result[3,] <-  c("IV, long",ss1$kClass$point.est[2], ss1$kClass$std.err[2], ss1$Fuller$point.est, ss1$Fuller$std.err)

############ with selection
fit.lasso.b <-rlassoIV(x=x,d=as.matrix(data1[,c("educ")]),y=as.matrix(data1[,"lwage"]),z=z, select.X=TRUE, select.Z=TRUE, post=TRUE)
summary(fit.lasso.b)
fit.lasso.b$call
confint(fit.lasso.b)
Result[4,] <- c("DML",fit.lasso.b$coef[1], fit.lasso.b $se[1], NA,NA)

colnames(Result) <- c("Method","Pt Est","Std","Fuller Pt est.","Fuller Std")
Result

