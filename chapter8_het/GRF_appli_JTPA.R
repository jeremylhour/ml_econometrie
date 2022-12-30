### Heterogeneous treatment effects, causal random forests application
### Used in Chapter 8
### Edited: 12/2022
### application of  GRF to  job training program \url{https://economics.mit.edu/faculty/angrist/data1/data/abangim02}. 
library(R.matlab)

rm(list=ls())
filename ="C:/Users/chris/Dropbox/HD Econometrics_p/Codes_2022/chapter8_het/"
data <- readMat(paste0(filename,"matlab_jtpa.mat"))
dat_m <- readMat(paste0(filename,"matlab_male.mat"))
dat_f <- readMat(paste0(filename,"matlab_fem.mat"))


gender = "female"

data <-  as.data.frame(data$jtpa)
sex <- data[,5]

if(gender=="male"){
  ### male 
  data_m <- as.data.frame(dat_m$xm)
  colnames(data_m) <- c("hsorged","black","hispanic","married","wkless13",
                        "class_tr","ojt_jsa","age2225","age2629","age3035","age3644","age4554","f2sms")
  male <- (sex==1)*(1:dim(data)[1])
}else{
  ### female
  data_m <- as.data.frame(dat_f$xf)
  colnames(data_m) <- c("hsorged","black","hispanic","married","wkless13",
                        "afdc","class_tr","ojt_jsa", "age2225","age2629","age3035","age3644",
                        "age4554","f2sms")
  male <- (sex==0)*(1:dim(data)[1])
}

head(data_m)

# male <- (sex==1)*(1:dim(data)[1])

X <- data_m
################################################################################
ym = data[male,2]
zm =  data[male,3]
dm =  data[male,4]

##########################################"
data1 = cbind(X,dm,ym)
fm <- formula(paste0("ym ~ dm + ", paste0(colnames(X),collapse=" + ")))
fit_ols <- lm(fm, data=data1)
summary(fit_ols)

library(ivreg)
data1 = cbind(X,dm,ym,zm)
fm <- formula(paste0("ym ~ dm + ", paste0(colnames(X),collapse=" + "), "| zm + " ,paste0(colnames(X),collapse=" + ")))
fit_iv <-  ivreg(fm, data=data1)
summary(fit_iv)
####################################################################


# data_m

set.seed(032010)

sel <- sample(1:dim(data_m)[1],floor(0.5*dim(data_m)[1]),replace=FALSE)
# sel <- 1:dim(data_m)[1]
X.test <- data_m[-sel,]
X.train <- data_m[sel,]

dim(X.test)

ytrain = ym[sel]
ztrain = zm[sel]
dtrain = dm[sel]
# rm(list = ls())

library(grf)
p = dim(X.train)[2]
n = dim(X.train)[1]

# X.test <- 
forest.causal = causal_forest(X.train, ytrain , dtrain, min.node.size = 10, mtry = p)
preds.causal = predict(forest.causal, X.test)$predictions

forest.iv = instrumental_forest(X.train, ytrain , dtrain, ztrain, min.node.size = 10, mtry = p)
preds.iv = predict(forest.iv, X.test)$predictions


preds.causal1 <- data.frame(preds.causal)
preds.iv1 <- data.frame(preds.iv )
preds.causal1$veg <- "causal"
preds.iv1$veg <- "iv"
colnames(preds.iv1)[1] <- "len"
colnames(preds.causal1)[1] <- "len"
vegLengths <- rbind(preds.causal1,preds.iv1)
library(ggplot2)
ggplot(vegLengths, aes(len, fill = veg)) + geom_density(alpha = 0.2)


xx = seq(min(c(preds.iv,preds.causal)), max(c(preds.iv,preds.causal)), length.out=30)
p1 <- hist(preds.iv,breaks=xx)                     # centered at 4
p2 <- hist(preds.causal,breaks=xx)                     # centered at 6
x11()
if(gender=="male"){
  plot( p1, col=rgb(0,0,1,1/4), xlim=c(-3000,8500),ylim=c(0,400), main="", xlab="Prediction de l'effet du traitement sur les salaires à 30 mois",ylab ="Fréquence")  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(-3000,8500), add=T)  # second
}else{
  plot( p1, col=rgb(0,0,1,1/4), xlim=c(-2000,6000),ylim=c(0,400), main="", xlab="Prediction de l'effet du traitement sur les salaires à 30 mois",ylab ="Fréquence")  # first histogram
  plot( p2, col=rgb(1,0,0,1/4), xlim=c(-2000,6000), add=T)  # second
}

mean(preds.iv- preds.causal)
quantile(preds.iv- preds.causal)

mean(preds.iv[preds.iv>0])
mean(preds.iv)
mean(preds.causal)


# 
# x11()
# hist(preds.causal , 100)

#
# Estimate ATE
#
ATE = average_treatment_effect(forest.causal)
paste("95% CI for the ATE:", round(ATE[1], 3),
      "+/-", round(qnorm(0.975) * ATE[2], 3))

ATE = average_treatment_effect(forest.iv)
paste("95% CI for the ATE:", round(ATE[1], 3),
      "+/-", round(qnorm(0.975) * ATE[2], 3))

quantile(preds.iv,c(0.15,0.25,0.5,0.75,0.85))


#
# Omnibus tests for heterogeneity
#

# Run best linear predictor analysis
test_calibration(forest.causal)
# test_calibration(forest.causal)

# Compare regions with high and low estimated CATEs
high_effect0 = (preds.causal  > median(preds.causal ))*seq(1:length(preds.causal ))
high_effect = high_effect0[high_effect0 >0]
high_effect0 = (preds.causal  <=median(preds.causal ))*seq(1:length(preds.causal ))
low_effect = high_effect0[high_effect0>0]
ate.high = average_treatment_effect(forest.causal , subset = high_effect)
ate.low = average_treatment_effect(forest.causal, subset = low_effect)
paste("95% CI for difference in ATE:",
      round(ate.high[1] - ate.low[1], 3), "+/-",
      round(qnorm(0.975) * sqrt(ate.high[2]^2 + ate.low[2]^2), 3))

####################################################################################################



# pardef = par(mar = c(5, 4, 4, 2) + 0.5, cex.lab = 1.5, cex.axis = 1.5, cex.sub = 1.5)
# plot(NA, NA, xlim=range(xvals), ylim=range(preds.causal,preds.iv), xlab="X", ylab="tau")
# # lines(xvals, truth, lwd = 2, col = 1)
# lines(xvals, decoy, lwd = 2, col = 1, lty = 2)
# lines(xvals, preds.causal, lwd = 2, col = 4)
# lines(xvals, preds.iv, lwd = 2, col = 2)
# legend("topleft", c("True Treat. Effect", "Raw Correlation", "Causal Forest", "IV Forest"), lty=c(1, 2, 1, 1), col=c(1, 1, 4, 2), lwd=2, cex=1.5)
# par = pardef
# dev.off()






