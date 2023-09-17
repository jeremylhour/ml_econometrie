### Optimal policy learning, application
### Used in Chapter 9
### Edited: 12/2022

######################
rm(list=ls())

library(R.matlab)
library(foreign)
library(grf)
library(policytree)
library(DiagrammeR)
library(tree)
library(sandwich)
library(ggplot2)
library(xtable)

###### data 
filename ="C:/Users/chris/Dropbox/HD Econometrics_p/"
data <- read.dta(paste0(filename,"Policy learning/16437_Data_and_Programs_Max-teebor/Code for submission/jtpa2.dta"))
data$earnings <- data$earnings
data$prevearn <- data$prevearn
head(data)

set.seed(3331)
n= dim(data)[1]
# train <- sample(1:n, floor(0.5*n))
data$bfeduca2 <- data$bfeduca*data$bfeduca
data$bfeduca3 <- data$bfeduca2*data$bfeduca
X = data[,c("prevearn","bfeduca","bfeduca2","bfeduca3")]
Y = data[,"earnings"] 

## scale variables 
Ysc = Y -mean(Y)
sc = max(abs(Ysc))
Xsc = scale(X)

W = data[,"assignmt"]

OFFSET = 0

n = length(W)
nfold = 1
foldid = sample(rep(1:nfold, floor(n/nfold)))
predicted_prop = TRUE

tauhat = rep(NA, n)
Gamma.dr = rep(NA, n)
ehat_end = rep(NA,n)

X.safe = Xsc

tauhat.out.of.fold = rep(NA, n)
mhat.out.of.fold = rep(NA, n)
ehat.out.of.fold = rep(NA, n)

# for depth 1 & 2 DR trees
policy.oob.dr.1 = rep(NA, n)
trees.dr.1 = as.list(rep(NA, nfold))
policy.oob.dr.2 = rep(NA, n)
trees.dr.2 = as.list(rep(NA, nfold))
policy.oob.dr.3 = rep(NA, n)
trees.dr.3 = as.list(rep(NA, nfold))

# for depth 1 & 2 IPW trees
policy.oob.ipw.1 = rep(NA, n)
trees.ipw.1 = as.list(rep(NA, nfold))
policy.oob.ipw.2 = rep(NA, n)
trees.ipw.2 = as.list(rep(NA, nfold))


for(fold in 1:nfold) {
  
  if(nfold==1){
    idx =  1:n 
    test = 1:n
  }else{
    idx =  which(foldid != fold & foldid > 0)
    test = which(foldid == fold)   
  }
 
  propf = regression_forest(Xsc[idx,], W[idx])
  
  if(predicted_prop){
    ehat = predict(propf, newdata=Xsc)$predictions
  }else{
    ehat = rep(2/3, length(W)) 
  }
  ehat_end[test] = ehat[test]
    
  margf = regression_forest(Xsc[idx,], Ysc[idx])

  mhat = predict(margf, newdata=Xsc )$predictions
  cf = causal_forest(Xsc[idx,], Ysc[idx], W[idx], Y.hat = mhat[idx], W.hat = ehat[idx])
  tauhat[test] = predict(cf, newdata = Xsc[test,])$predictions
  tauhat_tr = predict(cf, newdata = Xsc)$predictions

  # sum(is.na(tauhat_tr))
  # quantile(tauhat)
  mu0 =  mhat  - ehat * tauhat_tr
  mu1 =  tauhat_tr +   mu0
  
  Gamma.0 = mu0 + (1-W)/(1-ehat)*(Ysc - mu0)
  Gamma.1 = mu1 + W/ehat*(Ysc - mu1)
  
  Gamma.dr[test] = (Gamma.1-Gamma.0)[test]
  
  Gamma.dr_tr = cbind(Gamma.0[idx],  Gamma.1[idx])
  # Gamma.dr_tr = double_robust_scores(cf, compliance.score= ehat[idx])
  ################
  
  tr.dr.1 = policy_tree(X.safe[idx,], Gamma.dr_tr, depth = 1)
  po.dr.1 = predict(tr.dr.1, X.safe[test,]) - 1
  trees.dr.1[[fold]] = tr.dr.1
  policy.oob.dr.1[test] = po.dr.1
  
  print("depth 1")
  
  tr.dr.2 = policy_tree(X.safe[idx,], Gamma.dr_tr, depth = 2)
  po.dr.2 = predict(tr.dr.2, X.safe[test,]) - 1
  trees.dr.2[[fold]] = tr.dr.2
  policy.oob.dr.2[test] = po.dr.2
  
  print("depth 2")
  
  tr.dr.3 =  hybrid_policy_tree(X.safe[idx,],Gamma.dr_tr)
  po.dr.3 = predict(tr.dr.3, X.safe[test,]) - 1
  trees.dr.3[[fold]] = tr.dr.3
  policy.oob.dr.3[test] = po.dr.3
  
  print("depth 3")
}

if(!predicted_prop){
  ehat_end = rep(2/3, length(W)) 
}else{
  
}


Gamma.ipw =Ysc *(W / ehat_end - (1 - W) / (1 - ehat_end)) - OFFSET

sum(is.na(Gamma.dr))
mean(Gamma.ipw)
mean(Gamma.dr,na.rm=T)
mean(tauhat[tauhat >0],na.rm=T)

sum(tauhat>0,na.rm=T)/length(tauhat)

# Look at consistency of predictions
#

all.tree.preds.dr.1 = sapply(1:nfold, function(fold) {
  predict(trees.dr.1[[fold]], X.safe) - 1
})

all.tree.preds.dr.2 = sapply(1:nfold, function(fold) {
  predict(trees.dr.2[[fold]], X.safe) - 1
})

trees.dr.1[[4]]
trees.dr.2[[6]]


all.tree.preds.dr.3 = sapply(1:nfold, function(fold) {
  predict(trees.dr.3[[fold]], X.safe) - 1
})

all.tree.preds.ipw.1 = sapply(1:nfold, function(fold) {
  predict(trees.ipw.1[[fold]], X.safe) - 1
})

all.tree.preds.ipw.2 = sapply(1:nfold, function(fold) {
  predict(trees.ipw.2[[fold]], X.safe) - 1
})

avg.pred.dr.1 = rowMeans(all.tree.preds.dr.1)
avg.pred.dr.2 = rowMeans(all.tree.preds.dr.2)
avg.pred.dr.3 = rowMeans(all.tree.preds.dr.3)
avg.pred.ipw.1 = rowMeans(all.tree.preds.ipw.1)
avg.pred.ipw.2 = rowMeans(all.tree.preds.ipw.2)

# get_advantage = function(policy,select) {
#   mu = mean((2 * policy[select] - 1) * Gamma.dr[select])
#   se = sqrt(var((2 * policy[select] - 1) * Gamma.dr[select]) / length(Gamma.dr[select]))
#   c(point.estimate=mu, std.err=se)
# }

get_advantage = function(policy,select) {
  mu = mean((2 * policy[select] - 1) * Gamma.ipw[select])
  se = sqrt(var((2 * policy[select] - 1) * Gamma.ipw[select]) / length(Gamma.ipw[select]))
  c(point.estimate=mu, std.err=se)
}


cf.treat0 = rep(1,length(Gamma.dr)) #as.numeric(tauhat - OFFSET > 0)
cf.treat = as.numeric(tauhat- OFFSET >0 )

select = !is.na(policy.oob.dr.1)

advs = rbind(c(get_advantage(cf.treat0,select), mean(cf.treat0)),
             # c(get_advantage(policy.oob.ipw.1), mean(policy.oob.ipw.1, na.rm=T)),
             # c(get_advantage(policy.oob.ipw.2), mean(policy.oob.ipw.2, na.rm=T)),
             c(get_advantage(policy.oob.dr.1,select), mean(policy.oob.dr.1, na.rm=T)),
             c(get_advantage(policy.oob.dr.2,select), mean(policy.oob.dr.2, na.rm=T)),
             c(get_advantage(policy.oob.dr.3,select), mean(policy.oob.dr.3, na.rm=T)),
             c(get_advantage(cf.treat,select), mean(cf.treat, na.rm=T)))
advs

# 
# get_gain = function(policy,select) {
#   mu = mean(policy[select] * Gamma.dr[select])
#   se = sqrt(var(policy[select] * Gamma.dr[select]) / length(Gamma.dr[select]))
#   c(point.estimate=mu, std.err=se)
# }

get_gain = function(policy,select) {
  mu = mean(policy[select] * Gamma.ipw[select])
  se = sqrt(var(policy[select] * Gamma.ipw[select]) / length(Gamma.ipw[select]))
  c(point.estimate=mu, std.err=se)
}

gain = rbind(c(get_gain(cf.treat0,select), mean(cf.treat0)),
  # c(get_gain(policy.oob.ipw.1), mean(policy.oob.ipw.1, na.rm=T)),
  # c(get_gain(policy.oob.ipw.2), mean(policy.oob.ipw.2, na.rm=T)),
  c(get_gain(policy.oob.dr.1,select), mean(policy.oob.dr.1, na.rm=T)),
  c(get_gain(policy.oob.dr.2,select), mean(policy.oob.dr.2, na.rm=T)),
  c(get_gain(policy.oob.dr.3,select), mean(policy.oob.dr.3, na.rm=T)),
  c(get_gain(cf.treat,select), mean(cf.treat, na.rm=T)))

gain
library(xtable)
xtable(gain)

########### Plots 
x11()
plot(X[,2],X[,1], col=cf.treat+1, cex.axis=1.5, xlab="Années d'éducation", ylab = "Salaire annuel pré-programme", lwd=3, 
     cex.lab=1.5) # xlab="Years of education", ylab = "Pre-program annual earnings")
legend("topright", c("Groupe non-traité", "Groupe traité"), col = c(1, 2), pch = 19)

plot(X[,2],X[,1], col=policy.oob.dr.1+1,cex.axis=1.5, xlab="Années d'éducation", ylab = "Salaire annuel pré-programme", lwd=3, cex.lab=1.5)
legend("topright", c("Groupe non-traité", "Groupe traité"), col = c(1, 2), pch = 19)

plot(X[,2],X[,1], col=policy.oob.dr.2+1,cex.axis=1.5,xlab="Années d'éducation", ylab = "Salaire annuel pré-programme", lwd=3, cex.lab=1.5)
legend("topright", c("Groupe non-traité", "Groupe traité"), col = c(1, 2), pch = 19)

plot(X[,2],X[,1], col=policy.oob.dr.3+1, cex.lab=1.5,xlab="Années d'éducation", ylab = "Salaire annuel pré-programme", lwd=3, cex.lab=1.5)
legend("topright", c("Groupe non-traité", "Groupe traité"), col = c(1, 2), pch = 19)


















