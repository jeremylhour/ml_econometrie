### High dimension and IV, simulations
### Used in Chapter 6
### Edited: 12/2022
###### Simulations adapted from  Chern. Hansen. Spindler. 

### Load packages
library(ggplot2)
library(gridExtra)
library(MASS)
library(mnormt)
library(hdm)
library(AER)
library(car)
library(xtable)
# update.packages("Rcpp")


### Simulation parameters
set.seed(13571113)
p_x = 100 ## number of controls
p_z = 100 ## number of instruments 
n = 300 ## total sample size
K = 3 # nb folds

#### Splitting decision rules
split = runif(n)
cvgroup = as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = T))  

##number of MC replications
MC =2000
Results = matrix(ncol=3, nrow=MC)

out00 = NULL
out = NULL
out1 = NULL
out2 = NULL
out3=NULL
outKK1 = NULL
outKK2 = NULL
for (kk in 1:MC){
  
  ### GENERATE DATA
  means <- c(0,0,0,0)
  Sigma <- matrix(0,p_x,p_x)
  for (i in 1:p_x){
    for (j in 1:p_x){
      Sigma[i,j] <- (1/2)^{abs(i-j)}
    }
  }
  
  nu <- 4/9 + sum((1:p_x)^(-2))
  beta <- matrix(0,1,p_x)
  beta[1,1:4] <-  1/(9*nu)
  beta[1,5:p_x] <-  1/nu*(5:p_x)^(-2)
  
  delta <- matrix(3*(1:p_z)^(-2),p_z,1)
  Pi_m <- cbind(diag(1,p_z,p_z),matrix(0,p_z,(p_x-p_z)))
  
  sigmas <- matrix(0,2+p_z+p_x, 2+p_z+p_x)
  sigmas[1:2,1:2] <- matrix(c(1,0.6,0.6,1), 2,2  )
  sigmas[3:(2+p_z),3:(2+p_z)] <- diag(1,p_z,p_z)
  sigmas[(3+p_z):(2+p_z+p_x),(3+p_z):(2+p_z+p_x)] <-Sigma
  
  
  var <- rmnorm(n, mean = rep(0, nrow(sigmas)), varcov = sigmas)
  dim(var )
  eps <- var[,1]
  us <- var[,2]
  zetas <- var[,3:(2+p_z)]
  x <- var[,(3+p_z):(2+p_z+p_x)]
  gamma = beta
  tau = 1.5
  z <- Pi_m%*%t(x) + 0.125*t(zetas)
  d <- x%*%t(gamma) +  t(z)%*%delta + us
  y <- tau *d + x%*%t(beta) + 2*eps
  z <- t(z)
  
  ##############################################################################################
  ### METHOD 0bis: oracle
  zO =z%*%delta
  xO =x%*%t(beta) 
  ivfit.lasso = tsls(y=y,d=d, x=xO, z=zO)
  out3 <- rbind(out3,c(ivfit.lasso$coef[1], ivfit.lasso$se[1],ivfit.lasso$coef[1]/ivfit.lasso$se[1]))
  
  ### METHOD 1: Double-Selection, no sample-splitting
  ## Do LASSO of D on X to obtain gamma
  W= cbind(z,x)
  rD_xz = rlasso(d ~ W)
  ind.dzx <- rD_xz$index
  
  ## Do LASSO of Y on X to obtain theta, and extract residuals
  rY_x = rlasso(y ~ x)
  rY = rY_x$residuals
  
  
  # if(build == FALSE){
  ## Build D_hat from estimated gamma and delta
  ### compute the projection of d on vect(W[selected covariates using lasso])
  PZ <-  W[, ind.dzx] %*% MASS::ginv(t( W[, ind.dzx]) %*%  W[, ind.dzx]) %*%  t(W[, ind.dzx]) %*% d
  ## do LASSO of this predicted d using these covariates on x (d_hat on X) to get nu
  rPZ.x <- rlasso(x, PZ)
  ind.PZx <- rPZ.x$index
  
  ## extract the residuals of the lasso of d_hat on X
  if (sum(ind.PZx) == 0) {
    Dr <- d - mean(d)
  } else {
    Dr <- d - x[,ind.PZx]%*%MASS::ginv(t(x[,ind.PZx])%*%x[,ind.PZx])%*%t(x[,ind.PZx])%*%PZ
  }
  
  ## extract the residuals of the lasso of Y on X 
  if (sum(rY_x$index) == 0) {
    Yr <- y - mean(y)
  } else {
    Yr <- rY
  }
  
  ## extract the residuals of the lasso of the projection of  Y on X 
  if (sum(rPZ.x$index) == 0) {
    Zr <- PZ - mean(x)
  } else {
    Zr <- rPZ.x$residuals
  }
  
  ## Do TSLS of the residuals of Y/X on residuals of D/X using residuals of Dhat/X as instruments
  ivfit.lasso <-  tsls(y = Yr, d = Dr, x = NULL, z = Zr, intercept = FALSE)
  # coef <- as.vector( ivfit.lasso$coefficient)
  out <- rbind(out,c(ivfit.lasso$coef[1], ivfit.lasso$se[1],ivfit.lasso$coef[1]/ivfit.lasso$se[1]))
  
  # }else{
  #   ### Build in function to do all this.... 
  ivfit.lasso2 = rlassoIV(y ~ x + d | x + z, select.X=TRUE, select.Z=TRUE)
  out00 <- rbind(out00,c(ivfit.lasso2$coef, ivfit.lasso2$se,ivfit.lasso2$coef/ivfit.lasso2$se))
  # }
  # 
  
  ### METHOD 0: selection, alternative (Non-orthogonal)
  ## select all the controls selected by the two Lasso
  sel = (abs(rD_xz$coefficients[(2+dim(z)[2]):(1+dim(x)[2]+dim(z)[2])])> 10^(-6))*1 + (rY_x$coefficients[2:(dim(x)[2]+1)]> 10^(-6))*1
  sel[sel ==2] <- 1 
  sel_z = (rD_xz$coefficients[2:(dim(z)[2])] > 10^(-6))*1 
  ## Do TSLS 
  x_sel = x[,sel==1]
  z_sel = z[,sel_z==1]
  if(sum(sel)>0 & sum(sel_z)>0){
    ivfit.lm = ivreg(y ~ d  + x_sel| z_sel + x_sel)
  }else if (sum(sel)==0 & sum(sel_z)>0){
    ivfit.lm = ivreg(y ~ d  | z_sel)
  }
  se <-  coef(summary(ivfit.lm))[2, "Std. Error"]
  out1 <- rbind(out1,c(ivfit.lm$coef["d"],  se ,ivfit.lm$coef["d"]/se))
  
  
  ### METHOD 2: Double Selection with Sample Splitting
  outK = matrix(ncol=3, nrow=K)
  # k=1
  for(k in 1:K){
    Ik = cvgroup==k # Separate the sample
    NIk = cvgroup!=k
    ind <- matrix(1,dim(d)[1],1)
    ind_x <- matrix(1,dim(x[Ik,])[1],1)
    
    ## Do LASSO of D on X to obtain gamma
    W= cbind(z,x)
    rD_xz = rlasso(d[NIk,] ~   W[NIk,] )
    ind.dzx <- rD_xz$index
    ## Do LASSO of Y on X to obtain theta, and extract residuals
    rY_x = rlasso(y[NIk,] ~ x[NIk,])
    ind.Y_x <- rY_x$index
    ## Build D_hat from estimated gamma and delta
    PZ <-  W[, ind.dzx] %*% MASS::ginv(t( W[, ind.dzx]) %*%  W[, ind.dzx]) %*%  t(W[, ind.dzx]) %*% d
    ## regress d_hat on X to get nu
    rPZ.x <- rlasso(x[NIk,], PZ[NIk,])
    ind.PZx <- rPZ.x$index
    
    ## extract the residuals of the lasso of d_hat on X
    if (sum(ind.PZx) == 0) {
      Dr <- d[Ik,] - mean(d[Ik,])
    } else {
      # Dr <- d[Ik,] - predict(rPZ.x, newdata=x[Ik,])
      Dr <- d[Ik,] - x[Ik,   ind.PZx] %*% (MASS::ginv(t( x[NIk,   ind.PZx]) %*%  x[NIk,   ind.PZx]) %*%  t(x[NIk,   ind.PZx]) %*% d[NIk,])
    }
    
    ## extract the residuals of the lasso of Y on X 
    if (sum(rY_x$index) == 0) {
      Yr <- y[Ik,] - mean(y[Ik,])
    } else {
      # Yr <-  y[Ik,] - predict(rY_x, newdata=x[Ik,])
      Yr <-  y[Ik,] - x[Ik,   ind.Y_x] %*% (MASS::ginv(t( x[NIk,   ind.Y_x]) %*%  x[NIk,   ind.Y_x]) %*%  t(x[NIk,   ind.Y_x]) %*% y[NIk,])
      
    }
    
    ## extract the residuals of the lasso of the projection of  Y on W on X
    if (sum(rPZ.x$index) == 0) {
      Zr <- PZ[Ik,] - mean(x[Ik,])
    } else {
      # Zr <-  PZ[Ik,] -predict(rPZ.x, newdata=x[Ik,])
      Zr <-  PZ[Ik,] - x[Ik,   ind.PZx] %*% (MASS::ginv(t( x[NIk,   ind.PZx]) %*%  x[NIk,   ind.PZx]) %*%  t(x[NIk,   ind.PZx]) %*% PZ[NIk,])
      
    }
    
    ## Do TSLS 
    ivfit.lasso<-   tsls(y = Yr, d = Dr, x = NULL, z = Zr, intercept = FALSE)
    outK[k,] <- c(ivfit.lasso$coef[1], ivfit.lasso$se[1],ivfit.lasso$coef[1]/ivfit.lasso$se[1])
  }
  outK1 <- outK
  coef1 <- median( outK1[,1])
  outK1[,2] <- outK1[,2] +(  outK1[,1] -   coef1  )^2
  outKK1 <-  rbind(outKK1,c(coef1, median(  outK1[,2]),coef1/median(  outK1[,2])))
  
  outK1 <- outK
  coef1 <- mean( outK1[,1])
  outK1[,2] <- outK1[,2] +(  outK1[,1] -   coef1  )^2
  outKK2 <-  rbind(outKK2,c(coef1, mean(  outK1[,2]),coef1/mean(  outK1[,2])))
  
  cat(paste0("iteration", kk , "\n"))
}


# hist(out[,3]-tau/out[,2],100, prob=T, col=4)
# xseq = seq(-3,3,length.out=100)
# lines(xseq,dnorm(xseq),col=2, lwd=2)
# 
# hist(out1[,3]-tau/out1[,2],100, prob=T, col=4)
# xseq = seq(-3,3,length.out=100)
# lines(xseq,dnorm(xseq),col=2, lwd=2)
# 
# hist(out3[,3]-tau/out3[,2],100, prob=T, col=4)
# xseq = seq(-3,3,length.out=100)
# lines(xseq,dnorm(xseq),col=2, lwd=2)

MC = dim(outKK2)[1]

Results = matrix(ncol=5, nrow=MC)
Results[,2] = out[1:MC,1]- tau
Results[,1] = out1[1:MC,1]- tau
Results[,3] = out3[1:MC,1]- tau
Results[,4] = outKK1[1:MC,1]- tau
Results[,5] = outKK2[1:MC,1]- tau

### COMPUTE BIAS AND RMSE
StatDisplay = data.frame()
StatDisplay[1:5,"bias"] = apply(Results,2,mean)
StatDisplay[1:5,"RMSE"] = sqrt(apply((Results)^2,2,mean))
StatDisplay[1:5,"MAD"] = sqrt(apply((Results)^2,2,median))
row.names(StatDisplay) = c("Naive","Immunized","Oracle", "Cross-fitted med.","Cross-fitted mean.")
print(StatDisplay)


##############################################################################################
xtable(StatDisplay)

Results[,1] = out1[1:dim(outKK2)[1],3] - tau /out1[1:dim(outKK2)[1],2]
Results[,2] = out00[1:dim(outKK2)[1],3]- tau/out00[1:dim(outKK2)[1],2]
Results[,3] = out3[1:dim(outKK2)[1],3]- tau/out3[1:dim(outKK2)[1],2]
Results[,4] = outKK1[1:dim(outKK2)[1],3]- tau/outKK1[1:dim(outKK2)[1],2]
Results[,5] = outKK2[1:dim(outKK2)[1],3]- tau/outKK2[1:dim(outKK2)[1],2]


Results_s <- Results
### DRAW CHARTS
id = c(mapply(function(x) rep(x,MC),1:5))
val = c(Results)
data_res = data.frame(val = val, model = id)
length(id)

# M = max(abs(quantile(Results,.05,na.rm=T)),abs(quantile(Results,.95,na.rm=T)))
# lb = -4; ub = 4
M = max(abs(quantile(Results[,1:3],.01,na.rm=T)),abs(quantile(Results[,1:3],.99,na.rm=T)))
lb = -1.3*M; ub = 1.3*M

get.plot <- function(data,modelS,title="A Title",s){
  plot_res <- ggplot(subset(data, (model==modelS)), aes(x=val)) + 
    geom_histogram(binwidth = .2, alpha=.5, position='identity',fill="steelblue", aes(y = ..density..)) +
    scale_x_continuous(limits=c(lb,ub), name="Treatment effect") +
    ggtitle(title) + 
    stat_function(fun = dnorm, args=list(mean=0, sd=s), colour="darkorchid3", size=1) +
    theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position="none")
  return(plot_res)
} # plot func

x11()
# pdf("plots/Immunized.pdf",width=14,height=4)
# grid.arrange(get.plot(data_res,1,"Naive", sd(Results[,1])), get.plot(data_res,2,"Immunized",sd(Results[,2])), get.plot(data_res,3,"Oracle",sd(Results[,3])), ncol=3)
# grid.arrange(get.plot(data_res,4,"Cross-fitted med.", sd(Results[,4])),get.plot(data_res,5,"Cross-fitted mean",  sd(Results[,5])), ncol=2)

grid.arrange(get.plot(data_res,1,"Naive", 1), get.plot(data_res,2,"Immunized", 1), get.plot(data_res,3,"Oracle", 1), ncol=3)
# grid.arrange(get.plot(data_res,4,"Cross-fitted med.",  1),get.plot(data_res,5,"Cross-fitted mean",   1), ncol=2)

###### ##### 


























