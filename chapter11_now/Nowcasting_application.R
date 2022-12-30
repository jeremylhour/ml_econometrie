### Nowcasting and HD, applications
### Used in Chapter 10
### Edited: 12/2022
path = "C:/Users/chris/Dropbox/HD Econometrics_p/Macroecon/Code/News/"

path2 = paste0(path,"Monthly_Topic_Attention_Theta.csv")
dat2 <- read.csv(path2,sep=",")
head(dat2)
colnames(dat2)
names0 <- colnames(dat2)

libraries = c("midasml","tsapp", "hdm", "pracma", "np", "sandwich", "flare")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

###########################################################################
data(us_rgdp)
attributes(us_rgdp)
rgdp <- us_rgdp$rgdp
cfnai <- us_rgdp$cfnai
# cfnai[,2] <- c(0,diff(cfnai[,2])/cfnai[c(-1),2]*100)
ads <- us_rgdp$ads
# ads[,2] <- c(0,diff(ads[,2])/ads[c(-1),2]*100)
payems <- us_rgdp$payems
payems[-1, 2] <- log(payems[-1, 2]/payems[-dim(payems)[1], 2])*100
payems <- payems[-1, ]
rgdp[-1, 2] <- ((rgdp[-1, 2]/rgdp[-dim(rgdp)[1], 2])^4-1)*100
rgdp <- rgdp[-1, ]

est.start <- as.Date("1990-03-01")
est.end <- as.Date("2002-03-01")

### low frequency vector rgdp[,2]
### high frequency one payems[,2]
# mix = vector(ls())
mix <- vector("list")

mix[["payems"]] <- mixed_freq_data(rgdp[,2], as.Date(rgdp[,1]), payems[,2],
                       as.Date(payems[,1]), x.lag = 9, y.lag = 4, horizon = 1,
                       est.start, est.end, disp.flag = TRUE)

mix[["cfnai"]] <- mixed_freq_data(rgdp[,2], as.Date(rgdp[,1]), cfnai[,2],
                            as.Date(cfnai[,1]), x.lag = 9, y.lag = 4, horizon = 1,
                            est.start, est.end, disp.flag = TRUE)

mix[["ads"]] <- mixed_freq_data(rgdp[,2], as.Date(rgdp[,1]), ads[,2],
                            as.Date(ads[,1]), x.lag = 9, y.lag = 4, horizon = 1,
                            est.start, est.end, disp.flag = TRUE)

names0[selection]
# selection <- c(1,2,6,7,10,11,12,13,16,20,23,24,32,37,38,40,42,44,45,53,54,66,71,73,76,77,78,79,80,85,87,88:90,116:118,151:156,162,181)
# selection <- c(1,6,7,16,23,38,181,71,73,40,77,119,142,145,147,167,170,176)
# selection <- c(1,6,7,16,38,181,71,73,2,11,17,40,77,119,142,145,147,167,170,176)
selection <-  c(1,43,37,20,2,42,15,35,6,7,16,38,181,71,73,11,17,40,77,119,142,145,147,167,170,176)
# selection <- c(1,2)
colnames(dat2)
selection1 <-selection 

dat2 <- dat2[,selection]
dd= length(selection)-1
dd
for(j in 2:dd){
  # generate 1/quarter, same frequency of GDP.
  mix[[j+4]] <- mixed_freq_data(rgdp[,2], as.Date(rgdp[,1]), dat2[,j],
                                  as.Date(dat2[,1]), x.lag = 1, y.lag = 4, horizon = 1,
                                  est.start, est.end, disp.flag = TRUE)
}


scale2 <- function(myVar){
  return((myVar - mean(myVar)) / sd(myVar))
}

###################################################################################################################
##
lim=61
ref<- mix[[1]]
y=ref$est.y
xmat =  cbind(mix[["payems"]]$est.x,mix[["cfnai"]]$est.x,mix[["ads"]]$est.x)
# xtilde <- apply(xmat,2,scale2)
for(j in 2:dd){
  # generate 1/quarter, same frequency of GDP.
  xmat = cbind(xmat,mix[[j+4]]$est.x)
}
dim(xmat)
# xtilde <- xmat
xtilde <- apply(xmat,2,scale2)
x=cbind(ref$est.lag.y,xtilde)



#############################
xout  =  cbind(mix[["payems"]]$out.x[1:lim,],mix[["cfnai"]]$out.x[1:lim,],mix[["ads"]]$out.x[1:lim,])
xtilde_out <- apply(xout,2,scale2)
for(j in 2:dd){
  # generate 1/quarter, same frequency of GDP.
  xout = cbind(xout,mix[[j+4]]$out.x[1:lim,])
}
dim(xtilde_out)
# xtilde_out <- xout
xtilde_out <- apply(xout,2,scale2)
xo=cbind(ref$out.lag.y[1:lim,],xtilde_out)


# gindex=NULL
# for(j in 1:(dd+3)){
#   gindex = c(gindex,rep(j,4)) 
# }

gindex=c(1,2,3,4)
gindex = c(gindex,rep(5,9*3)) 

dim(x)
length(gindex)
sum(is.na(x))
gamma = 0.8
ll <- cv.sglfit(x =x[,1:31], y =y ,gamma = gamma, gindex = gindex)
attributes(ll)
ll$lamin$lambda.min
# fitsgl <- sglfit(x =x[,1:31], y = y, lambda= ll$lamin$lambda.1se,gamma = gamma, gindex = gindex)
fitsgl <- sglfit(x =x[,1:31], y = y, lambda= ll$lamin$lambda.min,gamma = gamma, gindex = gindex)
fitsgl$beta
sum(fitsgl$beta!=0)
attributes(fitsgl)
fitsgl$lambda

forecasts <- predict(fitsgl, newx=xo[,1:31])
mean(abs(forecasts-ref$out.y[1:lim])^2)

####### prevision using news data #####################################################################################

# gindex=c(1,2,3,4)
# gindex = c(gindex,rep(5,9*3))
# gindex = c(gindex,rep(6,3))
# gindex = c(gindex,rep(7,2))
# gindex = c(gindex,rep(8,2))
# # gindex = c(gindex,rep(6,3))
# for(j in 1:(dd-6)){
#   gindex = c(gindex,rep(j+8,1))
# }

gindex=rep(1,4)
gindex = c(gindex,rep(2,9))
gindex = c(gindex,rep(3,9))
gindex = c(gindex,rep(4,9))

gindex = c(gindex,rep(5,3))
gindex = c(gindex,rep(6,2))
gindex = c(gindex,rep(7,2))
gindex = c(gindex,rep(8,2))
# gindex = c(gindex,rep(6,3))
for(j in 1:(dd-10)){
  gindex = c(gindex,rep(j+9,1))
}
###############################################


gamma = 0.2

dim(x)
select = 40

ll_text <- cv.sglfit(x =x[,1:select], y =y ,gamma = gamma, gindex = gindex[1:select])
attributes(ll_text)
ll$lamin$lambda.min
# fitsgl <- sglfit(x =x[,1:31], y = y, lambda= ll$lamin$lambda.1se,gamma = gamma, gindex = gindex)
fitsgl_text <- sglfit(x =x[,1:select], y = y, lambda= ll$lamin$lambda.min,gamma = gamma, gindex = gindex[1:select])
sum(fitsgl$beta!=0)
attributes(fitsgl)
fitsgl$lambda

forecasts_text <- predict(fitsgl_text, newx=xo[,1:select])
mean(abs(forecasts_text-ref$out.y[1:lim])^2)

fitsgl_text$beta

length(gindex)
########################################################################################################################
p=1
K=dim(xo[,1:31])[2]
## Gaussian choice penalty
lambda.ga = 2*1.1*qnorm(1-0.1/(2*K*p))*sqrt(T)
fit_hdm.joint2 =  rlasso(x[,1:31],y,penalty = list(homoscedastic = "none", lambda.start = lambda.ga))
sum(fit_hdm.joint2$beta!=0)
# ## Multiplier Bootstrap procedure
 bn =  10 # size of blocks
#   
bwNeweyWest = 0.75*(T^(1/3))
reg.lasso.hac2 <- rlassoHAC(x[,1:31], y,"Bartlett", bands=bwNeweyWest, bns=bn, nboot=5000,
                              X.dependent.lambda = TRUE, c=2.7)

T=dim(x)[1]
lambda.ga = 2*1.1*qnorm(1-0.1/(2*K*p))*sqrt(T)
fit_hdm.joint2 =  rlasso(x[,1:31],y,penalty = list(homoscedastic = "none", lambda.start = lambda.ga))

forecasts_c <- predict(reg.lasso.hac2,newdata=xo[,1:31])
forecasts_gauss <- predict(fit_hdm.joint2,newdata=xo[,1:31])

# ref$est.ydate

ar1 <- ar(ref$est.y)
forecast_ar1=NULL
for(j in 1:length(ref$out.lag.y[1:lim,1])){
  forecast_ar1 <- c(forecast_ar1,predict(ar1,newdata=ref$out.lag.y[j,1], n.ahead = 1)$pred[1])
}

x11()
plot(ref$out.ydate[1:lim], ref$out.y[1:lim], type="l", lwd=3, lty=2, xlab="Temps",ylab="(Prediction de) croissance du PIB", cex.axis=1.7,cex.lab=1.5)
lines(ref$out.ydate[1:lim],forecasts,col=2, lwd=3, lty=3)
# lines(ref$out.ydate[1:lim],forecasts_c,col=3, lwd=3, lty=4)
# lines(ref$out.ydate,forecasts_gauss,col=4, lwd=2)
lines(ref$out.ydate[1:lim],forecast_ar1,col=4, lwd=3, lty=5)
lines(ref$out.ydate[1:lim],forecasts_text,col=5, lwd=3)
abline(h=0, lty=2)
# legend(as.Date("2011-09-01"), -3, legend=c("Croissance du PIB", "Prédiction sg-LASSO","Prédiction sg-LASSO + texte","Prédiction boot-LASSO","Prédiction AR(1)"),
       # col=c(1,2,5,3,4), lty=c(2,5,1,3,4), cex=1.5, lwd=c(4,4,4,4,4))
legend(as.Date("2011-09-01"), -3, legend=c("Croissance du PIB", "Prédiction sg-LASSO","Prédiction sg-LASSO + texte","Prédiction AR(1)"),
       col=c(1,2,5,4), lty=c(2,5,1,4), cex=1.5, lwd=c(4,4,4,4))

################################################################################""
mean(abs(forecasts-ref$out.y[1:lim])^2)
mean(abs(forecasts_text-ref$out.y[1:lim])^2)
mean(abs(forecasts_c-ref$out.y[1:lim])^2)
# mean(abs(forecasts_gauss-ref$out.y[1:lim])^2)
mean(abs(forecast_ar1-ref$out.y[1:lim])^2)

########## Granger test 
# gindex = c(rep(1,9),rep(2,9),rep(3,dim( mix$est.lag.y)[2]))
yo =ref$out.y[1:lim]
TT =dim(xo)[1]
# p=length(gindex)
p=40
gindex[1:p]
length(y)

betahat = matrix(fitsgl_text$beta,p,1)
# lambda.1se
# ss$sgl.fit
tt <- thetafit(x[,1:p], parallel = FALSE, ncores = getOption("mc.cores", NULL),
               intercept = FALSE, K = 20, l = 5, seed = NULL, verbose = FALSE,
               registerpar = TRUE)
attributes(tt)
tt$thetahat

parzen <- function(x){
  res=0
  if(abs(x)<=1/2 & abs(x)>=0){
    res=1 - 6*x^2 + 6*abs(x)^3
  }else if(abs(x)<=1 & abs(x)>=1/2){
    res=2*(1-abs(x))^3
  }
  return(res)
}
###############################################
betahat
dim(xo)
MT = 1.3*(TT/log(p))^(1/3)


BG = tt$thetahat%*%t(xo[,1:p])%*%(yo - xo[,1:p]%*%betahat)/TT

names0[selection1][2:10]
selection1[35]
betahat[32:40]

# "Credit.ratings","M.A", "Short.sales" ,   "Problems" 
### compute t. of longterm variance .
### compute t. of longterm variance .
# groups "Banks"
selection=32:34

# groups "Growth"
selection=35:36

# groupe "Crisis"
selection=37:38

# ensemble des news
selection=32:40

GG= length(selection)
IG<- diag(1,GG,GG)
Theta = tt$thetahat[selection,selection]

U = (yo - xo[,1:p]%*%betahat)
res = vector("list")
for(k in 0:(TT-1)){
  res[[k+1]] = matrix(0,GG,GG)
  for(t in 1:(TT-k)){
    ss = Theta%*%(xo[t,selection]%*%t(xo[t+k,selection]))%*%t(Theta)
    res[[k+1]] = res[[k+1]]+ U[t]*U[t+k]*ss
  }
  res[[k+1]] =  res[[k+1]]/TT
}

xi = matrix(0,GG,GG)
for(k in 0:(TT-1)){
  xi = xi + parzen(k/MT)*res[[k+1]]
}
for(k in 1:(TT-1)){
  xi = xi + parzen(-k/MT)*t(res[[k+1]])
}
 
dim(tt$thetahat)
dim(t(xo[k:TT,1:p])%*%xo[1:(TT-k+1),1:p])
library(MASS)
cc = ginv(xi)
left = (betahat + BG)[selection]
stat = TT*t(left)%*%cc%*%left
crit = qchisq(0.95, df=GG, ncp = 0, lower.tail = TRUE, log.p = FALSE)
pval = 1- pchisq(stat, df=GG, ncp = 0, lower.tail = TRUE, log.p = FALSE)


c(stat,crit,pval)








