##########################################
## Generate correct scam.est.list for R0 estimation
## Original scam.est.list includes a R0 scaling parameter
## That might distort the estimates
## Perkins Nature Micro 2016 methods
##########################################

##############################################################################
## Code from Perkins
##############################################################################
# load libraries and seroprevalence data and metadata
library(scam)
x = read.csv('data/seroprev_metadata.csv')
x$factor.adj = 1

# relationship between temperature and mortality
load('data/algam_85re.Rdata')
lifespan = function(Temperature,se.mult){
  dd = seq(0,120,length.out=(120*24+2))
  nwdd = data.frame(Days=dd,Temperature=rep(Temperature, (120*24+2)), Study_number=5, Feed_B=2, Feed_S=1)
  nwdd = cbind(nwdd,logDay=log(nwdd$Days+1), logTemp=log(nwdd$Temperature+1))  ## +1 avoids log(0)
  prediction = as.vector(unlist(predict(algam,newdata = (nwdd), se.fit = TRUE, type = "response")$fit))
  prediction.se = as.vector(unlist(predict(algam,newdata = (nwdd), se.fit = TRUE, type = "response")$se.fit))
  prediction = prediction + se.mult * prediction.se
  prediction = prediction[-1]
  prediction[1:24] = prediction[1:24]/prediction[1]
  prediction[which(prediction>1)] = 1
  prediction[which(prediction<=0.001)] = 0
  diffDeath = (prediction[1: (length(prediction)-1)] - prediction[2:length(prediction)])
  diffDeath = diffDeath/sum(diffDeath)
  return(pmax(0, sum(dd[2:length(prediction)]*diffDeath)))
}
fielddata = rbind(c(20, 34, 1 - 0.91))
tgrd = 0.1
lifespan.range = sapply(seq(fielddata[1],fielddata[2],tgrd), function (tr) {lifespan(tr,0)})
fieldcorxn = fielddata[3] - 1/mean(lifespan.range)
mort.fun = approxfun(seq(0,50,tgrd), sapply(seq(0,50,tgrd), function(TT) 1/(1/lifespan(TT,0)+fieldcorxn)))

# relationship between temperature and extrinsic incubation period
eip = function(T){exp(8 - .2 * T)}

# constant parameters
b = 0.4
c.r = 3.5
a = 1 / 1.5

# R0 as a function of mosquito-human ratio and temperature
R0 = function(m,T){
  g = 1 / mort.fun(T)
  m * a ^ 2 * b * c.r * exp(-g * eip(T)) / g
}
cam <- R0(0.231 * exp(-1.79 - .07/.5 * (log(23000)-10)), 28)
cam

# attack rate as a function of R0
R0.vec = seq(0,1000,.01)
AR.vec = numeric(length(R0.vec))
for(ii in 1:length(AR.vec)){
  AR.vec[ii] = 1 - optimize(f=function(S){(S-exp(-R0.vec[ii]*(1-S)))^2},interval=c(0,1))$minimum
}

AR.fun.vec = approxfun(R0.vec,AR.vec)
AR.fun = function(R0,h){
  if(R0 < 1){
    return(0)
  }
  if(R0 >= 1){
    return(AR.fun.vec(R0 ^ h))
  }
}

# R0 for a given location
R0.base = function(row){
  vecs = c(0,0)
  vecs[1]=-log(1-max(x$aegypti[row],x$albopictus[row]))
  m.fun = max(vecs)
  mean(sort(sapply(x[
    row,c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')],
    function(TT) R0(m.fun,TT)),decreasing=TRUE)[1:6])
}


# R0 needed to make attack rate consistent with seroprevalence data
R0.needed = function(row,h){
  optim(par=1,fn=function(par){abs(AR.fun(par,h)-x$seroprev[row])})$par
}

# factor change in R0 necessary to make each attack rate == seroprevalence data
h.vec = seq(.01,1,.01)
factor.needed = matrix(0,nrow(x),length(h.vec))
for(hh in 1:length(h.vec)){
  factor.needed[,hh] =
    sapply(1:nrow(x),function(rr)R0.needed(rr,h.vec[hh])) /
    sapply(1:nrow(x),function(rr)R0.base(rr))
}

###############################################
## Setup data frame for fitting scam
## Still perkins code
###############################################

df = data.frame(econ=log(x$gdp_pcppp2005),factor=log(factor.needed[,ncol(factor.needed)]))

scam.est = scam(factor~s(econ, bs='mdcx'),data=df)

plot(scam.est)
###############################################
## Bootstrap Dataframe to get 1000 scam draws
## For relating economic index to the multiplication factor
###############################################
load("data/functions_R0_AR_random_draws.RData")

set.seed(808)
scam.est.list = vector("list", 1000)

for(i in 1:1000){
  indices <- sample(x = 1:13, size = 10, replace = F)
  scam.est.list[[i]] <- scam(factor~s(econ, bs='mdcx'),data=df[indices,])
}


save(reps,rep.master,scam.est.list,mort.fun,eip.fun,a,b,c.r,h.list,file='data/functions_R0_AR_random_draws_bootstrap_scam.RData')


econ = seq(6.5,10.5,0.1)
plot(
  econ,predict(scam.est.list[[1]],newdata=data.frame(econ=econ)),
  type='l',col=rgb(0,0,0,.05),ylim=c(-1,6),xlab='',ylab='')
for(ii in 2:length(scam.est.list)){
  lines(econ,predict(scam.est.list[[ii]],newdata=data.frame(econ=econ)),col=rgb(0,0,0,.1))
}
