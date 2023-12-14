#### Panel Econometrics #################################################################
library(plm); library(sandwich); library(lmtest); library(xtable); library(foreign)
file='Data/Stata_ICAPM.dta'
user='rush'; path=paste('/home/',user,'/Dropbox/',sep='');
if(Sys.info()[[1]]=="Darwin") path='/Users/Ender/Dropbox/'
if(Sys.info()[[1]]=="Windows") { path=paste(readWindowsShortcut(
  paste('C:/Users/',user,'/Links/Dropbox.lnk',sep=''))$pathname,sep='') }
load(paste(path,file,sep='')); # Load R data set
#panel=read.dta(file=paste(path,file,sep=''),convert.dates=TRUE) #Load Stata data set
#### Minimize Data ######################################################################
panel[,Obs:=length(d_Int),by='CUSIP']; panel=panel[Obs>=10]
#### Model Specification ################################################################
m01=d_Int~Daily.VPIN+Size+Turnover+lag(d_Int,1:5)
m02=d_Int~Daily.VPIN+Size+Turnover+Analyst+Ownership | lag(d_Int,2:3)
#### Panel Estimation ###################################################################
model=plm(get(m),panel,index=c('CUSIP','Date'),model='within',effect='individual')
#model=pggls(get(m),panel,index=c('CUSIP','Date'),model='within',effect='individual')
#model=pgmm(get(m),panel,index=c('CUSIP','Date'),model='onestep',transformation='ld',effect='twoways')
smry=summary(model,robust); smry
#### Econometric Tests ##################################################################
#sargan #Sargan/Hansen test of overidentifying restrictions
#mtest
#pbgtest(model); #pwfdtest(model,h0 = c("fd","fe"))
#### Double Cluster
install.packages('multiwayvcov')
library(multiwayvcov)
#### Error Correction ###################################################################
hc=coeftest(model, vcov=function(x) vcovHC(x,type="HC1", method='arellano'))
#### Double Clustered Standard Errors ###################################################
mclx=function(fm,dfcw,cluster1,cluster2) {
  library(sandwich); library(lmtest); cluster12=paste(cluster1,cluster2, sep="")
  M1=length(unique(cluster1)); M2=length(unique(cluster2)); M12=length(unique(cluster12))
  N=length(cluster1); K=fm$rank
  dfc1=(M1/(M1-1))*((N-1)/(N-K)); dfc2=(M2/(M2-1))*((N-1)/(N-K))
  dfc12=(M12/(M12-1))*((N-1)/(N-K));
  u1=apply(estfun(fm), 2, function(x) tapply(x, cluster1,sum))
  u2=apply(estfun(fm), 2, function(x) tapply(x, cluster2,sum))
  u12=apply(estfun(fm), 2, function(x) tapply(x, cluster12, sum))
  vc1=dfc1*sandwich(fm, meat=crossprod(u1)/N )
  vc2=dfc2*sandwich(fm, meat=crossprod(u2)/N )
  vc12=dfc12*sandwich(fm, meat=crossprod(u12)/N)
  vcovMCL=(vc1 + vc2 - vc12)*dfcw; coeftest(fm, vcovMCL)
}
dc=mclx(model,1,panel$Date,panel$CUSIP)
#### Output to LaTeX ####################################################################
if(exists('hc')) {
  Table=rbind(hc[,c(1,3)],c(length(smry$residuals),NA),c(smry$r.squared[2],NA))
} else {
  Table=rbind(smry[,c(1,3)],c(length(smry$residuals),NA),c(smry$r.squared[2],NA))
}
row.names(Table)=c(rownames(smry$coefficients),"Obs","Adj.R2"); round(Table,4)
print(xtable(as.data.frame(Table),digits=4),include.rownames=FALSE)
