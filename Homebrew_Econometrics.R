#### Homebrew Econometrics ##############################################################
library(robustbase); library(sandwich); library(lmtest); library(xtable)
user='user'; path=paste('/home/',user,'/Dropbox/',sep=''); #path='/Users/Ender/Dropbox/'
#### Load and Validate Data #############################################################
load(paste(path,'Data/Panels/ICAPM_Panel.RData',sep=''))
panel=panel[abs(CRSP.Price)>5]
panel[,MktCap:=abs(CRSP.Price)*Shares*1000]; panel=panel[MktCap>1000000]
panel[,Obs:=length(d_Int),by='CUSIP']; panel=panel[Obs>=10]
### Data Preparation ####################################################################
panel[,Size:=log(MktCap)]; panel[,Analyst:=log(NUMREC)]
panel[,Turnover:=log(CRSP.Volume/(Shares*1000))]; panel=panel[Turnover!=-Inf];
winsor1=function (x, fraction=.1) {
  if(length(fraction) != 1 || fraction < 0 || fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim<-quantile(x,probs=c(fraction,1-fraction))
  x[x<lim[1]]<-lim[1]; x[x>lim[2]]<-lim[2]; x
}
panel[,d_Int:=winsor1(d_Int*100)]; panel[,d_Mkt:=winsor1(d_Mkt*100)]
panel[,L1.d_Int:=c(NA,head(d_Int,-1)),by='CUSIP']
panel[,L2.d_Int:=c(NA,head(L1.d_Int,-1)),by='CUSIP']
panel[,L3.d_Int:=c(NA,head(L2.d_Int,-1)),by='CUSIP']
panel[,L4.d_Int:=c(NA,head(L3.d_Int,-1)),by='CUSIP']
panel[,L5.d_Int:=c(NA,head(L4.d_Int,-1)),by='CUSIP']
#### Final Cut ##########################################################################
panel=panel[complete.cases(panel[,list(Date,CUSIP,d_Int,Daily.VPIN,Size,Turnover)])]
panel=panel[,list(Date,CUSIP,d_Int,Daily.VPIN,Size,Turnover,Analyst,Ownership)]
panel[,Year:=year(Date)];
#### Fixed Effects (within) Estimator ###################################################
# Demean of each group 
regdat=foreach(cusip=unique(panel$CUSIP),.combine='rbind',.inorder=FALSE) %dopar% {
  out=panel[CUSIP==cusip,list(d_Int,Daily.VPIN,Size,Turnover,Analyst,Ownership,
    L1.d_Int,L2.d_Int,L3.d_Int,L4.d_Int,L5.d_Int)]
  out=apply(out,2,function(x) x-mean(x,na.rm=TRUE)); return(out)
}
regdat=as.data.table(regdat); regdat[,Year:=panel$Year]
#### Model Specification ################################################################  
m24=d_Int~Daily.VPIN+Size+Turnover+Analyst+Ownership+L1.d_Int+L2.d_Int+L3.d_Int+L4.d_Int+L5.d_Int-1
m2=d_Int~Daily.VPIN+Size+Turnover+Analyst+Ownership+L1.d_Int+L2.d_Int+L3.d_Int+L4.d_Int+L5.d_Int+factor(Year)-1
Models=foreach(m=c('m2')) %dopar% {
  ols=lm(get(m),data=as.data.frame(regdat)); smry=summary(ols)
  #model=lmrob(get(m),data=as.data.frame(regdat),init=list(coefficients=coef(ols),scale=summary(ols)$sigma))
  #summary(model)
  #### Error Correction #################################################################
  #hc=coeftest(model, vcov=function(x) vcovHC(x,type="HC1", method='arellano'))
  #### Double Clustered Standard Errors #################################################
  source(paste(path,'Workspace/Cluster-Robust_SE.R',sep=''))
  dc=mclx(ols,1,panel$Date,panel$CUSIP)
  #### Output to LaTex ##################################################################
  Table=rbind(dc[,c(1,3)],c(length(smry$residuals),NA),c(smry$adj.r.squared,NA))
  row.names(Table)=c(rownames(smry$coefficients),"Obs","Adj.R2"); round(Table,4)
  #print(xtable(as.data.frame(Table),digits=4),include.rownames=FALSE)
  return(Table)
}
#### Combine to LaTeX table #############################################################
LaTeX=foreach(m=length(Models),.combine='cbind') %dopar% {
  mod=Models[m]
  chunk=foreach(i=1:nrow(mod),.combine='rbind') %do% {
    rbind(mod[i,1],mod[i,2])
  }
  return(chunk)
}
print(xtable(as.data.frame(LaTeX),digits=4),include.rownames=FALSE)
