#### Load Data ################################################################
if(Sys.info()[[1]]=="Windows") registerDoParallel(detectCores())
path='/Data/';
load(paste(path,'Panels/Monthly_Panel.RData',sep=''))
setkey(panel,Date); panel=unique(panel);
panel=panel[,list(Date,MRP,SMB,HML,UMD,PS_VWF)]; setkey(panel,Date);
load(paste(path,'Panels/Monthly_VPIN_Factor.RData',sep=''))
factor[,Date:=as.yearmon(Date)]; setkey(factor,Date);
panel=merge(panel,factor); rm(factor)
#### Add NBER Recessions ######################################################
nber=fread(paste(path,'Data Sets/NBER Recessions.csv',sep=''))
setnames(nber,c('Date','Recession')); nber[,Date:=as.yearmon(Date)]
panel=merge(panel,nber); rm(nber); original=panel
panel=original[Recession==1]
panel=original[Recession==0]
#### Robust Linear Regression #################################################
models=foreach(i=1:6,.packages='robustbase') %dopar% {
  data=panel[,c(2:6,i+6),with=FALSE]
  setnames(data,c('MRP','SMB','HML','UMD','LIQ','Spread'))
  ols=lm(Spread~MRP+SMB+HML+UMD+LIQ,data=data)
  model=lmrob(Spread~MRP+SMB+HML+UMD+LIQ,data=data,
              init=list(coefficients=coef(ols),scale=summary(ols)$sigma))
  assign(paste('Model',i-1,sep=''),model)
}
#### Robust Linear Regressions (5 Year sub-periods) ###########################
Begin=paste('Jan',seq(1993,2012,5)); End=paste('Dec',seq(1997,2013,5))
models=foreach(i=1:6,.combine='cbind',.packages='robustbase') %dopar% {
  sub=foreach(j=1:4,.combine='rbind') %do% {
    data=panel[Date %between% c(Begin[j],End[j]),c(2:6,i+6),with=FALSE]
    setnames(data,c('Spread','MRP','SMB','HML','UMD','LIQ'))
    ols=lm(Spread~MRP+SMB+HML+UMD+LIQ,data=data)
    model=lmrob(Spread~MRP+SMB+HML+UMD+LIQ, data=data,
        init=list(coefficients=coef(ols), scale=summary(ols)$sigma))
    return(summary(model)$coefficients[1,4])
  }
  return(alpha)
}
require(xtable)
print(xtable(models,digits=4),include.rownames = FALSE)
#### Robust Linear Regressions on sub-periods (entire model) ##################
require(robustbase)
security='Quintile.Factor_EW'
Begin=paste('Jan',seq(1993,2012,5)); End=paste('Dec',seq(1997,2013,5))
models=foreach(i=1:4,.packages='robustbase') %dopar% {
  data=panel[Date %between% c(Begin[i],End[i]),
    c(security,'MRP','SMB','HML','UMD','PS_VWF'),with=FALSE]
  setnames(data,c('Spread','MRP','SMB','HML','UMD','LIQ'))
  ols=lm(Spread~MRP+SMB+HML+UMD+LIQ,data=data)
  model=lmrob(Spread~MRP+SMB+HML+UMD+LIQ, data=data,
    init=list(coefficients=coef(ols), scale=summary(ols)$sigma))
  return(model)
}
#### LaTeX ####################################################################
require(texreg)
texreg(models, digits=4, model.names=colnames(factor[,7:12]), dcolumn=TRUE)
summary(models[[1]])
