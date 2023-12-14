#### Monthly Data #############################################################
require(nlme); path='/Data/';
load(paste(path,'Panels/Monthly_Panel.RData',sep=''))
setkey(panel,Date); panel=unique(panel); panel=panel[,list(MRP,SMB,HML,UMD,PS_VWF)]
load(paste(path,'Panels/Monthly_VPIN_Factor.RData',sep=''))
factor[,Date:=as.Date(Date)]
data=na.omit(cbind(factor[,2,with=FALSE],panel))
setnames(data,c('Spread','MRP','SMB','HML','UMD','LIQ'))
#### Robust Linear Regression #################################################

require(robustbase)
models=foreach(i=2:7) %dopar% {
  data=na.omit(cbind(factor[,i,with=FALSE],panel))
  setnames(data,c('Spread','MRP','SMB','HML','UMD','LIQ'))
  ols=lm(Spread~MRP+SMB+HML+UMD+LIQ,data=data)
  model=lmrob(Spread~MRP+SMB+HML+UMD+LIQ,data=data,
    init=list(coefficients=coef(ols),scale=summary(ols)$sigma))
  assign(paste('Model',i-1,sep=''),model)
}
require(texreg)
texreg(models,digits=4,model.names=colnames(factor[,2:7]),dcolumn=TRUE)
summary(models[[1]])
#### Check out the Data #######################################################
acf(na.omit(factor[,4,with=FALSE]))
summary(factor)
ggplot(factor,aes(Date,Tercile.Factor_EW)) + geom_line() +
  xlab('Year') + ylab('VPIN Risk Premium')

model=gls(Spread~MRP+SMB+HML+UMD+LIQ,correlation=corARMA(p=1,q=1),data=data)
summary(model)
acf(model$resid)
mean(model$resid^2)
auto.arima(model$resid)

#### ARIMA Fit ################################################################
require(forecast)
ols=lm(Spread~MRP+SMB+HML+UMD+LIQ,data=data)
summary(ols); acf(ols$resid)
mean(ols$resid^2)
auto.arima(ols$resid)
auto.arima(na.omit(factor[,2,with=FALSE]),trace=TRUE)

#### GARCH Model ##############################################################
require(rugarch)
form=ugarchspec()
form=ugarchspec(distribution.model='std',mean.model=list(include.mean=TRUE,
  external.regressors=as.matrix(data[,2:6,with=FALSE])),
  variance.model=list(model='fGARCH',submodel='GARCH',garchOrder=c(0,1)))
ugarchfit(form,data[,1,with=FALSE])

ugarchfit(ugarchspec(),ols$resid)
