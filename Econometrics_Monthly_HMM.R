#### Hidden Markov Model ################################################################
#install.packages('depmixS4')
require(depmixS4); path='/Data/';
#### Data for HMM #######################################################################
load(paste(path,'Panels/Monthly Factors.RData',sep=''))
VPIN=factors[Factor=='Monthly.VPIN',1:3,with=FALSE] #Monthly VPIN
nber=fread(paste(path,'Data_Sets/NBER Recessions.csv',sep=''))
setnames(nber,c('Date','Recession')); nber[,Date:=as.yearmon(Date)]
data=merge(VPIN,nber,by='Date'); rm(VPIN,factors,nber)
VIX=fread(paste(path,'Data_Sets/WRDS VIX Jan 1993 to Jun 2014.csv',sep=''))
VIX[,Day:=as.Date(Date,'%d%b%Y')]; VIX[,Date:=as.yearmon(Day)]
VIX=VIX[,list(Monthly.VIX=mean(vix,na.rm=TRUE)), by='Date']
data=merge(data,VIX,by='Date'); rm(VIX)
#### Data for Asset Pricing Test ########################################################
load(paste(path,'Panels/Monthly_Panel.RData',sep=''))
panel[,MktCap:=abs(MktCap)]; panel=panel[MktCap>1000000]
panel=panel[!is.na(panel$Excess.Ret),]; panel=as.data.table(panel)
panel[,Dispersion:=sd(Return,na.rm=TRUE),by='Date']
data=merge(data,unique(panel[,list(Date,RF,Dispersion)]),by='Date')
#### Estimate Regimes ###################################################################
#summary(lm(Quintile.Factor_VW~Recession+Monthly.VIX+Dispersion,data=data[2:nrow(data),]))
regime.spec=depmix(response=Quintile.Factor_VW~Recession+Monthly.VIX+Dispersion+RF,
  data=data[2:nrow(data),], nstates=2); regime.model=fit(regime.spec)
#summary(regime.model) #logLik(regime.model)
#regime.model
# lower AIC and BIC is better | higher log Lik is better
# likelihood increases with number of states | inclusion of RF is a big deal
#plot(ts(posterior(regime.model)[,2], start=c(1993,1),deltat=1/12),ylab="Probability",
#     main="Posterior Probability of State 1")
states=cbind(posterior(regime.model),data[2:nrow(data),list(Date,Quintile.Factor_VW)])
#### Add States #########################################################################
panel=as.data.table(merge(states,panel,by='Date')); rm(states)
#panel[,VPIN1:=S1*Quintile.Factor_VW]; panel[,VPIN2:=S2*Quintile.Factor_VW]
panel[,VPIN1:=(state-1)*Quintile.Factor_VW]; panel[,VPIN2:=abs(state-2)*Quintile.Factor_VW]
#### Estimate Asset Pricing Equation ####################################################
require(robustbase); require(xtable); require(plm)
m1=Excess.Ret~MRP+SMB+HML+UMD+PS_VWF+VPIN1+VPIN2
m2=Excess.Ret~MRP+SMB+HML+UMD+PS_VWF+Quintile.Factor_VW
m3=Excess.Ret~MRP+SMB+HML+UMD+PS_VWF+VPIN1+VPIN2+Ticker
#ols=lm(m3,data=panel)
#model=lmrob(m3, data=panel, init=list(coefficients=coef(ols), scale=summary(ols)$sigma))
#### Panel
pdata=pdata.frame(panel,index=c("Ticker","Date"),drop.index=TRUE,row.names=TRUE)
model2=plm(m2,data=pdata,model="within",effect="individual"); #summary(model)
Table=summary(model2)$coefficients[,c("Estimate","t-value")]
row.names(Table)=c('MRP','SMB','HML','UMD','LIQ','VPIN')
print(xtable(Table,digits=c(0,2,2),display=c('d','f','f')))
