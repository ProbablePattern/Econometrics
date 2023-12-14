#### Panel Econometrics #######################################################
require(plm); require(quantreg); #require(nnet); #require(MTS)
HPC=TRUE; UConn=FALSE; path='/Data/BitTorrent/';
if(Sys.info()[[1]]=="Windows" & UConn)  {
  path='P:/BitTorrent/'; registerDoParallel(2)
}
if(HPC) { 
  path='/scratch/scratch0/srr11006/Panels/'; require(data.table);
  require(doParallel); require(foreach); require(parallel);
  registerDoParallel(4)
}
#years=seq(1995,1997) # Daily1
#years=seq(1999,2001) # Daily2
years=c(2003,2006)
#years=c(seq(1993,2009),seq(2010,2013)); y=1;
#### Load Data ################################################################
foreach(y=1:length(years),.combine='rbind') %do% {
  load(file=paste(path,'Daily_Panel_',years[y],'.RData',sep=''))
  data[,Excess.Ret:=Return-RF]
  data[,Turnover:=VOL/SHROUT]; data[,Spread:=(ASK-BID)/PRC]
  data=data[!is.na(Excess.Ret),]
  data[,TERM:=UST10-UST1]; data[,DEF:=BAA-UST10]
  setkeyv(data,c("Date","Ticker")); data=unique(data); #sub=data[MnA>0,Ticker];
  data=data[!is.na(Ticker),]
#### Unexpected Informed Trading ##############################################
  data[,VPIN.R:=lm(Daily.VPIN~Daily.Market.VPIN)$residuals,by=Ticker]
  data[,CDF.R:=lm(Daily.CDF~Daily.Market.CDF)$residuals,by=Ticker]
#### Regressions ##############################################################
  m1=Excess.Ret~MRP+SMB+HML+UMD+TERM+DEF+Spread+Turnover+CDF.R+Daily.Market.CDF
  #m2=MnA~MRP+SMB+HML+UMD+TERM+DEF+Spread+Turnover+CDF.R+Daily.Market.CDF
  #m3=Predict.MnA~MRP+SMB+HML+UMD+TERM+DEF+Spread+Turnover+CDF.R+Daily.Market.CDF
  #m3a=MnA~lag(MRP)+lag(SMB)+lag(HML)+lag(UMD)+lag(TERM)+lag(DEF)+lag(Spread)+
  # lag(Turnover)+lag(CDF.R)+lag(Daily.Market.CDF)
  #model=lm(m1,data=data); summary(model)
#### Logit Model ##############################################################
  #model=glm(m2,family=binomial(link="logit"),data=data[Ticker %in% sub,])
#### Multinomial Logit Model ##################################################
  #data[,MnA1:=c(tail(MnA,-1)*2,0),by=Ticker];
  #data[,MnA2:=c(tail(MnA,-2)*3,0,0),by=Ticker];
  #data[,Predict.MnA:=MnA+MnA1]; data[Predict.MnA>2,Predict.MnA:=1];
  #data[,Predict.MnA:=Predict.MnA+MnA2]; data[Predict.MnA>3,Predict.MnA:=2];
  #data[,MnA1:=NULL]; data[,MnA2:=NULL]; data[,ID:=paste(Date,Ticker)]
  #data[,Predict.MnA:=as.factor(Predict.MnA)]
  #model=multinom(m3,data=data[Ticker %in% sub,])
  #se=summary(model)$standard.errors
  #(1-pnorm(abs(coef(model)/se), 0, 1))
  #range(model$residuals)
  #summary(predict(model,data=data[Ticker %in% sub,]))
  #summary(data[Ticker %in% sub,Predict.MnA])
#### Panel Regression #########################################################
  panel=pdata.frame(data,index=c('Ticker','Date'))
  Pmodel=pggls(m1,data=panel,model='within',effect='individual')
  #summary(model,robust = TRUE)
  #Pmodel99=pgmm(Event~(L.VPIN-L.Market.VPIN) | lag(L.VPIN-L.Market.VPIN,2),
  #  data=panel,effect="twoways",model="twosteps",transformation="ld")
  save(Pmodel,file=paste(path,'PanelReg_',years[y],'.RData',sep=''))
  rm(Pmodel,panel)
#### Quantile Regression ######################################################
  Qmodel=rq(m1,tau=c(.1,.25,.5,.75,.9),na.action=na.omit,data=data);
  #summary(Qmodel)
  save(Qmodel,file=paste(path,'QuantileReg_',years[y],'.RData',sep=''))
  rm(Qmodel)
}
#save(reg,file=paste(path,'QuantileRegs.RData',sep=''))
