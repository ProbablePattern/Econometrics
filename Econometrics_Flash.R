#### Flash Crash Events #################################################################
user='user'; path=paste('/home/',user,'/Dropbox/Data/',sep=''); year=1993
panel=foreach(year=seq(1993,2013),.combine='rbind') %do% {
  # Load hourly statistics
  data=fread(paste(path,'TAQ/Vclock_',year,'.csv',sep=''),header=FALSE)
  setnames(data,c('DateTime','Mean','Variance','Skew','Kurtosis','Min','Max',
                'Median','Count','Ticker'))
  data[,Next.Min:=c(tail(Min,-1),NA),by='Ticker']
  data[,Next.Max:=c(tail(Max,-1),NA),by='Ticker']
  data[,I.Min:=0]; data[(Mean-(2.5758*sqrt(Variance)))>Next.Min,I.Min:=1]
  data[,I.Max:=0]; data[(Mean+(2.5758*sqrt(Variance)))<Next.Max,I.Max:=1]
  data=data[,list(DateTime,Ticker,I.Min,I.Max)]
  # Load VPIN and CDF observations
  
}