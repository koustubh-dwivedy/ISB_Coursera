library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
library(zoo)

start.time <- proc.time()
daily=fread(file.choose(), sep=",", nrows=-1L, header=T, na.strings="NA",stringsAsFactors=FALSE,data.table = T) #Drawdowns analysis folder
end.time <- proc.time()
cat("Elapsed; ", end.time[3]-start.time[3], "seconds.\n")


colnames(daily)=c("PERMNO","DATE","PRC")

uq=unique(daily$PERMNO)


start.time <- proc.time()
#for(z in 1:10000){

z=1
g=grep(uq[z],daily$PERMNO)

if(length(g)==1){
  t=t+1
  next
}

SP=xts(daily$PRC[t:(t+length(g)-1)],order.by=as.Date(daily$DATE[t:(t+length(g)-1)],format="%d/%m/%Y"))

DJ.roc <- ROC(SP,n=1,type="discrete")

SP500.RET=monthlyReturn(SP)
dailyDD <- findDrawdowns(SP500.RET) #WHAT DOES THIS DO????

Drawdowns <- table.Drawdowns(DJ.roc[,1],top=500) #WHAT DOES THIS DO????

for(i in 1:nrow(Drawdowns)){
  if(is.na(Drawdowns[i,1])==1){
    Drawdowns[i,1]=as.Date(daily$DATE[t+1],format="%d/%m/%Y")
  }
  if(is.na(Drawdowns[i,3])==1){
    Drawdowns[i,3]=as.Date(daily$DATE[t+length(g)-1],format="%d/%m/%Y")
  }
}

df=daily$DATE[t:(t+length(g)-1)]

DD=Drawdowns[order(as.Date(Drawdowns[,1], format="%d/%m/%Y")),]