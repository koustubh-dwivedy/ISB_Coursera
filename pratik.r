setwd("D:/Backtracking")
library(quantmod)
library(xts)

#importing the data
data.CPNY<-read.csv("TATASTEELData.csv",header=TRUE)

#change format and save again, change the variable names, no sr. no. only dates.
date<-as.Date(data.CPNY$Date, format="%Y-%m-%d")
data.CPNY<-cbind(date, data.CPNY[,-1])
data.CPNY<-data.CPNY[order(data.CPNY$date),]
data.CPNY<-xts(data.CPNY[,2:7],order.by=data.CPNY[,1])
names(data.CPNY)<-paste(c("CPNY.Open","CPNY.High","CPNY.Low","CPNY.Close","CPNY.Volume","CPNY.Adjusted"))
#data.CPNY[c(1:3,nrow(data.CPNY)),]

#considering only the closing price in another variable
CPNY.prc.ret<-data.CPNY[,4]
CPNY.prc.ret[c(1:3,nrow(CPNY.prc.ret)),]

#calculating the price return in the same variable *.prc.ret
CPNY.prc.ret$CPNY.prc.ret<-Delt(CPNY.prc.ret$CPNY.Close)
CPNY.prc.ret[c(1:3,nrow(CPNY.prc.ret)),]
options(digit=7)

#calculating total returns
CPNY.ret<-data.CPNY[,6]
CPNY.ret[c(1:3,nrow(CPNY.ret)),]

CPNY.ret$CPNY.tot.ret=Delt(CPNY.ret$CPNY.Adjusted)
CPNY.ret<-CPNY.ret[,2]
CPNY.ret[c(1:3,nrow(CPNY.ret)),]

#calculating log returns
options(digits=3)
CPNY.log.ret<-data.CPNY[,6]
CPNY.log.ret$CPNY.log.ret<-diff(log(CPNY.log.ret$CPNY.Adjusted))
CPNY.log.ret[c(1:3,nrow(CPNY.log.ret)),]
CPNY.log.ret<-CPNY.log.ret[,2]
options(digits=7)

#comparing log and arithmetic returns
options(digits=3,CPNYpen=100)
tot.rets<-cbind(CPNY.ret,CPNY.log.ret)
tot.rets[c(1:3,nrow(tot.rets)),]
max(abs(tot.rets$CPNY.tot.ret-tot.rets$CPNY.log.ret),na.rm=TRUE)
min(abs(tot.rets$CPNY.tot.ret-tot.rets$CPNY.log.ret),na.rm=TRUE)
options(digits=7,CPNYpen=0)

#cumulating arithmetic returns
CPNY.acum<-CPNY.ret
CPNY.acum[1,1]<-0
CPNY.acum[c(1:3,nrow(CPNY.acum)),]

CPNY.acum$GrossRet<-1+CPNY.acum$CPNY.tot.ret
CPNY.acum[c(1:3,nrow(CPNY.acum)),]

CPNY.acum$GrossCum<-cumprod(CPNY.acum$GrossRet)
CPNY.acum[c(1:3,nrow(CPNY.acum)),]

CPNY.acum$NetCum<-CPNY.acum$GrossCum-1
CPNY.acum[c(1:3,nrow(CPNY.acum)),]

#cumulative log returns
CPNY.logcum<-CPNY.log.ret
CPNY.logcum[1,1]<-0
CPNY.logcum[c(1:3,nrow(CPNY.logcum)),]

logcumret=sum(CPNY.logcum$CPNY.log.ret)
logcumret

cumret=exp(logcumret)-1
cumret

#comparing price return and total return
CPNY.Ret<-cbind(CPNY.prc.ret,CPNY.ret)
CPNY.Ret[c(1:3,nrow(CPNY.Ret)),]

names(CPNY.Ret)<-c("Close","prc.ret","tot.ret")
CPNY.Ret[c(1:3,nrow(CPNY.Ret)),]

CPNY.Ret$prc.ret[1]<-0
CPNY.Ret$tot.ret[1]<-0
CPNY.Ret[c(1:3,nrow(CPNY.Ret)),]

CPNY.Ret$gross.prc<-1+CPNY.Ret$prc.ret
CPNY.Ret$gross.tot<-1+CPNY.Ret$tot.ret
CPNY.Ret[c(1:3,nrow(CPNY.Ret)),]

CPNY.Ret$cum.prc<-cumprod(CPNY.Ret$gross.prc)
CPNY.Ret$cum.tot<-cumprod(CPNY.Ret$gross.tot)
CPNY.Ret[c(1:3,nrow(CPNY.Ret)),]

y.range<-range(CPNY.Ret[,5:6])
y.range
plot(CPNY.Ret$cum.tot, type="l",auto.grid=FALSE,xlab="Date",ylab="Value of Investment ($)",ylim=y.range,minor.ticks=FALSE, main="CPNY Stock Performance Based On Total Returns and Price Returns December 31, 2010 - December 31, 2013")
lines(CPNY.Ret$cum.prc, type="l",lty=3)
abline(h=1,col="black")
legend("topleft",col=c("black","black"),lty=c(1,3),c("Value Based on Total Return","Value Based on Price Return"))

#WEEKLY returns
wk<-data.CPNY
wk[c(1:3,nrow(data.CPNY)),]

CPNY.weekly<-to.weekly(wk)
CPNY.weekly[c(1:3,nrow(CPNY.weekly)),]

CPNY.weekly<-CPNY.weekly[,6]
CPNY.weekly[c(1:3,nrow(CPNY.weekly)),]

CPNY.weekly$Ret<-Delt(CPNY.weekly$wk.Adjusted)
CPNY.weekly[c(1:3,nrow(CPNY.weekly)),]

CPNY.weekly<-CPNY.weekly[-1,2]
CPNY.weekly[c(1:3,nrow(CPNY.weekly)),]

CPNY.acum[c(6,11,15),]

#monthly returns
mo<-data.CPNY
mo[c(1:3,nrow(data.CPNY)),]

CPNY.monthly<-to.monthly(mo)
CPNY.monthly[c(1:3,nrow(CPNY.monthly)),]

CPNY.monthly<-CPNY.monthly[,6]
CPNY.monthly[c(1:3,nrow(CPNY.monthly)),]

CPNY.monthly$Ret<-Delt(CPNY.monthly$mo.Adjusted)
CPNY.monthly[c(1:3,nrow(CPNY.monthly)),]

CPNY.monthly<-CPNY.monthly[-1,2]
CPNY.monthly[c(1:3,nrow(CPNY.monthly)),]





