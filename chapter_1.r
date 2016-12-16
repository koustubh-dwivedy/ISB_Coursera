#	FILENAME: chapter_1.r
#	DESCRIPTION: Support code for Chapter 1 of "Analyzing Financial Data and Implementing Financial Moidels Using R; Clifford S. Ang"
#	AUTHOR: Koustubh Dwivedy, Indian School of Business
#	START DATE: 15 Dec 2016

#####################################
# Basic data importing and handling #
#####################################

# METHOD 1
	# We are using the data of AMAZON (ticker: AMZN) for demonstration
	# STEPS:
	# 1. On your web browser, ENTER the following website address: http://finance.yahoo.com
	# 2. ENTER the ticker symbol AMZN in the “Quote Lookup” or “Enter Symbol” box.
	# 3. CLICK on the Historical Prices link.
	# 4. ENTER the date range December 31, 2010 to December 31, 2013.
	# 5. DOWNLOAD this sheet onto the R working directory with the "labelAMZNYahooo.csv"
	# 6. "setwd()" in R to set working directory to the dierctory in which you have downloaded the above file

	# Setting working directory
	setwd("C:/Users/TradingLab15/Desktop/R_project")
	# Reading .csv file
	data.AMZN<-read.csv("labelAMZNYahooo.csv", header = TRUE)
	# Creating a new variable of Date type from Factor type
	date<-as.Date(data.AMZN$Date,format="%Y-%m-%d")
	# Replacing "Factor" date in data.AMZN with "Date" date
	data.AMZN<-cbind(date, data.AMZN[,-1])
	# Sorting date in chronological order
	data.AMZN<-data.AMZN[order(data.AMZN$date),]
	# Converting date.AMZN object type frdata.frame to xts (eXtensible Time Series)
	# Installing xts package
	install.packages("xts")
	library(xts)

	data.AMZN<-xts(data.AMZN[,2:7],order.by=data.AMZN[,1])
	# Renaming variable names
	names(data.AMZN)<-paste(c("AMZN.Open", "AMZN.High", "AMZN.Low", "AMZN.Close", "AMZN.Volume", "AMZN.Adjusted"))


# METHOD 2
	# Retrieving Yahoo Finance Data Directly Using getSymbols
	install.packages("quantmod")
	library(quantmod)
	data.AMZN<-getSymbols("AMZN", from="2010-12-31", to="2013-12-31", auto.assign=FALSE)


# Summary Statistics of data.AMZN
summary(data.AMZN)

#############################################
# Leaving out portions of data manipulation #
#############################################

############################################################
# Comparing Capital Gains of Multiple Securities over time #
############################################################

rm(list = ls())
data.AMZN<-getSymbols("AMZN", from="2010-12-31", to="2013-12-31", auto.assign=FALSE)
data.YHOO<-getSymbols("YHOO", from="2010-12-31", to="2013-12-31", auto.assign=FALSE)
data.IBM<-getSymbols("IBM", from="2010-12-31", to="2013-12-31", auto.assign=FALSE)
data.GSPC<-getSymbols("GSPC", from="2010-12-31", to="2013-12-31", auto.assign=FALSE)

dim(data.AMZN)
dim(data.YHOO)
dim(data.IBM)
dim(data.GSPC)

# Combine Data into One Data Object
Close.Prices<-cbind(data.AMZN$AMZN.Close, data.GSPC$GSPC.Close, data.YHOO$YHOO.Close, data.IBM$IBM.Close)

# Convert Data into a data.frame
multi.df<-cbind(index(Close.Prices), data.frame(Close.Prices))
names(multi.df)<-paste(c("date","AMZN","GSPC","YHOO","IBM"))
rownames(multi.df)<-seq(1,nrow(multi.df),1)

# Calculate NormalizedValues for Each Security
multi.df$AMZN.idx<-multi.df$AMZN/multi.df$AMZN[1]
multi.df$GSPC.idx<-multi.df$GSPC/multi.df$GSPC[1]
multi.df$YHOO.idx<-multi.df$YHOO/multi.df$YHOO[1]
multi.df$IBM.idx<-multi.df$IBM/multi.df$IBM[1]

# Plotting
y.range<-range(multi.df[,6:9])

plot(x=multi.df$date, y=multi.df$GSPC.idx, type="l", xlab="Date", ylim=y.range, ylab="Value of Investment ($)", col="black", lty=1, lwd=2, main="Value of $1 Investment in AMZN, IBM, YHOO, and the S&P 500 Index December 31, 2010 - December 31, 2013")
lines(x=multi.df$date, y=multi.df$AMZN.idx, col="black", lty=2, lwd=1)
lines(x=multi.df$date, y=multi.df$IBM.idx, col="gray", lty=2, lwd=1)
lines(x=multi.df$date, y=multi.df$YHOO.idx, col="gray", lty=1, lwd=1)
abline(h=1,lty=1,col="black")
legend("topleft", c("AMZN","IBM","YHOO","S&P 500 Index"), col=c("black","gray","gray","black"), lty=c(2,2,1,1), lwd=c(1,1,1,2))

###############################
# Technical Analysis Examples #
###############################

### Trend: Simple Moving Average Crossover


# Creating a new variable containing closing prices of AMZN
AMZN.sma<-data.AMZN[,4]
# NOTE: class(AMZN.sma): [1] "xts" "zoo"

# 50 day moving average
AMZN.sma$sma50<-rollmeanr(AMZN.sma$AMZN.Close,k=50)
# 200 day moving average
AMZN.sma$sma200<-rollmeanr(AMZN.sma$AMZN.Close,k=200)

# Selecting 2012 data
AMZN.sma2012<-subset(AMZN.sma, index(AMZN.sma)>="2012-01-01")

# Plotting
y.range<-range(AMZN.sma2012,na.rm=TRUE)

par(mfrow=c(1,1))
plot(x=index(AMZN.sma2012), xlab="Date", y=AMZN.sma2012$AMZN.Close, ylim=y.range, ylab="Price ($)", type="l", main="Amazon - Simple Moving Average January 1, 2012 - December 31, 2013")
lines(x=index(AMZN.sma2012),y=AMZN.sma2012$sma50)
lines(x=index(AMZN.sma2012),y=AMZN.sma2012$sma200,lty=2)
legend("topleft", c("Amazon Price","50-Day Moving Average","200-Day Moving Average"), lty=c(1,1,2))


### Volatility: Bollinger Bands


# Obtain Closing Prices for Amazon.com Stock
AMZN.bb<-data.AMZN[,4]

# Calculate Rolling 20-Day Mean and Standard Deviation
AMZN.bb$avg<-rollmeanr(AMZN.bb$AMZN.Close,k=20)
AMZN.bb$sd<-rollapply(AMZN.bb$AMZN.Close,width=20,FUN=sd,fill=NA)

# Subset to Only Show 2013 Data
AMZN.bb2013<-subset(AMZN.bb, index(AMZN.bb)>="2013-01-01")

# Calculate the Bollinger Bands
AMZN.bb2013$sd2up<-AMZN.bb2013$avg+2*AMZN.bb2013$sd
AMZN.bb2013$sd2down<-AMZN.bb2013$avg-2*AMZN.bb2013$sd

# Plot the Bollinger Bands
y.range<-range(AMZN.bb2013[,-3],na.rm=TRUE)

plot(x=index(AMZN.bb2013), xlab="Date", y=AMZN.bb2013$AMZN.Close, ylim=y.range, ylab="Price ($)", type="l", lwd=3, main="Amazon - Bollinger Bands (20 days, 2 deviations) January 1, 2013 - December 31, 2013")
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$avg,lty=2)
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$sd2up,col="gray40")
lines(x=index(AMZN.bb2013),y=AMZN.bb2013$sd2down,col="gray40")
legend("topleft", c("Amazon Price","20-Day Moving Average","Upper Band","Lower Band"), lty=c(1,2,1,1), lwd=c(3,1,1,1), col=c("black","black","gray40","gray40"))


### Leaving out: Momentum: Relative Strength Index