#	FILENAME: code.r
#	DESCRIPTION: Code for importing Financial Statement data from Google Finance to R environment
#	AUTHOR: Koustubh Dwivedy
#	START DATE: 19 Dec 2016

rm(list=ls())

##############
# USER SPACE #
##############
# tick: Ticker of stock
tick = "AMZN"

# date_start and date_end is not needed as the code is using whichever latest data is available on Google Finance

# your working directory
dir = "C:/Users/TradingLab15/Desktop/R_project/yahoo_finance_R"
##################
# USER SPACE END #
##################

setwd(dir)
library(quantmod)

s<-c(tick)
fin<-lapply(s, getFinancials, auto.assign=FALSE)
names(fin)<-s

# F_ROA
NIBEA<-lapply(fin, function(x){
	x$IS$A["Net Income Before Extra. Items", ]
	})
data_NIBEA<-as.data.frame(NIBEA)

TASS<-lapply(fin, function(x){
	x$BS$A["Total Assets", ]
	})
data_TASS<-as.data.frame(TASS)


# F_delta_ROA

# F_CFO
CFO<-lapply(fin, function(x){
	x$CF$A["Cash from Operating Activities", ]
	})
data_CFO<-as.data.frame(CFO)

# F_Accrual

# F_delta_Margin
GP<-lapply(fin, function(x){
	x$IS$A["Gross Profit", ]
	})
data_GP<-as.data.frame(GP)

TR<-lapply(fin, function(x){
	x$IS$A["Total Revenue", ]
	})
data_TR<-as.data.frame(TR)

# F_delta_Turn

# F_delta_Lever
TLTD<-lapply(fin, function(x){
	x$BS$A["Total Long Term Debt", ]
	})
data_TLTD<-as.data.frame(TLTD)

# F_delta_Liquid
TCL<-lapply(fin, function(x){
	x$BS$A["Total Current Liabilities", ]
	})
data_TCL<-as.data.frame(TCL)

TCA<-lapply(fin, function(x){
	x$BS$A["Total Current Assets", ]
	})
data_TCA<-as.data.frame(TCA)

# EQ_Offer
TCSO<-lapply(fin, function(x){
	x$BS$A["Total Common Shares Outstanding", ]
	})
data_TCSO<-as.data.frame(TCSO)