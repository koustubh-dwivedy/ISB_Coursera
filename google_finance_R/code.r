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

test_1<-lapply(fin, function(x){
	x$IS$A["Operating Income", ] / x$IS$A["Total Revenue",]
	})
data_test_1<-as.data.frame(test_1)

test_2<-lapply(fin, function(x){
	x$IS$A["Total Revenue", ] / x$IS$A["Operating Income",]
	})
data_test_2<-as.data.frame(test_2)

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
# F_Accrual
# F_delta_Margin
# F_delta_Turn
# F_delta_Lever
# F_delta_Liquid
# EQ_Offer