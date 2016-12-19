#	FILENAME: code.r
#	DESCRIPTION: Code for importing Financial Statement data from Google Finance to R environment
#	AUTHOR: Koustubh Dwivedy
#	START DATE: 19 Dec 2016

rm(list=ls())

##############
# USER SPACE #
##############
# tick: Ticker of stock
tick = "M&M"

# date_start and date_end is not needed as the code is using whichever latest data is available on Google Finance

# your working directory
dir = "C:/Users/TradingLab15/Desktop/R_project/google_finance_R"
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

ROA_val = data_NIBEA[1, 1]/data_TASS[2, 1]


# F_delta_ROA
delta_ROA_val = (data_NIBEA[1, 1]/data_TASS[2, 1]) - (data_NIBEA[2, 1]/data_TASS[3, 1])


# F_CFO
CFO<-lapply(fin, function(x){
	x$CF$A["Cash from Operating Activities", ]
	})
data_CFO<-as.data.frame(CFO)

CFO_val = data_CFO[1, 1]/data_TASS[2, 1]


# F_Accrual
Accrual_val = (data_NIBEA[1, 1] - data_CFO[1, 1])/data_TASS[2, 1]


# F_delta_Margin
GP<-lapply(fin, function(x){
	x$IS$A["Gross Profit", ]
	})
data_GP<-as.data.frame(GP)

TR<-lapply(fin, function(x){
	x$IS$A["Total Revenue", ]
	})
data_TR<-as.data.frame(TR)

delta_Margin_val = (data_GP[1, 1]/data_TR[1, 1]) - (data_GP[2, 1]/data_TR[2, 1])


# F_delta_Turn
delta_Turn_val = 2*data_TR[1, 1]/(data_TASS[1, 1] + data_TASS[2, 1]) - 2*data_TR[2, 1]/(data_TASS[2, 1] + data_TASS[3, 1])


# F_delta_Lever
TLTD<-lapply(fin, function(x){
	x$BS$A["Total Long Term Debt", ]
	})
data_TLTD<-as.data.frame(TLTD)

delta_Lever_val = 2*data_TLTD[1, 1]/(data_TASS[1, 1] + data_TASS[2, 1]) - 2*data_TLTD[2, 1]/(data_TASS[2, 1] + data_TASS[3, 1])


# F_delta_Liquid
TCL<-lapply(fin, function(x){
	x$BS$A["Total Current Liabilities", ]
	})
data_TCL<-as.data.frame(TCL)

TCA<-lapply(fin, function(x){
	x$BS$A["Total Current Assets", ]
	})
data_TCA<-as.data.frame(TCA)

delta_Liquid_val =  data_TCA[1, 1]/data_TCL[1, 1] - data_TCA[2, 1]/data_TCL[2, 1]


# EQ_Offer
TCSO<-lapply(fin, function(x){
	x$BS$A["Total Common Shares Outstanding", ]
	})
data_TCSO<-as.data.frame(TCSO)

F_ROA = 0
F_delta_ROA = 0
F_CFO = 0
F_Accrual = 0
F_delta_Margin = 0
F_delta_Turn = 0
F_delta_Lever = 0
F_delta_Liquid = 0
EQ_Offer = 0

if(ROA_val > 0){
	F_ROA = 1
}
if(CFO_val > 0){
	F_CFO = 1
}
if(delta_ROA_val > 0){
	F_delta_ROA = 1
}
if(Accrual_val < 0){
	F_Accrual = 1
}
if(delta_Lever_val < 0){
	F_delta_Lever = 1
}
if(delta_Liquid_val > 0){
	F_delta_Liquid = 1
}
if(delta_Margin_val > 0){
	F_delta_Margin = 1
}
if(delta_Turn_val > 0){
	F_delta_Turn = 1
}
if((data_TCSO[1, 1] - data_TCSO[2, 1]) <= 0){
	EQ_Offer = 1
}

F_SCORE = F_ROA + F_delta_ROA + F_CFO + F_Accrual + F_delta_Margin + F_delta_Turn + F_delta_Lever + F_delta_Liquid + EQ_Offer

print(F_SCORE)