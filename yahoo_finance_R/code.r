#	FILENAME: code.r
#	DESCRIPTION: Code for importing Financial Statement data from Yahoo Finance to R environment
#	AUTHOR: Koustubh Dwivedy
#	START DATE: 19 Dec 2016

rm(list=ls())

##############
# USER SPACE #
##############
# tick: Ticker of stock
tick = "AMZN"
# start date in yyyy-mm-dd format
date_start = "2010-12-31"
# end date in yyyy-mm-dd format
date_end = "2011-06-30"
# your working directory
dir = "C:/Users/TradingLab15/Desktop/R_project/yahoo_finance_R"
##################
# USER SPACE END #
##################

setwd(dir)
