##############################################################################
################### Data Preprocessing Sectors/Sentiment/FARMA5 ##############
###################            and static Regression            ##############
###################              Alexander Doebele              ##############
##############################################################################


##Part 1: Data preprocessing
require(lubridate)

setwd("/Users/alexd/Desktop/Masterarbeit/Sector Reg/Original")

#Load datasets of different sectors:
Indust        = read.table(file = "S&P 500 Industrials.csv", sep = ";", 
                    header = FALSE, col.names = c("Date","Price"))

Commu         = read.table(file = "S&P 500 Communication Services.csv", sep = ";", 
                   header = FALSE, col.names = c("Date","Price"))

Consumer_st   = read.table(file = "S&P Consumer Staples.csv", sep = ";", 
                         header = FALSE, col.names = c("Date","Price"))

Consumer_dis  = read.table(file = "S&P 500 Consumer Discretionary.csv", sep = ";", 
                          header = FALSE, col.names = c("Date","Price"))

Energy        = read.table(file = "S&P 500 Energy.csv", sep = ";", 
                    header = FALSE, col.names = c("Date","Price"))

Financials    = read.table(file = "S&P 500 Financials.csv", sep = ";", 
                        header = FALSE, col.names = c("Date","Price"))

Health        = read.table(file = "S&P 500 Health Care.csv", sep = ";", 
                    header = FALSE, col.names = c("Date","Price"))

Information   = read.table(file = "S&P 500 Information Technology.csv", sep = ";", 
                         header = FALSE, col.names = c("Date","Price"))

Estate        = read.table(file = "S&P Real Estate Select.csv", sep = ";", 
                    header = FALSE, col.names = c("Date","Price"))

Farma         = read.table(file = "Farma5.csv", sep = "," , header = TRUE, 
                   col.names = c("Date","Mkt.RF","SMB","HML","RMW","CMA","RF"))

Sent = read.table(file = "Pred_Sentiment.csv" ,sep = ",", header = FALSE,
                  col.names = c("Date","Sentiment"))



#Define list of sector names for faster computation:
sectors = list(Commu,Consumer_dis,Consumer_st,Energy,Estate,Financials,Health,
               Indust,Information)

names(sectors) = c("Commu","Consumer_dis","Consumer_st","Energy",
                   "Estate","Financials","Health","Indust","Information")


##Uniform format for all dates and returns:

#Sector Data:
for( i in 1:length(sectors)){
  print(i)
  sectors[[i]]$Date = as.Date(sectors[[i]]$Date,format = "%d.%m.%y")
  sectors[[i]]$Week_day = as.numeric(format(sectors[[i]]$Date, format='%w'))
  sectors[[i]]$End_of_Week = sectors[[i]]$Date + (7 - sectors[[i]]$Week_day)
  sectors[[i]]$Price = gsub(",",".",sectors[[i]]$Price)
  sectors[[i]]$Price = as.numeric(as.character(sectors[[i]]$Price))
}

#Sentiment:
Sent$Date = as.Date(Sent$Date,format = "%Y-%m-%d")

#Controll variables:
Farma$Date = ymd(Farma$Date) 
Farma$Week_day = as.numeric(format(Farma$Date, format='%w'))
Farma$End_of_Week = Farma$Date + (7 - Farma$Week_day)


##Calculate Returns:
for( i in 1:length(sectors)){
  sectors[[i]]$Return = c(NA,diff(sectors[[i]]$Price)/sectors[[i]]$Price[-length(sectors[[i]]$Price)])
}


##Adjust timeframe: 
#Sectors:
for (i in 1:length(sectors)){
  
  sectors[[i]] = subset(sectors[[i]],Date >="2016-01-01" & Date <= "2019-03-01") 
}
#Controlls:
Farma = subset(Farma, Date >="2016-01-01" & Date <= "2019-03-01")
#Sentiment:
Sent = subset(Sent, Date >="2016-01-01" & Date <= "2019-03-01")



#Return to global environment:
list2env(sectors,globalenv())

#########################################################################################################################

##Part 2: Static Regression
library(dynlm)

#Aggregate data into weekly levels (by mean values):
Estate_Return       = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Estate, na.rm=TRUE),c("Date","Estate_Return"))
Commu_Return        = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Commu, na.rm=TRUE),c("Date","Commu_Return"))
Consumer_dis_Return = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Consumer_dis, na.rm=TRUE),c("Date","Consumer_dis_Return"))
Consumer_st_Return  = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Consumer_st, na.rm=TRUE),c("Date","Consumer_st_Return"))
Energy_Return       = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Energy, na.rm=TRUE),c("Date","Energy_Return"))
Financials_Return   = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Financials, na.rm=TRUE),c("Date","Financials_Return"))
Health_Return       = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Health, na.rm=TRUE),c("Date","Health_Return"))
Indust_Return       = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Indust, na.rm=TRUE),c("Date","Indust_Return"))
Information_Return  = setNames(aggregate(Return~End_of_Week, FUN=mean, data=Information, na.rm=TRUE),c("Date","Information_Return"))

Sentiment           = setNames(aggregate(Sentiment~End_of_Week, FUN=mean, data=Sent, na.rm=TRUE),c("Date","Sentiment"))
Controlls           = setNames(aggregate(cbind(Mkt.RF,SMB,HML,RMW,CMA,RF)~End_of_Week, FUN=mean, data=Farma, na.rm=TRUE),c("Date","Mkt.RF","SMB","HML","RMW","CMA","RF"))


Finance_Returns = Financials_Return$Financials_Return

#Information_Returns = Information_Return$Information_Return

#Energy_Returns = Energy_Return$Energy_Return

Sentimen = Sentiment$Sentiment

Mkt.RF = Controlls$Mkt.RF
HML    = Controlls$HML
SMB    = Controlls$SMB
RMW    = Controlls$RMW
CMA    = Controlls$CMA

#Regression equation for financial returns:
first = dynlm(Finance_Returns ~ Sentimen + Mkt.RF + HML + SMB + RMW + CMA)

#Summary statistics
summary(first)

