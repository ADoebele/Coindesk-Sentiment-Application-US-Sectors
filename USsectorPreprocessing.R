##############################################################################
################### Data Preprocessing Sectors/Sentiment/FARMA5###############
##########################       Alexander D??bele      #######################
##############################################################################


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
