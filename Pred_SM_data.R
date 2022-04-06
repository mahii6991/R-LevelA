#predicting stock market return with the help of R

library(DMwR)#loading the library with the important amount of dataset present in it.
data("GSPC")


#summary of the project
#the data available for this project depends on the time.This means each observation of out dataset has a time value attached to 
#it . This type of data is frequently known as the time series dataset.
#the main aim of the time series forecasting is to predict the futures events based on the previous events


#In this process we were trying to predict the future evoulution of the index based on the historical quotes dataset..Thus our
#prediction model will be incorporated in a trading system, that is profit/loss resulting from the action of the system,that
#is profit and loss resulting from the system


#library another libraray
library(xts)
#loading the random set of variable with the help of xts library
x1 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01"),len=100,by="day"))


x1[1:5]#looking at the time dependent data form 1:5

#creating a new dataset with the random values
x2 <- xts(rnorm(100),seq(as.POSIXct("2000-01-01 13:00"),len=100,by='min'))

#loading the 1:4 values in R
x2[1:4]


#loading another vector of data with the same method as above 
x3 <- xts(rnorm(3),as.Date(c("2005-01-01","2005-01-10","2005-01-12")))

#lets view the dataset
x3



#finding the index values
x1[as.POSIXct("2000-01-04")]

#finding the whole series of index values
x1["2000-04"]

#finding the special data from the dataset
x1["/20000103"]


#creating the multiple time series
mts.vals <- matrix(round(rnorm(25),2),5,5)
colnames(mts.vals) <- paste('ts',1:5,sep ="")
mts <- xts(mts.vals,as.POSIXct(c('2003-01-01','2003-01-04','2003-01-05','2003-01-06','2003-01-16')))
mts#printing out the values of the values of the time series data and checking it

#finding out the index of the data set
index(mts)

#finding out the correlation in the dataset
coredata(mts)
#In  summary xts, object are adequate to store stock quotes data, as they allow to store multiple
#timeseries with irregular time tags, and provide powerful indexing schemes


#===================================================================================================

#loading the data into csv format with the help of an package
library(tseries)#it is a package for computational finance and time series analysis

#loading the dataset form the web wiht the help of function get.hist.quote
GSPC1 <- as.xts(get.hist.quote("^GSPC",start = "1970-01-02",end = '2009-09-15',
                               quote = c("Open","High","Low","Close","Volume","AdjClose")))
#in this way we are not able to load the dataset into our R environment
library(quantmod)
getSymbols("^GSPC")



#now loading the dataset from the web
getSymbols("^GSPC",from="1970-01-01",to="2009-09-15")
colnames(GSPC) <- c("Open","High","Low","Close","Volume","AdjClose")#changing the colnames of the GSPC dataset


