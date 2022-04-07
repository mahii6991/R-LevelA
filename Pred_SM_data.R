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
#library(tseries)#it is a package for computational finance and time series analysis

#loading the dataset form the web wiht the help of function get.hist.quote
#GSPC1 <- as.xts(get.hist.quote("^GSPC",start = "1970-01-02",end = '2009-09-15',
                               #quote = c("Open","High","Low","Close","Volume","AdjClose")))


#in this way we are not able to load the dataset into our R environment
library(quantmod)
getSymbols("^GSPC")



#now loading the dataset from the web
getSymbols("^GSPC",from="1970-01-01",to="2009-09-15")
colnames(GSPC) <- c("Open","High","Low","Close","Volume","AdjClose")#changing the colnames of the GSPC dataset

#==========================================================================================================

           #Defining the function which we were going to use in it 

#=========================================================================================================
#now we are going to implement the functions with the simple indicator
T.ind <- function(quotes,tgt.margin=0.025,n.days=10) {
  v <- apply(HLC(quotes),1,mean)
  
  r <- matrix(NA,ncol=n.days,nrow=NROW(quotes))
  ## The following statment is wrong in the book (page 109)!
  for(x in 1:n.days) r[,x] <- Next(Delt(Cl(quotes),v,k=x),x)
  
  x <- apply(r,1,function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
  if (is.xts(quotes)) xts(x,time(quotes)) else x
}

#to get the better idea of the behaviour of this indicator which was produced with the following code
candleChart(last(GSPC,'3 months'),theme='white',TA=NULL)
avgPrice <- function(p) apply(HLC(p),1,mean)
addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
addT.ind <- newTA(FUN=T.ind,col='red',legend='tgtRet')
addAvgPrice(on=1)
addT.ind()


#the function newTA() can be used to create new plotting functions
#the intial graph produced by the candleChart()
#the function addAvgPrice() was called with the parameter set to 1.
#the function addT.ind() was not called with this argument

#now we are going to use some of the indicators in the r package like average true range(), which is the idicator
#of volatility of the series; the stochastic Momentum Index(SMI),which is the momentum indicator, the wells
#wilder's Directional Movement Index(ADX), the aroon indicator that tries to identify starting trends, 
#the bollinger bands that compares the volatility over the period of time; the chaikin volatility , the close 
#location value Movement Value(EMV)


#as such we do not plan to use these indicators for trading. As such, we have carried out some post-processing
#of th output of the TTR functions to obtain a single value for each one. The following process implements this:


myATR <- function(x) ATR(HLC(x))[,'atr']
mySMI <- function(x) SMI(HLC(x))[,'SMI']
myADX <- function(x) ADX(HLC(x))[,'ADX']
myAroon <- function(x) aroon(x[,c('High','Low')])$oscillator
myBB <- function(x) BBands(HLC(x))[,'pctB']
myChaikinVol <- function(x) Delt(chaikinVolatility(x[,c("High","Low")]))[,1]
myCLV <- function(x) EMA(CLV(HLC(x)))[,1]
myEMV <- function(x) EMV(x[,c('High','Low')],x[,'Volume'])[,2]
myMACD <- function(x) MACD(Cl(x))[,2]
myMFI <- function(x) MFI(x[,c("High","Low","Close")], x[,"Volume"])
mySAR <- function(x) SAR(x[,c('High','Close')]) [,1]
myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]

#the variable we have described form our initial set of predictors for the task of forecasting the future
# value of the T indicator.we will try to reduce the set of 22 variable using a features selection method.
#Random forest can also be used to estimate the importance of the variale involved in a prediction tasks.
#we will the % of error as the indicators to filter out the features , this method can be the same as the 
#wrapper method 



#In this process we will split the dataset into two part the first set to 30 years and the other set to the 
#9 years for the final test

#let's start
library(randomForest)
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) + 
                             myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) + 
                             myBB(GSPC)  + myChaikinVol(GSPC) + myCLV(GSPC) + 
                             CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) + 
                             myVolat(GSPC)  + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                             mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))

#splitting my dataset into two parts training and tesitng part 
set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c(start(GSPC),index(GSPC["1999-12-31"])),
                 ntree=50, importance=T)


ex.model <- specifyModel(T.ind(IBM) ~ Delt(Cl(IBM),k=1:3))
data <- modelData(ex.model,data.window=c('2009-01-01','2009-08-10'))

#the code given above starts by specifying and obtaining the data to be used for modelling using the 
#function specifyModel(). This function creates a quantmod object that contains the specification of a certain
#dataset.This specification may refer to data coming from different types of sources some of which may even 
#not be currently in the memory of the computer.


#====================================================
varImpPlot(rf@fitted.model,type=1)
#by looking at the results of the figures provided by this chart we can say that we will keep the threshold
#as the 10 for the features selection from the random forests.

#filtering out the features of the model using the threshold value of it
imp <- importance(rf@fitted.model,type=1)
rownames(imp)[which(imp > 10)]

#using this information we can obtain our final data set as follows:
data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1) + myATR(GSPC) + myADX(GSPC) +    myEMV(GSPC) + myVolat(GSPC)  + myMACD(GSPC) + mySAR(GSPC) + runMean(Cl(GSPC)) )


###################################################
### The Prediction Models
###################################################

#in the previous section we have obtained a quantmod object(data.model) containing the data we plan to 
#use with our predictive models. This data has the target the value of the T indicators and as a predictor a 
#series of other variable that results from a features selection process. We have seen that our real goal,
#is to predict the correct trading signal at any time t. How can we do that , given the data we have 
#generated in the previous section?


Tdata.train <- as.data.frame(modelData(data.model,
                                       data.window=c('1970-01-02','1999-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model,
                                              data.window=c('2000-01-01','2009-09-15'))))
Tform <- as.formula('T.ind.GSPC ~ .')







