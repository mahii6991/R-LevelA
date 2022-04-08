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


#---------------------------------------------------------------------------------------------------------

#the modelling tool                 #Artificial neural network

#------------------------------------------------------------------------------------------------------
#the artificial neural network are frequently used in the financial forecasting because of their ability 
#to deal with the highly nonlinear problems. the package nnet implements the feed forward neural network in R.
#ANNs are formed by the set of computing units linked to each other. Each neuron executes two consecutive-
#computation of the result to obtain its output value that is feed to to other neuron in the network , . Each
#of this neuron consists of associated weights. Constructing an artificial neural network consists of establishing
#an architecture for the network and then using an algorithm to find the weight of the connection between the
#neuron.

#this can be applied to both the tasks be it the regression or the classification task,

set.seed(1234)
library(nnet)
norm.data <- scale(Tdata.train)
nn <- nnet(Tform,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,linout=T,trace=F)
norm.preds <- predict(nn,norm.data[1001:2000,])
preds <- unscale(norm.preds,norm.data)


sigs.nn <- trading.signals(preds,0.1,-0.1)

true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.nn,true.sigs)


set.seed(1234)
library(nnet)

signals <- trading.signals(Tdata.train[,'T.ind.GSPC'],0.1,-0.1)
norm.data <- data.frame(signals=signals,scale(Tdata.train[,-1]))
nn <- nnet(signals ~ .,norm.data[1:1000,],size=10,decay=0.01,maxit=1000,trace=F)
preds <- predict(nn,norm.data[1001:2000,],type='class')


sigs.PR(preds,norm.data[1001:2000,1])

#---------------------------------------------------------------------------------------------------------

                   #support vector machines

#------------------------------------------------------------------------------------------------------

#support vector machines are modeling tools that , as ANNs can be applied to both regression and classification
#tasks. SVMs have been withnessing increased attention from different research communities based on their
#succussful application to several domain and also their strong theoretical background.
#The basic idea behind SVMs is that of mapping the original data into a new, high-dimensional space,where it is
#possible to apply linear models to obtain a seprating hyper plane , for example, seprating the classess 
#of the problem, in the case of classification tasks. The mapping of the original data into the new space is
#carried out with the help of the so called kernel function . SMVs are the linear machines operating on the dual
#representation induced by kernel functions.

library(e1071)
sv <- svm(Tform,Tdata.train[1:1000,],gamma=0.001,cost=100)
s.preds <- predict(sv,Tdata.train[1001:2000,])
sigs.svm <- trading.signals(s.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.svm,true.sigs)
#as we have observed that the SVM model achieves a considerable better scores than the ANN in terms of 
#precision, although with a much lower recall.

#next we consider the classification task, this time using the kernlab package:-
library(kernlab)
data <- cbind(signals=signals,Tdata.train[,-1])
ksv <- ksvm(signals ~ .,data[1:1000,],C=10)
ks.preds <- predict(ksv,data[1001:2000,])
sigs.PR(ks.preds,data[1001:2000,1])

#multivariate adaptive regression splines

#Multivariate adaptive splines are an example of an additive regression model. A MARS model .
library(earth)
e <- earth(Tform,Tdata.train[1:1000,])
e.preds <- predict(e,Tdata.train[1001:2000,])
sigs.e <- trading.signals(e.preds,0.1,-0.1)
true.sigs <- trading.signals(Tdata.train[1001:2000,'T.ind.GSPC'],0.1,-0.1)
sigs.PR(sigs.e,true.sigs)


#the results are comparable to the one which we have obtained with SVMs for classification with
#precision scores around 30% although with lower recall.

#MARS is only applicable to the regression problem so we do not show any example for the classification tasks.

################################################################################
    #from prediction to action

###############################################################################

#how will the prediction be used!
#we will use different kinds of trading strategies in the futures market and build different kinds of 
#trading strategies


#-------------------------------------------
#trading related evaluation criteria
#---------------------------------------------

#The r package performance analytics implements many of the existing financial metrics for analyzing the
#returns of some trading algorithms as the one we are proposing in this chapter.

#with respect to the overall results we will use (1) the simple net balance between the initial capital
#and the capital at the end of the testing period (sometimes called the profit/loss) (2) the percentage return
#that this net balance represets and (3) the excess return over the buy and hold strategy. This strategy
#consists of opening a long position at the beginning of the testing period and waiting until the end
#to close it.
#regarding risk -related measures, we will use the sharpe ratio coefficient, which measure the return per
#unit of risk, the latter being measured as the standard deviation of the returns.


#-------------------------------------------
#putting everything together
#---------------------------------------------
#this section describe how to implement the ideas we have sketched regarding trading with the signals
#of our model. OUr book package provides the function trading.simulator(), which can be used to put all 
#these ideas together by carrying out a trading simulation with the signals of any model.


#the following is an illustration of a user-defined trading policy function-
###################################################
### From Predictions into Actions
###################################################
policy.1 <- function(signals,market,opened.pos,money,
                     bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b' && !nOs) {
    quant <- round(bet*money/market[d,'Close'],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's' && !nOs) {
    # this is the nr of stocks we already need to buy 
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1-exp.prof),
                                         market[d,'Close']*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  # Now lets check if we need to close positions
  # because their holding time is over
  if (nOs) 
    for(i in 1:nOs) {
      if (d - opened.pos[i,'Odate'] >= hold.time)
        orders <- rbind(orders,
                        data.frame(order=-opened.pos[i,'pos.type'],
                                   order.type=1,
                                   val = NA,
                                   action = 'close',
                                   posID = rownames(opened.pos)[i]
                        )
        )
    }
  
  orders
}

#this function implements the first trading strategy we describe . the function has four parameter that we can
#to tune this strategy , these are the bet parameter, which specifies the percentage of our current money,
#that we will invest each time.  we will open the new position and is used when posting the limit order, 
#the exp.prof parameter which indicates the profit margin we wish for our position and is used when posting 
#the limit order , the max.loss which indicates the maximum loss we are willing to admit before we close the 
#position and is used to stop orders. and hold.time parameter which indicates the number of days we are
#willing to wait to reach the profit.

#Equivalently, the following function implements our secound trading strategies :

policy.2 <- function(signals,market,opened.pos,money,
                     bet=0.2,exp.prof=0.025, max.loss= 0.05
)
{
  d <- NROW(market) # this is the ID of today
  orders <- NULL
  nOs <- NROW(opened.pos)
  # nothing to do!
  if (!nOs && signals[d] == 'h') return(orders)
  
  # First lets check if we can open new positions
  # i) long positions
  if (signals[d] == 'b') {
    quant <- round(bet*money/market[d,'Close'],0)
    if (quant > 0) 
      orders <- rbind(orders,
                      data.frame(order=c(1,-1,-1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1+exp.prof),
                                         market[d,'Close']*(1-max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
    
    # ii) short positions  
  } else if (signals[d] == 's') {
    # this is the money already committed to buy stocks
    # because of currently opened short positions
    need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1,
                               "N.stocks"])*market[d,'Close']
    quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
    if (quant > 0)
      orders <- rbind(orders,
                      data.frame(order=c(-1,1,1),order.type=c(1,2,3), 
                                 val = c(quant,
                                         market[d,'Close']*(1-exp.prof),
                                         market[d,'Close']*(1+max.loss)
                                 ),
                                 action = c('open','close','close'),
                                 posID = c(NA,NA,NA)
                      )
      )
  }
  
  orders
}
#the function is similar to the previous one. the main difference lies in the fact that in the this trading 
#policy we allow for more than one position to be opened at the same time, and also there is no aging limit
#for closing the position.


#having defined our trading policy function we are ready to try our trading simulator . For illustration 
#purpose we will select a small sample fo our data to obtain an SVM , which is then used to obtain prediction
#for a subsequent period.
#we call our trading simulator with these predictions to obtain the results of our trading plolicy

# Train and test periods
start <- 1
len.tr <- 1000
len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)

# getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.tr,])
market <- GSPC[paste(date,'/',sep='')][1:len.ts]

# learning the model and obtaining its signal predictions
library(e1071)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)

# now using the simulated trader
t1 <- trading.simulator(market,sig,
                        'policy.1',list(exp.prof=0.05,bet=0.2,hold.time=30))



t1
#finding out the summary of the model
summary(t1)

#getting the details information of the summary of the model
tradingEvaluation(t1)#obtain a series of economic indicators of the performance during the simulation period.

#we can also obtain a graphical overview of the performance of the trader using the function 
plot(t1,market,theme='white',name='SP500')

#the results of this command is shown
#the results of this trader are bad, with a negative return. would the scenario be different if we used
#the secound trading policy?


#lets see:
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)


start <- 2000
len.tr <- 1000

len.ts <- 500
tr <- start:(start+len.tr-1)
ts <- (start+len.tr):(start+len.tr+len.ts-1)
s <- svm(Tform,Tdata.train[tr,],cost=10,gamma=0.01)
p <- predict(s,Tdata.train[ts,])
sig <- trading.signals(p,0.1,-0.1)
t2 <- trading.simulator(market,sig,'policy.2',list(exp.prof=0.05,bet=0.3))
summary(t2)
tradingEvaluation(t2)
#the trader , obtained by the same modelling techniques and using the same trading stategies , obtained a 
#considerable worse results. the major lesson to be learned here is: reliable statistical estimates. Do not 
#fooled by few repetitions fo some experiments , even if it includes a 2 -year testing period.


##model evaluation and selection

#by using the monte carlo simulation
###################################################
### Model Evaluation and Selection
###################################################
MC.svmR <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(e1071)
  t <- svm(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.svmC <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(e1071)
  tgtName <- all.vars(form)[1]
  train[,tgtName] <- trading.signals(train[,tgtName],b.t,s.t)
  t <- svm(form,train,...)
  p <- predict(t,test)
  factor(p,levels=c('s','h','b'))
}
MC.nnetR <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(nnet)
  t <- nnet(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
MC.nnetC <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(nnet)
  tgtName <- all.vars(form)[1]
  train[,tgtName] <- trading.signals(train[,tgtName],b.t,s.t)
  t <- nnet(form,train,...)
  p <- predict(t,test,type='class')
  factor(p,levels=c('s','h','b'))
}
MC.earth <- function(form,train,test,b.t=0.1,s.t=-0.1,...) {
  require(earth)
  t <- earth(form,train,...)
  p <- predict(t,test)
  trading.signals(p,b.t,s.t)
}
singleModel <- function(form,train,test,learner,policy.func,...) {
  p <- do.call(paste('MC',learner,sep='.'),list(form,train,test,...))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
slide <- function(form,train,test,learner,relearn.step,policy.func,...) {
  real.learner <- learner(paste('MC',learner,sep='.'),pars=list(...))
  p <- slidingWindowTest(real.learner,form,train,test,relearn.step)
  p <- factor(p,levels=1:3,labels=c('s','h','b'))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
grow <- function(form,train,test,learner,relearn.step,policy.func,...) {
  real.learner <- learner(paste('MC',learner,sep='.'),pars=list(...))
  p <- growingWindowTest(real.learner,form,train,test,relearn.step)
  p <- factor(p,levels=1:3,labels=c('s','h','b'))
  eval.stats(form,train,test,p,policy.func=policy.func)
}
#the following code creates a set of function that will be used to carry out a full+train+test+evaluate cycle
#of the different trading system we will compare , These function will be called form within the monte carlo-
#simulation 

eval.stats <- function(form,train,test,preds,b.t=0.1,s.t=-0.1,...) {
  # Signals evaluation
  tgtName <- all.vars(form)[1]
  test[,tgtName] <- trading.signals(test[,tgtName],b.t,s.t)
  st <- sigs.PR(preds,test[,tgtName])
  dim(st) <- NULL
  names(st) <- paste(rep(c('prec','rec'),each=3),
                     c('s','b','sb'),sep='.')
  
  # Trading evaluation
  date <- rownames(test)[1]
  market <- GSPC[paste(date,"/",sep='')][1:length(preds),]
  trade.res <- trading.simulator(market,preds,...)
  
  c(st,tradingEvaluation(trade.res))
}
#the function eval.stats() uses the two other function to collect the precision and recall of the signals
#and several economic evaluatin metrics.Funciton sigs.PR() receives as argument the predicted and true signals.
#and calcualte precision and recall

#---------------------------------------
#the following code runs the monte carlo experiments.
pol1 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.025,max.loss=0.05,hold.time=10)

pol2 <- function(signals,market,op,money)
  policy.1(signals,market,op,money,
           bet=0.2,exp.prof=0.05,max.loss=0.05,hold.time=20)

pol3 <- function(signals,market,op,money)
  policy.2(signals,market,op,money,
           bet=0.5,exp.prof=0.05,max.loss=0.05)

# The list of learners we will use
TODO <- c('svmR','svmC','earth','nnetR','nnetC')

# The data sets used in the comparison
DSs <- list(dataset(Tform,Tdata.train,'SP500'))

# Monte Carlo (MC) settings used
MCsetts <- mcSettings(20,     # 20 repetitions of the MC exps
                      2540,   # ~ 10 years for training
                      1270,   # ~ 5 years for testing
                      1234)   # random number generator seed

# Variants to try for all learners
VARS <- list()
VARS$svmR   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$svmC   <- list(cost=c(10,150),gamma=c(0.01,0.001),
                    policy.func=c('pol1','pol2','pol3'))
VARS$earth <- list(nk=c(10,17),degree=c(1,2),thresh=c(0.01,0.001),
                   policy.func=c('pol1','pol2','pol3'))
VARS$nnetR  <- list(linout=T,maxit=750,size=c(5,10),
                    decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))
VARS$nnetC  <- list(maxit=750,size=c(5,10),decay=c(0.001,0.01),
                    policy.func=c('pol1','pol2','pol3'))

# main loop
for(td in TODO) {
  assign(td,
         experimentalComparison(
           DSs,         
           c(
             do.call('variants',
                     c(list('singleModel',learner=td),VARS[[td]],
                       varsRootName=paste('single',td,sep='.'))),
             do.call('variants',
                     c(list('slide',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('slide',td,sep='.'))),
             do.call('variants',
                     c(list('grow',learner=td,
                            relearn.step=c(60,120)),
                       VARS[[td]],
                       varsRootName=paste('grow',td,sep='.')))
           ),
           MCsetts)
  )
  
  # save the results
  save(list=td,file=paste(td,'Rdata',sep='.'))
}
#this code will take days to run in the full manner so need to download the saved files form the book 
#website and try to ask the permission for the HPC clusters of computers to run this code on them

#RESULT ANALYSIS
#first need to download the files form the book webiste
load('svmR.Rdata')
load('svmC.Rdata')
load('earth.Rdata')
load('nnetR.Rdata')
load('nnetC.Rdata')



tgtStats <- c('prec.sb','Ret','PercProf',
              'MaxDD','SharpeRatio')
allSysRes <- join(subset(svmR,stats=tgtStats),
                  subset(svmC,stats=tgtStats),
                  subset(nnetR,stats=tgtStats),
                  subset(nnetC,stats=tgtStats),
                  subset(earth,stats=tgtStats),
                  by = 'variants')
rankSystems(allSysRes,5,maxs=c(T,T,T,F,T))

#for each trading system variants, we have measured several statistics of performance . Deciding the best 
#model according to our score require the balance of all these scores.the selected model may vary depending 
#on which criteria we value the most.

#in terms of trading performance , the return of the system is important (statistics "ret" in our experiment)
#as well as the return over the buy and hold stategy . also important is the percentage of profitable trades,
#which should be clearly above 50% (statics "percProf")

#In terms of risk analysis , it is relevant to look to both the value of the sharpe ration and the maximum
#Draw-Down



summary(subset(svmC,
               stats=c('Ret','RetOverBH','PercProf','NTrades'),
               vars=c('slide.svmC.v5','slide.svmC.v6')))



#in effect at most these methods made a single trade over the testing period wiht an average return of 0.25%
#which is -77.01% below the naive buy and hold strategy. these are clearly useless models.

#A final remark on the global ranking is that the result in terms of maximum draw -down cannot be considered
#too bad , while the sharpe ratio scores are definitely disappointing.

fullResults <- join(svmR,svmC,earth,nnetC,nnetR,by='variants')
nt <- statScores(fullResults,'NTrades')[[1]]
rt <- statScores(fullResults,'Ret')[[1]]
pp <- statScores(fullResults,'PercProf')[[1]]
s1 <- names(nt)[which(nt > 20)]
s2 <- names(rt)[which(rt > 0.5)]
s3 <- names(pp)[which(pp > 40)]

namesBest <- intersect(intersect(s1,s2),s3)


compAnalysis(subset(fullResults,
                    stats=tgtStats,
                    vars=namesBest))

#note that the above code can generate some warnings caused by the fact that some system do not obtain the 
#valid results Despite the variability of the results , the above wilcoxon significance test tells us that
#average return of "single.nnetR.v12" is higher than those of the other systems with the 95% confidence 
#interval
plot(subset(fullResults,
            stats=c('Ret','PercProf','MaxDD'),
            vars=namesBest))


getVariant('single.nnetR.v12',nnetR)
#as we can observe, it uses the trading policy "plo3" and learns a neural network with the ten hidden 
#units with the learing decay rate of 0.01.






###################################################
### The Trading System
###################################################

#in order to apply any of the selected system to the evaluation period , we need the last 10 years 
#before the evaluation period.



data <- tail(Tdata.train,2540)
results <- list()
for(name in namesBest) {
  sys <- getVariant(name,fullResults)
  results[[name]] <- runLearner(sys,Tform,data,Tdata.eval)
}
#we are getting the error message in it
results <- t(as.data.frame(results))


results[,c('Ret','RetOverBH','MaxDD','SharpeRatio','NTrades','PercProf')]
#this command is not ran properly



#the best model has the following characterstics
getVariant('grow.nnetR.v12',fullResults)

#we now proceed with a deeper analysis of the performance of this best trading system, accross the 
#evaluation period.For this to be possible , we need to obtain the trading record of the system during
#this period
model <- learner('MC.nnetR',list(maxit=750,linout=T,trace=F,size=10,decay=0.001))
preds <- growingWindowTest(model,Tform,data,Tdata.eval,relearn.step=120)
signals <- factor(preds,levels=1:3,labels=c('s','h','b'))
date <- rownames(Tdata.eval)[1]
market <- GSPC[paste(date,"/",sep='')][1:length(signals),]
trade.res <- trading.simulator(market,signals,policy.func='pol2')

#the analysis of the figure found out that the system went through the no trading zone form 2003 to 2007
plot(trade.res,market,theme='white',name='SP500 - final test')

#this package provides us overwhelming set of tools for analyzing the performance fo any trading system.
#here is some glance of it 
library(PerformanceAnalytics)
rets <- Return.calculate(trade.res@trading$Equity)


chart.CumReturns(rets,main='Cumulative returns of the strategy',ylab='returns')
#for most the period the system is on the positive side with a peak return of 10% 

yearlyReturn(trade.res@trading$Equity)


plot(100*yearlyReturn(trade.res@trading$Equity),
     main='Yearly percentage returns of the trading system')
abline(h=0,lty=2)


table.CalendarReturns(rets)


table.DownsideRisk(rets)

#code for this module is finished here

#summary 
#Time series modelling
#Handling regime shifts with windowing mechanism
#Artificial neural network
#support vector machine
#Multivariate adaptive regression splines
#Evaluating time series models with the monte carlo method

