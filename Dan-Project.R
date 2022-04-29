head(Database_1)

#so in this dataset we are analyzing the dataset that is based on the price fluctuation by the crypto market and lets check out the correlation
M1 <- Database_1

#finding out the summary of the model
summary(M1)
#finding out the glimpse of the model
glimpse(M1)

#using another function
library(psych)
describe(M1)

dim(M1)
#need to remove the na's value from the model
hist(M1$bth)

library(car)
qqPlot(M1$bth,main="finding the mean values of the bth")

plot(M1$bth)#finding the relationship of the chart with respect to the time

library(ggplot2)
viz1 <- ggplot(M1, aes(y=M1$bth,x=M1$`Exchange Date`))+
  geom_point()#we can see how the price of bitcoin is incresing from the 2019, it had suddenly became more and more popular with the time

viz1
library(plotly)
ggplotly(viz1)#plotting it with the interactinve plotting techiniques

#now is the time to remove the na values from the dataset
library(DMwR)

manyNAs(M1, 0.2)
#filling up the values using the central imputaion techniques to fill the na's values in the dataset
M2 <- centralImputation(M1[,2:8])
str(M1)

#trying out with the different missing value filling indicator
M2 <- knnImputation(M1, k=10) #meth = "median")

#how to fill the missing values in the column
M1$`eur/us`[is.na(M1$`eur/us`)] <- mean(M1$`eur/us`, na.rm = TRUE)
M1$`pnd/us`[is.na(M1$`pnd/us`)] <- mean(M1$`pnd/us`, na.rm = TRUE)
M1$Feds[is.na(M1$Feds)] <- mean(M1$Feds, na.rm = TRUE)
M1$FTSE[is.na(M1$FTSE)] <- mean(M1$FTSE, na.rm = TRUE)
M1$`Gold Future`[is.na(M1$`Gold Future`)] <- mean(M1$`Gold Future`, na.rm = TRUE)
M1$`Gold Cash`[is.na(M1$`Gold Cash`)] <- mean(M1$`Gold Cash`, na.rm = TRUE)

#checking the results after filling the na's values with the mean values
head(M1)

#==============================================================================
                 #let's start with the project of statistics
#==============================================================================

summary(M1)#finding out the summary of the model

hist(M1$`Gold Future`)#looking out the distribution of different variates

plot(M1$`eur/us`)#by looking at the curve we can see that there is lot of amount of volatility in it

qqPlot(M1$`pnd/us`)#we can see it is currency so the distribution between the values in following the confidence interval of 95% with some 
                   # exception in the middle

names(M1)
M2 <- M1[,c(2,4,5,7)]#selecting each set form the each of the variates for further analysis
head(M2)
str(M2)
M2$Feds

X <- M2$Feds
X
length(X)
hist(X)
sum(X)/length(X)
mean(X)  # same as calculation above 

mean(X,trim=0.1) # this implements the trimmed mean (see book)

X-mean(X) # mean correct , this is called the deviation
hist(X-mean(X))
(X-mean(X))^2  # note R multiplied vector elements and produced a vector 
sum((X-mean(X))^2)/(length(X*X)-1) # divide by n-1 to avoid sampling bias
var(X)  # same as calculation above

summary(M2$Feds)  # quartiles and boxplot
boxplot(M2$Feds)
sort(M2$Feds) #check quartiles against ordered data here 


boxplot()



intro_1 %>%  
  kbl() %>%
  kable_styling()


#======================================================================================================================================
  
                                 #staring off with the introduction of the dataset and then going to build the summary table out of it.
#=======================================================================================================================================

a <- c("Exchange Date","bth","eur/us", "pnd/us" , "Feds", "FTSE","Gold Future","Gold Cash")
b <- c(" the data at which the price of different curriencies and crypto","which is a type of decentralized digital currency without a central bank","the two centralised currency euro aganist dollar","the two centralised currency pound aganist dollar","the interest rate at which depository institutions trade federal funds"," the London Stock Exchange "," world's leading benchmark futures contract for gold","he amount of gold bought")
c <- c(1,2,3,4,5,6,7,8)

intro <- data.frame(a,b)

dim(intro)

intro_1 <- setNames(intro, c("Variable","Variable Explaination"))


intro_1 %>%  
  kbl() %>%
  kable_styling()


#======================================================================================================================================

#summary table
#=======================================================================================================================================


#so in this dataset we are analyzing the dataset that is based on the price fluctuation by the crypto market and lets check out the correlation
M1 <- Database_1


#using another function
summary(M1) %>%  
  kbl() %>%
  kable_styling()
#===============================

colSums(is.na(M1))%>%  
  kbl() %>%
  kable_styling()


quantile(M1$bth)
mean(M1$bth)
#==================================================================plotting a histogram==================================

library(car)
library(ggplot2)
ggplot(M1,aes(x=bth))+
  geom_histogram(bin=5, colour="black")+
  geom_vline(aes(xintercept=mean(M1$bth, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  ggtitle("Frequency distribution of prices for Bitcoin")


qqPlot(M1$bth,main="finding the mean values of the bth")

#================================plotting a scatterplot our of it====================================
viz1 <- ggplot(M1, aes(y=bth,x=`Exchange Date`))+
  geom_point()+#we can see how the price of bitcoin is incresing from the 2019, it had suddenly became more and more popular with the time
  ggtitle("How price changed in Recent years")

viz1
#===============================done with the data analysis now need to start the t-test of the given variables

#removing the na variable in order to find the correlation between different variables in the dataset
X<-M1[,-1]
M1.corr<-cor(X)

#loading the library to view the corrplot 
library(corrplot)
## corrplot 0.84 loaded
#corrplot.mixed(M1.corr, lower.col = "black", number.cex = .7)


#corrplot.mixed(M1.corr, Method = "color", number.cex = .7)


#with the help of ggplot2 we have plotted a beautiful correlation matrix
library(ggplot2)
library(ggcorrplot)
ggcorrplot(M1.corr, hc.order = TRUE, type = "lower",
           lab = TRUE)


library(GGally)
ggpairs(X)
#===================================next we will look more deep amoung our variable to find our the relationship between the variable by doing some test=====
#we are finding out the mean of the variable and then we will calculate its value using the mean of the variable
mean(M1$bth)

#doing the t-test with respect to the hypothesis and checking out the results from it.
t.test(M1$bth, mu=1110)


#null hyptohesis states that there is no difference between the two groups
# so we will try to predict this with the help of the t test and find out more about it.
#1st 
mean(M1$bth)#values 1113.837
mean(M1$`Gold Cash`)#values 1487.771

#we will try to put this values in the t=test and try to find out the difference between both groups
t.test(M1$bth,M1$`Gold Cash`)


#performing the fishter.ttest
fisher.test(M1$bth,M1$`Gold Cash`,simulate.p.value = TRUE)


#======================================making a model out of linear regression and extracting the resutls================================


#first doing some visualisation
boxplot(log(M1$bth))
#first running the linear model in the datset and checking out the results of it
model_1 <- lm(bth ~.,data = M1[,-1])
summary(model_1)

#running on the all dataset and try to find the appropiate model out of it
old_model_1 <- lm(bth ~ `eur/us` + `pnd/us` + Feds +  FTSE +  `Gold Future` + `Gold Cash`, data= M1[,-1])
summary(old_model_1)


#ran the model with the terms which contains the correlation between them 
old_model_2 <- lm(bth ~ `eur/us` + `pnd/us` + Feds *  FTSE +  `Gold Future` + `Gold Cash`, data= M1[,-1])
summary(old_model_2)
#tried to convert the column into the log form to see if it has an effect on the overall model
m2 <- log(M1$`Gold Future`)
#saved the dataframe into the another model
m2 <- M1
#replaced the values of the column gold futures with the log values in it
m2$`Gold Future` <- log(M1$`Gold Future`)


#trying to change the gold cash 

#replced the values of the "-inf", with tht values equal to 0
m2$`Gold Future`[m2$`Gold Future`=="-Inf"] <- 0
#first running it without the log and running it with the log term and checking the f- statistics change on the model to find the fitting and the r value
old_model_3 <- lm(bth ~ `eur/us` + `pnd/us`  + `Gold Future`  + `Gold Cash`, data= m2[,-1])
summary(old_model_3)

#now trying out the model after removing the values of gold futures and checking the reuslts
old_model_4 <- lm(bth ~ `eur/us` + `pnd/us`  + `Gold Cash`, data= M1[,-1])
summary(old_model_4)#with this model out F-statistics jumped to the top in all the models 

#now we will try to do some model comparison and we will try the step function to find the higher Aic values we are getting form which model and checking it out.
step(old_model_1)# feature selection method does not provide us the significant results form the data set we will try to look for some other method.

#testing the different models with the help of anova method 
anova(old_model_3,old_model_4)

#using the aov function
aov(old_model_3,old_model_4)

hist(M1$bth)


library(car)
qqPlot(M1$bth)#we can say that the data is not normally distributed around the 95% confidence interval
library(moments)#loading the package to find the skewness of the dataset
skewness(M1$bth, na.rm = TRUE)


#loading some package to try to find the skewness of the dataset and to correct it in the form of normally distribution 
library(ggpubr)
library(moments)

#finding out the normally distributed curve with the help of visualisation and checking how much is it skewed to the other side of it.
ggdensity(M1, x = "bth", fill = "lightgray", title = "bth") +
  scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

# Distribution of PHYS variable
ggdensity(df, x = "PHYS", fill = "lightgray", title = "PHYS") +
  scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")


#so after transforming my response variable into the log form we can see that it is somewhat became distributed in a sense we can consider 
#because it provides far more better distribution than the previous one
hist(log(M1$bth))

#after transforming it into a log lets check out the results
log_model_1 <- lm(log(bth) ~ `eur/us` + `pnd/us`  + `Gold Future`  + `Gold Cash`, data= m2[,-1])

summary(log_model_1)


#running the log model again by removing some variable and checking out the results out of it 
log_model_2 <- lm(log(bth) ~ `eur/us` + `pnd/us`  + `Gold Cash`, data= m2[,-1])

#this is by far the best performing model we have received.
summary(log_model_2)#This is the project model that we have received.

#using the performance metrics to find out the model performance in the model
model_performance(log_model_2)

#finding the performance of the another model
model_performance(log_model_1)

plot(log_model_2)


#=========================================done till date 17/04/2022===========================================================================

                                           #need to do a t-test in this place
#when doing the t-test between the log transformation of bth and gold cash, we are doing this
t.test(log(M1$bth),M1$`Gold Cash`)

library(robust)
library(MASS)

#by using the 
log_model_2 <- lmRob(log(bth) ~ `eur/us` + `pnd/us`  + `Gold Cash`, data= m2[,-1])

#this is by far the best performing model we have received.
summary(log_model_2)#This is the project model that we have received.






old_model_6 <- lm(bth ~ `eur/us` + `pnd/us` +`eur/us` * `pnd/us` + `Gold Cash`, data= M1[,-1])
summary(old_model_6)#





#trying to transform some predictor variables and trying to fit the model again.

log_model_7 <- lm(log(bth) ~ `eur/us` + `pnd/us`  + log(`Gold Cash`), data= m2[,-1])


summary(log_model_7)


plot(log_model_7)





log_model_8 <- lm(log(bth) ~ log(`eur/us`) + `pnd/us`  + log(`Gold Cash`), data= m2[,-1])


summary(log_model_8)

plot(log_model_8)



model_performance()





#=======================================================================================================================================================
dim(Boston)

#starting to train and test my dataset to find how it performs in the training data and then in the test data
dim(M1)
set.seed(1)
train <- sample(1:nrow(M1), 1000)
M1.train <- M1[train,]

M1.test <-M1[-train,]

#so we have splitted the model in the testing and the training dataset to find out how our model performs on both the datasets
dim(M1.train)
dim(M1.test)

Model_2 <- lm(bth ~.,data = M1[,-1])
summary(Model_2)
Model_3 <- lm(bth ~.-FTSE,data = M1[,-1])

summary(Model_3)
Model_4 <- lm(bth ~.-FTSE-`Gold Future`,data = M1[,-1])
summary(Model_4)

#trying new model and finding out the relevan results out of it.


#so after running the model multiple times we have removed the ftse and the gold futures variable form our model to find the prediction 
train.model_4 <- Model_4  # chosen model on training data
train.model_4$coefficients


# plot diagnostics with model applied to test data
par(mfrow=c(1, 3))  # divide graph area in 3 columns

#predicting the test data with the train dataset
TestPred <- predict(train.model_4, M1.test)  # use predict()  to check model on testData

#finding out the misfit to the dataset
mis_fit_to_testData <- TestPred-M1.test$bth  # these are the residuals for the test data fit
#finding out the mean to the dataset
mean((TestPred-M1.test$bth)^2) # compare to simpler model below
#plotting the test and the preicted values in the dataset
plot(M1.test$bth,TestPred) # plot predicted med val ~ actual 
#plottig the abline in the dataser
abline(0,1,col="red")


#finding out the correlation in the dataset
cor(M1.test$bth,TestPred) # check correlation 
plot(M1.test$bth,mis_fit_to_testData) # look for unwanted pattern in residuals
abline(0,0,col="red")

#we can find that there is the normallity in the dataset
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction

qqline(mis_fit_to_testData,col="red")



#