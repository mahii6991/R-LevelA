#QUESTION 1
#required to load all the libraries
library(dplyr)
library(factoextra)
library(gridExtra)
library(tidyverse)
library(MASS)
library(ISLR)
library(factoextra)
library(vtable)
library(car)
library(InformationValue)
library(nnet)
#loading the project data
project_data <- read_csv("project_data.csv")
#first cleaning the last row of the dataset
project_data <- project_data[-186,]
#removing the categorical values form the dataset and only keeping the contineous varible 
project_data_num <- project_data[-c(1,2,20)]
#filling the na values with the mean values
library(tidyr)
no_mis_data <- replace_na(project_data_num,as.list(colMeans(project_data_num,na.rm=T)))
#removing the datacolumns form the table to make it cleaner 
project_data_3 <-  project_data[,c(1,2,20)]
#now joining again the dataset to make the complete table
full_project_data <- cbind(project_data_3,no_mis_data)
View(full_project_data)
#filling out the values in the full project dataset to trying to complete it as a whole
full_project_data$Continent <- full_project_data$Continent %>% replace_na("Australia/Oceania")
#need to remove the na values from the comp.education dataset so first need to convert it in numeric form from character
full_project_data$Comp.education[is.na(full_project_data$Comp.education)] = mean(full_project_data$Comp.education,na.rm = T)
#after cleaning the dataset properly we have 185 colums and 20 variables
summary(full_project_data) #finding the summary of the dataset
#plotting the histogram
hist(full_project_data$Covid.deaths)
#STARTING OF QUESTION 2
#now starting with the clustering task
#first starting with scaling the dataset in R
data_1 <- scale(full_project_data_1[,-1])
#but first set.seed
set.seed(123)
#making the first cluster
k2 <- kmeans(data_1, centers = 2, nstart = 20)
#making the secound cluster
k3 <- kmeans(data_1, centers = 3, nstart = 20)
#making the third cluster
k4 <- kmeans(data_1, centers = 4, nstart = 20)
#now plotting the cluster 1 and visualizing it on the chart
fviz_cluster(k2, data = data_1)
#now plotting the different cluster 2 and visualizing the plot
fviz_cluster(k3, data = data_1)
#now plotting the different cluster 3 and visualizing the plot
fviz_cluster(k4, data = data_1)
#now putting all the plots into one big diagram 
f1 <- fviz_cluster(k2, geom = "point", data = data_1) + ggtitle("k = 2")
f2 <- fviz_cluster(k3, geom = "point", data = data_1) + ggtitle("k = 3")
f3 <- fviz_cluster(k4, geom = "point", data = data_1) + ggtitle("k = 4")
#loading the library gridExtra to make one big clusters
library(gridExtra)
grid.arrange(f1, f2, f3, nrow = 2) #arranged the figure into two rows with big figures
#need to try the hirarchial clustering
#first finding the average distance between the clusters using the euclidean distance
dist_mat <- dist(data_1, method = 'euclidean')
#now using the dendogram function on the dataset to view the clusters
hclust_avg <- hclust(dist_mat, method = 'average') #plotting the histogram using average method
plot(hclust_avg)#plotting it
rect.hclust(hclust_avg , k = 4, border = 2:6)#finding the borders of clusters
abline(h=6.5, col="blue")#plotting the abline 
#QUESTION 3
#first I need to convert the covid_deaths into a binary variable
final_data$Covid.deaths.bin <- ifelse(final_data$Covid.deaths>median(final_data$Covid.deaths), 1, 0) #converted my dataset into the binary variable
#STARTING OUT WITH ALL THE VARIABLE AND THEN REFINING IT TO FEW
#using the logistic regression model on the dataset to predict the high COVID caualties
glm_1 <- glm(Covid.deaths.bin ~  Comp.education + Life.expec+ Elect.access + Net.nat.income       
            + Net.nat.income.capita + Mortality.rate + Primary + Pop.growth  + Pop.density + Pop.total + Health.exp.capita + Health.exp           
             + Unemployment + GDP.growth+ GDP.capita+ Birth.rate+ Water.services,data = final_data,family = binomial)
#training the model with few variable and removed the insignificant variables from the older model and only training the model on the updated model
set.seed(1)
#splitting the dataset into 60%  and 40%
row.number = sample(1:nrow(final_data_2), 0.6*nrow(final_data_2))
train = final_data_2[row.number,] #training data
test = final_data_2[-row.number,]#testing data
dim(train)# finding out dimension
dim(test)#finding out dimension
#now I am going to run the model on the train data set 
glm_4 <- glm(Covid.deaths.bin ~  Comp.education + Life.expec+ Elect.access     
             + Mortality.rate + Health.exp.capita + Health.exp           
             + Unemployment + GDP.growth+ Water.services,data = train,family = binomial)
#finding the summary of the train dataset
summary(glm_4)
#checking for collinearity
car::vif(glm_4)
###Predict for training data and find training accuracy
pred.prob = predict(glm_4, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, train$Covid.deaths.bin) #finding the table
#hence my model ran successfully and provided me the confusion matrix
#now I am going to calculate the accuracy of the model
(43+47)/111 #so finally I can say that I am getting the 81% accuracy in the model when running it with the train
#now I am going to run my dataset on the test data set and try to compare both the model and evaluate them
pred.prob = predict(glm_4, newdata= test, type="response")
pred.prob = ifelse(pred.prob > 0.5, 1, 0)
table(pred.prob, test$Covid.deaths.bin)
#finding out the accuracy of the model on the train dataset and finding out the accuracy
(28+34)/74 #so after running my model , I am getting the accuracy of the 83% on the test dataset
#loading the library to find out the accurarcy measures of the model
library(InformationValue)
misClassError(pred.prob, test$Covid.deaths.bin,threshold=optimal)
#finding the specificity of model
specificity(pred.prob, test$Covid.deaths.bin)
#finding the sensitivity  of the model
sensitivity(test$Covid.deaths.bin,pred.prob)
#plotting the roc curve for the model
plotROC(test$Covid.deaths.bin,pred.prob)
#STARTING OF THE QUESTION 5
#Splittting the response variable into 4 categorical variables
final_data$Covid.deaths <- ifelse((final_data$Covid.deaths < 167) , 0 ,  
                                     ifelse((final_data$Covid.deaths < 998), 1,
                                            ifelse((final_data$Covid.deaths < 1830), 2, 3)))
#viewing the datset and considering if some manupalation is required or not
view(final_data)

#manupulating my dataset before applying my learning my algorithm to this dataset, removing some columns like country name, continents and covid deaths bin
final_data_3 <- final_data[,-c(2,3,21)]
#setting the seed
set.seed(1)
#spitting my dataset into the training and testing 
row.number = sample(1:nrow(final_data_3), 0.6*nrow(final_data_3))
train = final_data_3[row.number,] #spitting the dataset into training dataset
test = final_data_3[-row.number,] #spitting the dataset into testing dataset
dim(train) #finding out the dimensions training 
dim(test)#finding out dimenstion of testing dataset
#now running the lda fit model to my dataset and seeing the results
lda.fit <- lda(Covid.deaths ~ Comp.education + Life.expec+ Elect.access     
               + Mortality.rate + Health.exp.capita + Health.exp           
               + Unemployment + GDP.growth+ Water.services,data = train)
plot(lda.fit)
##Predicting training results.
predmodel.train.lda = predict(lda.fit, data=train)
table(predmodel.train.lda$class, train$Covid.deaths)
#by checking the accuracy of the lda.fit on the training data , we are getting somewhere around the 70.27%
#now need to check 
#attach(test)
predmodel.test.lda = predict(lda.fit, newdata=test)
table(predmodel.test.lda$class, test$Covid.deaths)
#on the testing data set I am getting the accuracy of 54.05%
#running the qda.fit model
qda.fit <- qda(Covid.deaths ~ Comp.education + Life.expec+ Elect.access     
               + Mortality.rate + Health.exp.capita + Health.exp           
               + Unemployment + GDP.growth+ Water.services,data = train)

#analyzing the summary of qda.fit model
qda.fit
##Predicting training results.
predmodel.train.qda = predict(qda.fit, data=train)
table(predmodel.train.qda$class, train$Covid.deaths)
#by checking the accuracy of the lda.fit on the training data , we are getting somewhere around the 75.67%
#now need to check 
#attach(test)
predmodel.test.lda = predict(lda.fit, newdata=test)
table(predmodel.test.lda$class, test$Covid.deaths)
#on the testing data set I am getting the accuracy of 54.05%
#loading the library of the multiclass classification problem
library(nnet)
#library(tidyverse)
#running it / and it ran, it provided the results now the thing is to draw the contingency table for this class
glm_5 <- nnet::multinom(Covid.deaths ~ Comp.education + Life.expec+ Elect.access     
                          + Mortality.rate + Health.exp.capita + Health.exp           
                          + Unemployment + GDP.growth+ Water.services,data = train,family = binomial)
#so the multiclass classification ran and now we need to 
# Summarize the model
summary(glm_5)
# Make predictions
predicted.classes <- glm_5 %>% predict(test)
head(predicted.classes)
# Model accuracy
mean(predicted.classes == test$Covid.deaths)
#so on the testing data we are getting the accuracy of 45% when running the logistic regression for multiclass classification model

#In this project we have implemented-
#linear regression
#logistic regression
#LDA, QDA and logistic regression
#summary of our results which is better for the prediction
