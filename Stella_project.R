#what is the aim of the project
#The aim is to
#investigate the relationship between WDI indicators and the severity of the COVID-19 pandemic.
View(project_data)
#removing the na values from the the project
project_data <- na.omit(project_data)
project_data

#viewing the dataset after removing the na values
View(project_data)


#first finding the summary of the dataset
summary(project_data)

#plotting the histogram
attach(project_data)

hist(project_data$Covid.deaths,prob=F) #as we can see the covid deaths does not follow the normal distriburion curve
#loading the car library to see the qqplot of the curve
library("car")
#loading the required qq plot form the data
qqPlot(project_data$Covid.deaths,main="number of the covid deaths") #here we can see we have indentified two ouliers in the data

#now we are going to plot the boxplot 

boxplot(project_data$Covid.deaths, ylab= "covid deaths across the region")
rug(jitter(project_data$Covid.deaths),side = 2)
abline(h=mean(project_data$Covid.deaths))
#after plotting the box plot we can see that the mean line slightly up than the median line
#it means that there are greater number number of region present where there is more deaths than than the average rate accross the globe


#loading the library to find or convert the data of covid.deaths  




attach(project_data)

#finding the correlation between different variables
cor(cbind(project_data$Covid.deaths,project_data$Life.expec,project_data$Health.exp.capita))


#using the scatter plot to analyze the correlation between different variables
pairs(~Covid.deaths+Life.expec+Health.exp.capita)

#finding correlation amoung covid.deathds and life.expec
cor(Covid.deaths,Health.exp.capita, use = "complete.obs")



library(corrplot)
corrplot(project_data[,3:18], method="circle")

#detaching the project data from the dataframe
detach(project_data)


project_data$`Country Name`<- as.integer(project_data$`Country Name`)


#plotting the correlation matrix in a different way
M <- project_data[ , -c(1,2,20)]
corrplot(M, method = "ellipse")


corrplot(M, type="upper", order="hclust", 
         p.mat = p.mat, sig.level = 0.01, insig = "blank")#getting error in this code 

#starting out with finding the correct dataset for the above model
lm.a1 <- lm(Covid.deaths ~. ,data=M)#finding multiple linear regression aganist every other varible in the dataset
summary(lm.a1)

#finding the generalised linear model and using the poisson link function in it
summary(glm(Covid.deaths ~. ,data=M,family = poisson(link = log)))

#the results are kind of weired , when using the multiple linear regression the we are only getting one 
#significant varible and when using the generalised linear model i am gettig almost all the significance varible
#now the question arrises how to find the best model for the data set

#now we are going us the AIC to find the most significance model for our variable and use to remove some varible
#form the model
final.lm <- step(lm.a1)
summary(final.lm)
#so after calculating the aic of the model and the summary of the model , i think i can negelect the other variable
#from the model and can focus on only the significance variable provided by the model



#so after doing all this now I am going to foucs my work on the anova table()
#why I am running it to find the unnessary variable from my model and way to do is that is 
#first calculate the linear regression model from the whole variable and then use the anova to 
#filter out the unnecessay variable from our modlel
#that is what we are going to do in this
anova(lm.a1)

#so after finding the results of the anova table we can say that, we can some variable from the 1st linear 
#regression model


#here is the start of regression tree
install.packages("rpart")
library(rpart)


rt.a1 <- rpart(Covid.deaths ~., data = M)
rt.a1


#using the pretty form of tree
plot(rt.a1)



#making a function to calculate the cube of the numbers
comp_sum <- function(n){
  num= 1:n
  sum_zz <- sum(num^3)
  return(sum_zz)
}

comp_sum(5)

