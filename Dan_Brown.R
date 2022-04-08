##  Agresti, "Categorical Data Analysis" (2013, p. 91) Fisher's Tea Drinker
##  A woman colleague of Fisher, Dr Muriel Bristol,
##  claimed to be able to distinguish whether milk or
##  tea was added to the cup first.  To test this, she was given 8 cups of
##  tea, in four of which milk was added first.  The null hypothesis
##  is that there is no association between the true order of pouring
##  and the Muriel's guess, the alternative that there is a positive
##  association (that the odds ratio is greater than 1).
TeaTasting <-
  matrix(c(3, 1, 1, 3),
         nrow = 2,
         dimnames = list(Guess = c("Milk", "Tea"),
                         Truth = c("Milk", "Tea")))
TeaTasting
fisher.test(TeaTasting, alternative = "greater")
## => p = 0.2429, association could not be established

TeaTasting  # detailed calculation for the Fisher tea taster 
library(Combinations)
Milk_guesses_from_true_Milk <- choose(4,3)
Milk_guesses_from_true_Milk
Milk_guesses_from_true_Tea <- choose(4,1)
Milk_guesses_from_true_Tea
Milk_guesses_total <- choose(8,4)
Milk_guesses_total
Probability_Fishers_Tea_taster_data <- 
  Milk_guesses_from_true_Milk*Milk_guesses_from_true_Tea/Milk_guesses_total
Probability_Fishers_Tea_taster_data  # to get the p value inc the 4,0,0,4 case:

# complete p value calculation for the Fisher tea tasting 
choose(4,3)*choose(4,1)/choose(8,4)+choose(4,4)*choose(4,0)/choose(8,4)

# exactly the same as given above
Via_R <- fisher.test(TeaTasting, alternative = "greater")
Via_R$p.value


#=========================================================================================================
  
library(VGAM)
# Here we assess the ML estimator for the Rayleigh parameter 

x <- rrayleigh(1e5,1)  # plots an example Rayleigh distribution
hist(x, 100, freq = FALSE)

n <-3 # n samples ; true scale parameter = 1
rr <- rrayleigh(n,1)
sum(rr^2)/(2*n)  # try the estimator for the population value 1 

estimator <- c(rep(0,10000)) # generate 10,000 estimates on n samples
for(i in 1:10000){rr <- rrayleigh(n,1);
estimator[i] <- sum(rr^2)/(2*n)}
hist(estimator)       # plot the distribution of the estimates  
abline(v=1,col="red",lwd=3)
mean(estimator) # mean of the estimates approaches 1 


#============================================================================================================

#running the code provided for the danbrown class- code-1
# Adapted code for fig 1.1 of the book 
pop<-c(1:5) 
n <- 4  # set the sample size 
# We need a vector to store averages:  
ave<-rep(0,times=100)  
# Then a loop generates samples and their averages:  
for(i in 1:100){ 
  s<-sample(pop,n,replace=TRUE) 
  ave[i]<-mean(s) 
} 
# Finally the results are plotted:
hist(ave,xlim=c(0,5.0))  
mean(ave) # calculate the mean 
sd(ave)  # calculate the standard deviation 

#=====================================================================

#running the different code- code-2 - simple EDA for the mtcars dataset
data(mtcars)
?mtcars

names(mtcars)
ND <- mtcars[,c(1,2,4,6)]
head(ND)
str(ND)
ND$mpg

X <- ND$mpg
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

summary(ND$mpg)  # quartiles and boxplot
boxplot(ND$mpg)
sort(ND$mpg) #check quartiles against ordered data here 

# now we create a categorical variable from mpg 
fuel_economy <- as.factor(ifelse(ND$mpg <= 15.43,"Poor",
                                 ifelse(ND$mpg <=22.8 , "Normal", "Good"))) # create mpg factor
ND$fuel_economy <- fuel_economy # add the new factor to the dataframe
head(ND)
str(ND)
summary(ND)
levels(ND$fuel_economy)  # looks the levels of categorical variables 
table(ND$fuel_economy)  # table is a useful generic function 

tapply(ND$hp,ND$fuel_economy,mean)  # check mean hp over each mpg level
# we see how hp drops with better mpg

boxplot(ND$mpg~ND$fuel_economy,main="mpg against fuel ecomomy ") # as expected 
boxplot(ND$hp~ND$fuel_economy,main="hp against fuel ecomomy ") 

boxplot(ND$wt~ND$fuel_economy,main="disp against fuel ecomomy ") # skew boxplot for Poor fuel economy
hist(ND$wt[ND$fuel_economy=="Poor"]) # here we see why selecting just Poor fuel economy cars

plot(ND$mpg~ND$wt)  # see how mpg varies with weight of car
abline(lm(ND$mpg~ND$wt),col="red")  # best fit line added
cor(ND$mpg,ND$wt) # correlation coefficient 

plot(ND$mpg~ND$hp)  # see how mpg varies with hp of car
abline(lm(ND$mpg~ND$hp),col="red")  # best fit line added 
cor(ND$mpg,ND$hp) # correlation coefficient 
#==========================================================================================

#Code -3- A task to plot the sample variance in the dataset and plotting a histogram out of it.


Means_vec <- rep(0,1000)  # storage vector of 1000 means of a uniformly distributed samples of size "sample_size"

sample_size <- 5  # set the sample size out of [0,1] ; uniform distribution

runif(sample_size, min = 0, max = 1) # generates sample_size RVs with uniform distribution
mean(runif(sample_size, min = 0, max = 1)) # mean of these RVs with uniform distribution

for(i in 1:1000){      # loop taking 1000 samples of size sample_size, record means each time
  Means_vec[i] <- mean(runif(sample_size,min = 0, max = 1))
}

# now plot a probability density for the sample variances
h <- hist(Means_vec,xlim=c(0,1.0),ylim=c(0,5),freq=FALSE,main="PDF for sample mean" )
h$equidist
abline(v=0.5,col="red",lwd=3) # plots a red theoretical line 
# Add a Normal Curve to the histogram this plots a pdf (total area = 1)
scaled_norm <- dnorm(h$mids,mean=mean(Means_vec),sd=sd(Means_vec))
lines(h$mids,scaled_norm)
points(h$mids,scaled_norm,pch = 20, col = "blue" )

1.96* sd(Means_vec)# estimated max error in the sample var compared to pop var
lower_bound <- 0.5-1.96* sd(Means_vec)
upper_bound <- 0.5+1.96* sd(Means_vec)
abline(v=lower_bound,col="green",lwd=3) # plots a lower bound 
abline(v=upper_bound,col="green",lwd=3) # plots an upper bound 

# lab task for you (do it in RStudio)
# Adapt the above code to investigate the distribution of the  sample variance.
# Compared this in a histogram to the population variance for a uniform distribution [0,1] 
# which is equal to 1/12 or 0.083333333333. 
# Also try to show numerically in code that the pop variance in this case is 1/12. 
# (hint try to treat the continuous RV over [0,1] as a discrete RV and apply a formula you know)
# investigating the variance of sample variance

vars_vec <- rep(0,1000)  # storage vector of 1000 variances of a uniformly distributed samples of size "sample_size"

sample_size <- 7  # set the sample size out of the numbers 0 to  1 to sample_size; uniform distribution

runif(sample_size, min = 0, max = 1) # generates sample_size RVs with uniform distribution [0,1]
var(runif(sample_size, min = 0, max = 1)) # var of these RVs with uniform distribution

for(i in 1:1000){      # loop taking 1000 samples of size sample_size, record var each time
  vars_vec[i] <- var(runif(sample_size,min = 0, max = 1))
}

h <- hist(vars_vec,xlim=c(0,0.2),freq=FALSE,main="PDF for sample variance" )
h$equidist
abline(v=1/12,col="red",lwd=3) # plots a red theoretical line 
# Add a Normal Curve to the histogram 
scaled_norm <- dnorm(h$mids,mean=mean(vars_vec),sd=sd(vars_vec))
lines(h$mids,scaled_norm)
points(h$mids,scaled_norm,pch = 20, col = "blue" )

1.96* sd(vars_vec)# estimated max error in the sample var compared to pop var
lower_bound <- 1/12-1.96* sd(vars_vec)
upper_bound <- 1/12+1.96* sd(vars_vec)
abline(v=lower_bound,col="green",lwd=3) # plots a lower bound 
abline(v=upper_bound,col="green",lwd=3) # plots an upper bound 

var(runif(100000000,min = 0, max = 1))  # very large sample gives an estimate close to population values
1/12  # known population variance 

# better method : 
bar_width <- 0.001  # numerical integration to find the variance 
x_mid_pts <- seq(0.0005,1,0.001)  # 0 to 1 divided into 1000 intervals, these are mid points 
x_mid_pts 
sum(bar_width*x_mid_pts^2)-0.5^2  # correct to 6 significant figures; pop var =0.08333333333=1/12 
1/12  # known population variance 

#===========================================================================================================

#code-3- Binomial code for the data exploration

#1a 
dbinom(3,size=3,prob=0.5)
#1b
dbinom(2,size=3,prob=0.5)  
#1c
1-pbinom(0,size=3,prob=0.5)  #or
1-dbinom(0,size=3,prob=0.5)  
#1d
pbinom(1,size=3,prob=0.5)  

#2a
dbinom(1,size=4,prob=0.2)  
#2b
dbinom(0,size=4,prob=0.2)  
#2c
pbinom(1,size=4,prob=0.2)  

#3 use formulae for mean and var see book 

#4 
dbinom(7,size=10,prob=0.8)  

#5
# mean is 
500*0.05
#sd is
sqrt(500*0.05*(1-0.05))
# so 25 +/- 4.87

#6
#P(2)= (1-1/4)/5 = (3/4)/5
pbinom(1,size=6,prob=(3/4)/5)  

#==========================================================================================================
#code - 4 - poisson distribution 

#1a
dpois(2,lambda=5)
#1b
ppois(2,lambda=5)
#1c
1-ppois(10,lambda=5)  # 1.4% chance this will happen 

#2a
dpois(2,lambda=2)
ppois(1,lambda=2)
1-ppois(1,lambda=2) # excludes case 0 and 1 

#3a  should read exactly 3 
# first the binomial way 
dbinom(3,size=2000,prob=0.001)
# then by poisson approximation 
dpois(3,lambda=2000*0.001)  # note this value is very close to exact binomial value

#3b
1-ppois(2,lambda=2000*0.001) 
#or
1-pbinom(2,size=2000,prob=0.001)

#==========================================================================================================
#code - 5 - Application of binomial and poisson distribution

# Objectives 
# 1/ simulate various binomials with given n and p; try varying p alone 
# 2/ simulate a Poisson fitted to that binomial 
# 3/ simulate a normal fitted also fitted to the binomial 
# 4/ use QQ plots to show good or bad fits(small np=mean for Poisson, large mean for normal)

plot.new() # wipes out all plots 
rm(list = ls()) # wipes out R environment 
cat("\014") # clears console 

number_samples <- 1000 
binomial_trials <- 100  # n
binomial_prob <- 0.02  # p  ; keep p << 1 to avoid Poisson upper tail cutoff !!!

binom_data <- rbinom(number_samples,size=binomial_trials,prob=binomial_prob)  # random binomials
mean(binom_data)
counts_list <- table(binom_data)
counts_list
str(counts_list)
counts_binom <- as.numeric(counts_list) # extracting numbers from the table 
x_binom <- as.numeric(names(counts_list))
x_binom
counts_binom

h <- plot(counts_binom~x_binom ,main="simulated binomial counts",pch = 20, col = "red" ,lwd=5)
# Add a binom Curve 
scaled_binom <- dbinom(x_binom,size=binomial_trials,prob=binomial_prob)*sum(counts_binom)
sum(scaled_binom)  # check
lines(x_binom,scaled_binom)  # add lines points of population binomial 
points(x_binom,scaled_binom,pch = 20, col = "blue",lwd=5 )



###########################################################



poisson_data <- rpois(number_samples,binomial_trials*binomial_prob)  # same pop mean as the binomial (np)
mean(poisson_data)
counts_list <- table(poisson_data)
counts_list
counts_poisson  <- as.numeric(counts_list)
x_pois <- as.numeric(names(counts_list))
counts_poisson
x_pois

# now plot the frequency
h <- plot(counts_poisson~x_pois ,main="simulated Poisson counts",pch = 20, col = "red" ,lwd=5)
# Add a Poisson Curve 
scaled_pois <- dpois(x_pois,lambda=mean(poisson_data))*sum(counts_poisson)
sum(scaled_pois)
lines(x_pois,scaled_pois)
points(x_pois,scaled_pois,pch = 20, col = "blue",lwd=5 )


########

# first re-plot the binomial samples then fit a Poission to it
h <- plot(counts_binom~x_binom ,main="fitting a poisson to the binomial",pch = 20, col = "red" ,lwd=5)
# Add a best fit poisson  Curve 
fit_pois <- dpois(x_binom,lambda=mean(binom_data)) # fitted to sample mean of binomial data
sum(fit_pois) # its normalised 
fit_pois <- number_samples*fit_pois/sum(fit_pois) # scale the Poisson up to match the binomial 
fit_pois
lines(x_binom,fit_pois) # same x values as the binomial but plotting the Poisson
points(x_binom,fit_pois,pch = 20, col = "blue",lwd=5 )

# now we compare the quantiles  for the binomial and its it's fitted Poisson
sim_fit_pois <- rpois(number_samples,lambda=mean(binom_data))  # simulated Poisson with the binomial's mean 
?qqplot
qqplot(binom_data,sim_fit_pois,pch = 20, col = "blue",lwd=5,main="QQ Poisson to Binomial")  # QQ plot to compare culmulatives
abline(0,1,pch = 20, col = "red",lwd=3 ) # adds a 45 degree line (perfect match line)

# now try to compare the binomial quantiles with a fitted normal distribution 
normal_mean <- binomial_trials*binomial_prob # fit normal mean to binomial mean 
normal_sd <- sqrt(binomial_trials*binomial_prob*(1-binomial_prob)) # fit normal sd to binomial sd 
# now we compare the quantiles  for the binomial and its it's fitted Poisson
sim_fit_normals <- rnorm(number_samples,mean=normal_mean,sd=normal_sd)  # simulated Poisson with the binomial's mean 
qqplot(binom_data,sim_fit_normals ,pch = 20, col = "blue",lwd=5,main="QQ Normal to binomial ")  # QQ plot to compare culmulatives
abline(0,1,pch = 20, col = "red",lwd=3 ) # adds a 45 degree line (perfect match line)
plot(counts_binom~x_binom ,main="fitting a normal to the binomial",pch = 20, col = "red" ,lwd=5)
lines(x_binom,dnorm(x_binom,normal_mean,normal_sd)*number_samples) # same x values as the binomial but plotting the Poisson
points(x_binom,dnorm(x_binom,normal_mean,normal_sd)*number_samples,pch = 20, col = "blue",lwd=5 )

#==========================================================================================================
#code - 6 - p- value and the t-test of the data sets

dev.off()# clear out the past !!!
rm(list = ls())
cat("\014")
# text messages for use later: 
p_low <- "We Reject H0 which is fine since know H1 is true in this case"
p_high <-   "We accept the NULL hypothesis that mu=5.0; a type II error since we know H1 is true in this case"

sample_size <- 50 # set the sample size; try 50 and 60 and check the p values also high and low sample sizes 
set.seed(42)  # fixes the "Random" number generator; check for type II error 

sample_set<-rnorm(sample_size ,mean=6.0,sd=3.0)  # set up sample_size  normals RV, mean of 6.0 sd of 3.0 
s_mean<-mean(sample_set);s_mean  # check sample mean and sd
s_sd<-sd(sample_set);s_sd
s_sd/sqrt(sample_size )  # estimate for the sd of the sample mean 

hist(sample_set,main="Two tail test",xlab='Sample values',cex.lab=2,cex.axis=1.5,nclass=13)  #plot sample distribution
points(c(5,5),c(0,20),type='l',lty=5,col="red",lwd=4)  # plot line for NULL hypothesis 
text(5,5,'H1:  mu not 5',cex=2,col="blue")  # add population mean to plot

?t.test  # documentation of this hypothesis test
# two tailed t test; NULL hypothesis is H0: pop mean mu=5; alternative H1: pop mean mu not equal 5
t_test_object_A <-t.test(sample_set,mu=5)  
t_test_object_A  # full output 
t_test_object_A$conf.int  # extract a confidence range 
t_test_object_A$p.value  # extract the final p value 
if(t_test_object_A$p.value<=0.05){p_low}else{p_high}  # printout test result 

# explicit calculation for the p value
t_value <- (s_mean-5)/(s_sd/sqrt(sample_size )) # calculate the t statistic 
t_value
2*pt(-t_value,sample_size-1) # calculate the two tail probability for t_value
t_test_object_A$p.value # compare to p value above (exactly the same)


# explicit calculation for the "Power":   P(H0 rejected|H1 true)
?qt # inverse for the t test given prob find the t stats corresponding 
#power calculation (the long way !):
# assume pop sd of 3 and H1 true with pop mean =6
ncp <- 1.0/(3/sqrt(sample_size)) # ncp is the assumed shift of sample mean 
t_limit_95_two_tail <- qt(0.975,df=sample_size-1)
prob_accept_H0 <- pt(t_limit_95_two_tail,df=sample_size-1,ncp=ncp)-
  pt(-t_limit_95_two_tail,df=sample_size-1,ncp=ncp)
1-prob_accept_H0  # the power compare to using power.t.test() below


?power.t.test  # powerful R function, just set the argument to calculate to NULL 

# first compute power specify all other arguments  
power.t.test(power=NULL,n=sample_size,delta=1.0,sd=3,sig.level=0.05,
             type="one.sample",alternative="two.sided",strict = TRUE)

# now specify power and put sample size as NULL then compute sample size
power.t.test(power=0.6370943,n=NULL,delta=1.0,sd=3,sig.level=0.05,
             type="one.sample",alternative="two.sided",strict = TRUE)

# now specify all except delta the true difference in  means 
power.t.test(power=0.6370943,n=sample_size,delta=NULL,sd=3,sig.level=0.05,
             type="one.sample",alternative="two.sided",strict = TRUE)

################ class questions ###############################
#1 how does power change with sample size  ?
#2 how does power change with significance level ?
#3 how does power change with a change to a one tail test ?
#4 what sample size is needed to have a power of 0.95 (all other parameters unchanged)

# challenge question: modify the code above for the long way to 
# compute power to a one tail test and confirm your answer using power.t.test()

#==========================================================================================================
#code - 7 - categorical kaggle

dev.off()# clear out the past !!!
rm(list = ls())
cat("\014")

# https://www.kaggle.com/mysarahmadbhat/lung-cancer/activity  
# see https://statsandr.com/blog/data-manipulation-in-r/#creating-factors

# Set working directory
setwd("C:/Users/staff/Desktop/MA334/MA334_project")

#Import CSV
C_data_original <- read.csv("survey lung cancer.csv")

str(C_data_original)
table(C_data_original$LUNG_CANCER)  # imbalanced data 

library(ROSE)
?ovun.sample # this performs over or under sampling for unbalanced data 
C_data_balanced <- ovun.sample(LUNG_CANCER~.,data=C_data_original,
                               p=0.5,seed=1, method="both")$data
str(C_data_balanced)  
table(C_data_balanced$LUNG_CANCER)  # now balanced 

# quicker to make all at once to categorical
C_data_final <- as.data.frame(lapply(C_data_balanced, as.factor)) 
str(C_data_final$AGE)  # now wrong 
C_data_final$AGE <-C_data_balanced$AGE
str(C_data_final$AGE) # now right again to continuous 
levels(C_data_final$LUNG_CANCER)  # checks levels of a categorical variable 

str(C_data_final)
names(C_data_final)  # names of variables 
nrow(C_data_final) # data records 
table(C_data_final$LUNG_CANCER) # balanced now 
hist(C_data_final$AGE)  # age profile overall

table(C_data_final$LUNG_CANCER,C_data_final$SMOKING)  # 2 by 2 contingency 
?fisher.test
fisher.test(table(C_data_final$LUNG_CANCER,C_data_final$SMOKING))   
chisq.test(C_data_final$LUNG_CANCER,C_data_final$SMOKING)

boxplot(C_data_final$AGE~C_data_final$LUNG_CANCER)  # age profile with/without cancer
# t test for difference of mean see p value
t.test(C_data_final$AGE[C_data_final$LUNG_CANCER=="YES"],C_data_final$AGE[C_data_final$LUNG_CANCER=="NO"])

table(C_data_final$LUNG_CANCER,C_data_final$YELLOW_FINGERS)
fisher.test(table(C_data_final$LUNG_CANCER,C_data_final$YELLOW_FINGERS)) # two tests for independence
chisq.test(C_data_final$LUNG_CANCER,C_data_final$YELLOW_FINGERS)

#logistic model trying to predict cancer by the smoking category only (fails)
model_smoking <- glm(LUNG_CANCER~SMOKING,family=binomial(link='logit'),data=C_data_final) # logistic model fit
summary(model_smoking) # min_model is the best model found check p values for each variable 
S_K <- table(model_smoking$fitted.values,C_data_final$LUNG_CANCER)
S_K
fisher.test(S_K) # odds ratio not significantly different from 1 

#logistic model trying to predict cancer by the YELLOW_FINGERS category only 
model_YELLOW_FINGERS<- glm(LUNG_CANCER~YELLOW_FINGERS,family=binomial(link='logit'),data=C_data_final) # logistic model fit
summary(model_YELLOW_FINGERS) # min_model is the best model found check p values for each variable 

hist(model_YELLOW_FINGERS$fitted.values)
table(model_YELLOW_FINGERS$fitted.values)
Y_F <- table(model_YELLOW_FINGERS$fitted.values>0.5,C_data_final$LUNG_CANCER) # confusion table
Y_F
fisher.test(Y_F) # two tests for independence
chisq.test(Y_F)


# full logistic model 
model <- glm(LUNG_CANCER ~.,family=binomial(link='logit'),data=C_data_final) # logistic model fit
min_model <- step(model) # this finds a best fit model by selecting and rejecting variables 
summary(min_model) # min_model is the best model found check p values for each variable 

hist(min_model$fitted.values)
boxplot(min_model$fitted.values~C_data_final$LUNG_CANCER) # fitted.values are probabilities 
C_full <- table(min_model$fitted.values>0.5,C_data_final$LUNG_CANCER) # final result 
C_full

#===========================================================================================================
#code -7- usuage of dplyr library

# https://www.cancerresearchuk.org/health-professional/cancer-statistics/statistics-by-cancer-type/lung-cancer/survival#heading-Three

getwd()
library(dplyr)  # dplyr is used to manipulate data
all_data <- read.csv("adultcancersurvivaltables.csv") # serious data from cancer research uk
head(all_data)
names(all_data)
table(all_data$Survival.time..years.) # example 

library(dplyr)
library(stringr)
library(xlsx)

selected_data <- select(all_data,-4,-7,-8,-11,-12) # remove variable 4,7,8 etc 
names(selected_data)
table(selected_data$Stage)
Cohort <- selected_data %>% filter(Cancer.site=="Lung",  #using the pipe notation
                                   Sex=="Women",Age.group=="55-64",
                                   Stage=="Stage 1"|Stage=="Stage 2"|Stage=="Stage 3"|Stage=="Stage 4")
table(Cohort$Stage)
Cohort
# pipe notation, removing the "," from the numbers :
0.5*Cohort$Number.of.patients%>%str_replace_all(",","")%>%as.numeric()%>%sum
#same as above without pipe notation:
0.5*sum(as.numeric(str_replace_all(Cohort$Number.of.patients,",",""))) 

Cohort_results <- Cohort %>% select(-1,-2,-3,-6) # further removals 
Cohort_results # reduced results 
write.csv(Cohort_results, file = "Cohort_results.csv") #  write out as a .csv

#=============================================================================================================

#code 8- working through the titanic dataset

dev.off()# clear out the past !!!
rm(list = ls())
cat("\014")

#The variables are 
str(Titanic)
# Class(1,2,3,Crew); Sex(Male, female); Age(Child, Adult); Outcome(Died, survived). 

Titanic[4,2,1,]  # no child crew members !
Titanic[4,,2,]   # adult crew M/F 

# The various totals are found as follows:
sum(Titanic[,,,])  # total passengers and crew 
sum(Titanic[1:3,,,])  # total passengers 
sum(Titanic[4,,,])  # total  crew 
sum(Titanic[1:3,,2,])#No. of adult passengers
sum(Titanic[1:3,,2,2])#No. of adult passengers who survived
sum(Titanic[4,,2,2])#No. of adult crew who survived
sum(Titanic[1:3,1,2,])#No. of adult male passengers
sum(Titanic[4,1,2,])#No. of adult male crew
sum(Titanic[1 ,1,2,])#No. of male adult passengers class 1
sum(Titanic[2,1,2,])#No. of male adult passengers class 2
sum(Titanic[3,1,2,])#No. of male adult passengers class 3
sum(Titanic[1 ,1,2,2])#No. of male adult passengers class 1  who survived
sum(Titanic[2,1,2,2])#No. of male adult passengers class 2  who survived
sum(Titanic[3,1,2,2])#No. of male a adult passengers class 3  who survived

#To easily calculate G^2 we need the DescTools library:
library(DescTools)

Titanic[4,1,2,2]#No. of adult male crew who survived
sum(Titanic[1:3,1,2,2])#No. of adult male passengers who survived
Titanic[4,1,2,1] #No. of adult male crew who died
sum(Titanic[1:3,1,2,1])#No. of adult male passengers who died
M<-as.table(rbind(c(192,146),c(670,659)))
M
?GTest
LR_test <- GTest(M) #LR test 
LR_test

#recreate LR test result from bottom up
a <- Titanic[4,1,2,2]  #No. of adult male crew who survived
b <- sum(Titanic[1:3,1,2,2])  #No. of adult male passengers who survived
c <- Titanic[4,1,2,1]   #No. of adult male crew who died
d <- sum(Titanic[1:3,1,2,1]) #No. of adult male passengers  who died
ab <- a+b
cd <- c+d
ac <- a+c
bd <- b+d
abcd <- a+b+c+d  # overall total 

a;LR_test$observed  # for example compare a with the L_test observed 
(ac/abcd)*ab; LR_test$expected # for example compare with the L_test expected 
LR_test$statistic  # reproduce this from the basic a,b,c, etc ....
long_stat <- 2*(a*log(a/(ab*ac/abcd))+b*log(b/(ab*bd/abcd))+
                  c*log(c/(cd*ac/abcd))+d*log(d/(cd*bd/abcd)))
long_stat
LR_test$p.value # reproduce this as a chisq tail probability
1-pchisq(long_stat,df=1)

fisher.test(M)  # alternative tests for independence 
chisq.test(M)


#class QUESTION: Confirm the pearson's residual 4.5 in table 6.8 of the book
########### answer #####################################
Pas_male <- Titanic[1:3,1,2,]
Pas_male[1,2]  #  recorded male in class one survived 
s1 <- sum(Pas_male[1,1:2]) 
s2 <- sum(Pas_male[1:3,2])
s_tot <- sum(Pas_male[,])
exp_val <- s1*s2/s_tot
(Pas_male[1,2]-exp_val)/sqrt(exp_val)  # confirms table 6.8 value


#Class QUESTION : work out the pearson's residuals for the case above and interpret 
###########answer using above a,b,c,ab etc #######################
a_exp <- (ab*ac/abcd)  # cont with above a,b,c, etc 
b_exp <- (ab*bd/abcd)
c_exp <- (cd*ac/abcd)
d_exp <- (cd*bd/abcd)
(a-a_exp)/sqrt(a_exp);(b-b_exp)/sqrt(b_exp);(c-c_exp)/sqrt(c_exp);(d-d_exp)/sqrt(d_exp)
M

#CLASS QUESTION: Did 3rd class female survival differ from 2nd class female survival ?
########  answer ############
# Class(1,2,3,Crew); Sex(Male, female); Age(Child, Adult); Outcome(Died, survived). 
Titanic[2,2,2,2]    #No. of adult 2nd class female passengers  who survived
Titanic[2,2,2,1]    #No. of adult 2nd class female passengers who died 
Titanic[3,2,2,2]    #No. of adult 3rd class female passengers  who survived
Titanic[3,2,2,1]    #No. of adult 3rd class female passengers who died 
M_F <-as.table(rbind(c(80,13),c(76,89)))
M_F
?GTest
LR_test <- GTest(M_F) #LR test 
LR_test
chisq.test(M_F)

#==========================================================================================================

#code-9 - getting to know dplyr better

# Code uses dplyr (load the package once) to prcess a data frame read as a .csv
# its purpose is to get familiarity with some of dplyr package; 
#  see  https://dplyr.tidyverse.org/  for more details on dplyr ####

rm(list = ls())
cat("\014")

library(dplyr)  # use dplyr for dataframe manipulation 

getwd()  #  working directory;  put the csv here or use setwd(.....)
msleep <- read.csv("msleep.csv")  # read in csv 

head(msleep)  # usual loom at the data set 
names(msleep)
str(msleep)

sleepData <- select(msleep, name, sleep_total) # select only two variables 
head(sleepData)

head(select(msleep, -name)) # remove name var from dataframe

head(select(msleep, name:order))  # select 4 vars from name to order 

head(select(msleep, starts_with("sl")))  # select by name text start

filter(msleep, sleep_total >= 16, bodywt >= 1) # filter by values on specified vars

two_orders <-filter(msleep, order %in% c("Perissodactyla", "Primates")) # filtering 
table(two_orders$order)  # only two orders left 

head(select(msleep, name, sleep_total))   # some code 
msleep %>%select(name, sleep_total)%>%head()  # same code in pipe notation

?arrange  # sort function 
msleep %>% arrange(order) %>% head()  # this is all one pipe; "arrange" orders
head(arrange(msleep,order))  # same code without pipe notation 

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>%   # sort by more than one var 
  head()

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)  

#  above avoids the following nested code : use pipes !!!!!
filter(arrange(select(msleep,name, order, sleep_total),order, sleep_total),sleep_total >= 16) 

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, desc(sleep_total)) %>%     # descending order
  filter(sleep_total >= 16)

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total) %>% # mutate adds a new var
  head()

msleep %>% 
  mutate(rem_proportion = sleep_rem / sleep_total, # or two new defined vars
         bodywt_grams = bodywt * 1000) %>%
  head()

msleep %>% 
  summarise(avg_sleep = mean(sleep_total)) # summarise creates stats

msleep %>% 
  summarise(avg_sleep = mean(sleep_total), # multiple stats 
            min_sleep = min(sleep_total),
            max_sleep = max(sleep_total),
            total = n())                   # n() counts up 

msleep %>% 
  group_by(order) %>%   # group_by a var internally to compute functions  
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())

msleep %>% 
  group_by(vore) %>%
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())

msleep %>% 
  group_by(vore) %>%na.omit()%>%        # same as above but na.omit removes "NA"
  summarise(avg_sleep = mean(sleep_total), 
            min_sleep = min(sleep_total), 
            max_sleep = max(sleep_total),
            total = n())

carni_sleep <- msleep %>%filter(vore=="carni")%>%select(sleep_total) 
summary(carni_sleep)  # sleep totals for carniovores
omni_sleep <- msleep %>%filter(vore=="omni")%>%select(sleep_total)  
summary(omni_sleep)  # sleep totals for carniovores
t.test(carni_sleep,omni_sleep)  # no sig diff of mean sleep_total 

which (is.na(msleep$brainwt))  # find index for any NA present 
#  na.omit() just leaves out missing data which in R is coded "NA"

carni_brain <- msleep %>%filter(vore=="carni")%>%select(brainwt)%>%na.omit() 
omni_brain <- msleep %>%filter(vore=="omni")%>%select(brainwt)%>%na.omit() 
carni_brain$brainwt
omni_brain$brainwt
t.test(carni_brain$brainwt,omni_brain$brainwt)  # no sig difference mean brainwt

#===========================================================================================================

#code-10-analyzing the titanic dataset

library(DescTools)
library(dplyr)
if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

getwd()
titanic <- read.csv("titanic.csv")
head(titanic)
names(titanic)
summary(titanic)
str(titanic)

table(titanic$Survived,titanic$Pclass)
GTest(table(titanic$Survived,titanic$Pclass))

table(titanic$Survived,titanic$Sex)

boxplot(titanic$Fare~titanic$Pclass)

hist(titanic$Age)
boxplot(titanic$Age~titanic$Pclass)

fare_2 <- titanic%>%filter(Pclass==2)%>%select(Fare)
fare_3 <- titanic%>%filter(Pclass==3)%>%select(Fare)
par(mfrow=c(1,2))
boxplot(fare_2)
boxplot(fare_3)
t.test(fare_2,fare_3)

titanic%>%group_by(Pclass)%>%summarise(mean_fare=mean(Fare))

titanic%>%group_by(Survived)%>%summarise(mean_fare=mean(Fare))

titanic%>%group_by(Sex)%>%summarise(mean_fare=mean(Fare))

table(titanic$Sex,titanic$Pclass)

titanic%>%group_by(Pclass)%>%summarise(mean_Age=mean(Age))

titanic%>%group_by(Pclass)%>%summarise(no=n())

sum(titanic$Age)/nrow(titanic)

par(mfrow=c(1,2))
Age_3 <- titanic%>%filter(Pclass==3)%>%select(Age)
hist(Age_3$Age)
Age_1 <- titanic%>%filter(Pclass==1)%>%select(Age)
hist(Age_1$Age)
par(mfrow=c(1,1))


?arrange
fares_ordered <- titanic%>%arrange(desc(Fare))
plot(fares_ordered$Fare)
head(fares_ordered)
fares_ordered <- titanic%>%arrange(Fare)
head(fares_ordered)

fare_zero <- titanic%>%filter(Fare==0)
fare_zero
titanic%>%filter(Fare==0,Survived==1)

# logistic model 
str(titanic)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

model <- glm(Survived ~Fare+Sex+Pclass+Age,family=binomial(link='logit'),data=titanic) # logistic model fit
summary(model) # min_model is the best model found check p values for each variable 

model_2 <- glm(Survived ~Sex+Pclass+Age,family=binomial(link='logit'),data=titanic) # logistic model fit
summary(model_2) # min_model is the best model found check p values for each variable 
hist(model_2$fitted.values)
boxplot(model_2$fitted.values~titanic$Survived) # fitted.values are probabilities 
Confusion_mat <- table(model_2$fitted.values>0.5,titanic$Survived) # final result 
Confusion_mat 
model_2$coefficients

model_3 <- glm(Survived ~Sex+Pclass,family=binomial(link='logit'),data=titanic) # logistic model fit
summary(model_3) # min_model is the best model found check p values for each variable 
hist(model_3$fitted.values)
boxplot(model_3$fitted.values~titanic$Survived) # fitted.values are probabilities 
Confusion_mat <- table(model_3$fitted.values>0.5,titanic$Survived) # final result 
Confusion_mat 
model_3$coefficients

AIC(model_3,model_2)


#===========================================================================================================

#code-11-a permutation test

if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")


# this code implements a permutation test for the NUll
# hypothesis that the correlation coefficient is zero 

setwd("C:/Users/staff/Documents")  # where the data lies
titanic <- read.csv("titanic.csv")
head(titanic)
names(titanic)
plot(titanic$Fare~titanic$Age)

sample_CC <- cor(titanic$Fare,titanic$Age)  # sample correlation coefficient 
sample_CC

# Estimating accuracy of the correlation coefficient using a permutation test

nreps <- 10000
CC_stats <- numeric(nreps)
for(i in 1:nreps){
  CC_stats[i] <- cor(sample(titanic$Fare),titanic$Age) }
hist(CC_stats)  # NULL distribution for the correlation
abline(v=cor(titanic$Fare,titanic$Age),col="red",lwd =5) # sample correlation
mean(CC_stats>sample_CC) # p value  calculation
summary(lm(titanic$Fare~titanic$Age)) # compare to linear regression "p-value:"

#===========================================================================================================

#code-12-simulation a simple linear regression model

if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")

n_points <- 20 # number of data points for each linear regression
n_regressions <- 1000  # number of simulations 
y <- numeric(n_points)   # x and y points 
x <- seq(-1,1,2/(n_points-1))
length(x)
intercepts <- numeric(n_regressions) # records intercepts 
slopes <- numeric(n_regressions)  # records slopes 
for(i in 1:n_regressions){
  y <- x + rnorm(n_points,mean=0,sd=1.0)  # line y=x plus N(0,1.0) noise
  lin_mod <-lm(y~x)  # fit a linear model 
  intercepts[i] <- lin_mod$coefficients[1] # record alpha and beta 
  slopes[i] <- lin_mod$coefficients[2]    #  y=alpha+beta*x
}
hist(slopes)  # distribution of slope estimates 
abline(v=0.0,col="red")
sum(slopes<0)/length(slopes)  # p value (one tail)
hist(intercepts)  # distribution of intercepts

y <- x + rnorm(n_points,mean=0,sd=1.0)  # single y=x plus N(0,1.0) noise
plot(x,y)  # last x y plotted
lin_mod <-lm(y~x);sd(intercepts);sd(slopes)
summary(lin_mod) # compare results with linear model estimates 

# looking at residuals (obviously well behaved here)
y <- x + rnorm(n_points,mean=0,sd=1.0)  # single y=x plus N(0,1.0) noise
lin_mod <-lm(y~x) # fit the linear model 
plot(lin_mod$fitted.values,lin_mod$residuals) # check residuals for unwanted patterns
abline(0,0,col="red")
qqnorm(lin_mod$residuals) # check for normality of errors
qqline(lin_mod$residuals,col="red")
#following are the two types of 95% error boundaries
X_values <- data.frame(x) # dataframe; must have same field name as the data
names(X_values) <- "x"  #rename field exactly as the data for predict()
predict_conf <- predict.lm(lin_mod,X_values, level = 0.95,interval = "confidence")
predict_pred <- predict.lm(lin_mod,X_values, level = 0.95,interval = "prediction")
head(predict_conf)
head(predict_pred)
par(mfrow=c(1, 1))  # reset graph area 
plot_area <- cbind(y, predict_conf,predict_pred[,-1]) # bind into one dataframe
matplot(x,cbind(predict_conf,predict_pred[,-1]),lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")
lines(y~x,type="p") # plot data points together
abline(0,1,col="red",lwd=2) # plot population line  (here known as its a simulation)

#===========================================================================================================

#code-13-An introduction to statistical learning by Hastie et.al.

if(!is.null(dev.list())) dev.off()  # clear out the past 
rm(list = ls())
cat("\014")

# Code from the online resources of the book 
# An introduction to statistical learning by Hastie et.al.
# Full explantion see pages (first edition)  154 to 161 (4.6 Lab)

# Chapter 4 Lab: Logistic Regression

# The Stock Market Data

library(ISLR)  # data is in this package 
names(Smarket)  
dim(Smarket) 
View(Smarket) # spreadsheet type view 
head(Smarket)
summary(Smarket)
str(Smarket)
pairs(Smarket) # plots of all correlations  
cor(Smarket)  # error due to categorical data 
cor(Smarket[,-9]) # exclude  the categorical data 
detach(Smarket)
attach(Smarket) # used to make the DF accessible by default 
plot(Volume)

# Logistic Regression

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fits) 
coef(glm.fits)
summary(glm.fits)$coef
summary(glm.fits)$coef[,4]
glm.probs=predict(glm.fits,type="response") # fitted probabilities
glm.probs[1:20]
contrasts(Direction)  # tells which is the reference level 
glm.pred=rep("Down",1250)
glm.pred[glm.probs>.5]="Up"  # the predicted up and down 
table(glm.pred,Direction)
(507+145)/1250
mean(glm.pred==Direction)
table(Year)
train=(Year<2005)
table(train)

Smarket.2005 <- Smarket[!train,]  # 2005 only 
dim(Smarket.2005)
dim(Smarket)
Direction.2005 <- Direction[!train] #2005 only 
length(Direction.2005)

# logistic model 2001 to 2004 data not 2005
glm.fits.train <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
summary(glm.fits.train)
glm.probs.2005 <- predict(glm.fits.train,newdata=Smarket.2005,type="response") # predicted probs on 2005 data
length(glm.probs.2005)

glm.pred.2005=rep("Down",252)
glm.pred.2005[glm.probs.2005>.5]="Up"
table(glm.pred.2005,Direction.2005)
mean(glm.pred.2005==Direction.2005)
mean(glm.pred.2005!=Direction.2005)

# try model on two most recent lag variables  only
glm.fits=glm(Direction~Lag1+Lag2,data=Smarket,family=binomial,subset=train)
glm.probs=predict(glm.fits,newdata=Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs>.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
106/(106+76) # maybe predicts ups OK not downs
predict(glm.fits,newdata=data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)),type="response")


#===========================================================================================================

#code-14 - linear regression boston data

rm(list = ls())
library(MASS)
library(olsrr)

?Boston
names(Boston) # variable names
head(Boston)  # display the first 6 observations
str(Boston) # types of variables
summary(Boston) # stats on variables
Boston$chas <- as.factor(Boston$chas)
str(Boston) # types of variables

par(mfrow=c(1, 1))  # divide graph area in 1 columns
scatter.smooth(y=Boston$medv, x=Boston$lstat, main="Medv ~ Status")  # scatterplot
# here we decide to log the property value predicted variable 
scatter.smooth(y=log(Boston$medv), x=Boston$lstat, main="Medv ~ Status")  # scatterplot
scatter.smooth(y=log(Boston$medv), x=Boston$rm, main="Medv ~ Status")  # scatterplot
boxplot(Boston$medv)
boxplot(log(Boston$medv)) # decide to  log medv 
Boston$medv <- log(Boston$medv)  # log the medv dependent variable 

boxplot(Boston$crim)
boxplot(log(Boston$crim))
Boston$crim <- log(Boston$crim) # use the log of crim 

# Build the model on training data
dim(Boston)
set.seed(1)
train = sample(1:nrow(Boston), 400)
boston.train <- Boston[train,]
boston.test=Boston[-train,]

lmMod <- lm(medv~., data=boston.train)  # build the model
summary (lmMod)  # model summary
lmMod1 <- lm(medv~.-age-zn-crim,data=boston.train)
summary (lmMod1)  # model summary
lmMod2 <- lm(medv~.-age-zn-crim-indus,data=boston.train)
summary (lmMod2)  # model summary
train.model <- lmMod2  # chosen model on training data
train.model$coefficients


# plot diagnostics with model applied to test data
par(mfrow=c(1, 3))  # divide graph area in 3 columns
TestPred <- predict(train.model, boston.test)  # use predict()  to check model on testData
mis_fit_to_testData <- TestPred-boston.test$medv  # these are the residuals for the test data fit
mean((TestPred-boston.test$medv)^2) # compare to simpler model below
plot(boston.test$medv,TestPred) # plot predicted med val ~ actual 
abline(0,1,col="red")
cor(boston.test$medv,TestPred) # check correlation 
plot(boston.test$medv,mis_fit_to_testData) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

model <- lm(medv~., data=boston.train) # forward selection by AIC
AIC_model <- ols_step_forward_aic(model,detail=FALSE)
AIC_model$predictors  # chosen by AIC automatically
train.model$coefficients  # chosen manually by p value

# use  model common to both AIC and manual P values

# plot diagnostics for the minimal model
par(mfrow=c(1, 3))  # divide graph area in 3 columns
TestPred <- predict(train.model, boston.test)  # use predict()  to check model on testData
mis_fit_to_testData <- TestPred-boston.test$medv  # these are the residuals for the test data fit
plot(boston.test$medv,TestPred) # plot predicted med val ~ actual 
abline(0,1,col="red")
cor(boston.test$medv,TestPred) # check correlation 
plot(boston.test$medv,mis_fit_to_testData) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

# check the chosen predictors again
par(mfrow=c(1, 3))  # divide graph area in 3 columns
train.model$coefficients
hist(Boston$lstat)
hist(Boston$rm)
hist(Boston$nox)
hist(Boston$tax)
hist(Boston$ptratio)
hist(Boston$dis)
hist(Boston$rad)
hist(Boston$black) # try leaving out later
Boston$tax_c <- Boston$tax>500 # turn into binary factor

new_model <- lm(medv~nox+rm+dis+rad+tax_c+ptratio+black+lstat+chas,data=Boston)
# plot diagnostics for the the new model on all the data 
par(mfrow=c(1, 3)) 
Predict_all <- predict(new_model,Boston)  
mis_fit_to_testData <- Predict_all-Boston$medv  
plot(Boston$medv,Predict_all) #
abline(0,1,col="red")
cor(Boston$medv,Predict_all) # check correlation 
plot(Boston$medv,mis_fit_to_testData) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

?AIC
new_model_2 <- lm(medv~nox+rm+dis+rad+tax+ptratio+black+lstat+chas,data=Boston)
AIC(new_model,new_model_2)
new_model_3 <- lm(medv~nox+rm+dis+rad+tax+ptratio+lstat,data=Boston)
AIC(new_model_3,new_model_2)

summary(new_model_2)
# plot diagnostics for the model 2
par(mfrow=c(1, 3))  
Predict_all <- predict(new_model_2,Boston)  
mis_fit_to_testData <- Predict_all-Boston$medv 
plot(Boston$medv,Predict_all)  
abline(0,1,col="red")
cor(Boston$medv,Predict_all) # check correlation 
plot(Boston$medv,mis_fit_to_testData) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

summary(new_model_2) # chosen model 

# try to reduce the range of the regression
Boston_cut <- Boston[which(Boston$medv>2.5&Boston$medv<3.5),]
Final_model <- lm(medv~nox+rm+dis+rad+tax+ptratio+black+lstat+chas,data=Boston_cut)
# plot diagnostics for the final chosen 
par(mfrow=c(1, 3))  # divide graph area in 3 columns
Predict_all <- predict(Final_model,Boston_cut)  #
mis_fit_to_testData <- Predict_all-Boston_cut$medv 
plot(Boston_cut$medv,Predict_all) 
abline(0,1,col="red")
cor(Boston_cut$medv,Predict_all) # check correlation 
plot(Boston_cut$medv,mis_fit_to_testData) # look for unwanted pattern in residuals
abline(0,0,col="red")
qqnorm(mis_fit_to_testData) # check for normality of residuals in prediction
qqline(mis_fit_to_testData,col="red")

?Boston
summary(Final_model)  # interpret coefficients remember the log(response) step


#===========================================================================================================

#code-15-titanic logistic modelling

library(DescTools)
library(dplyr)
if(!is.null(dev.list())) dev.off()
rm(list = ls())
cat("\014")

getwd()
titanic <- read.csv("titanic.csv")  # read in  data from csv file 
head(titanic)  # usual initial investigation
names(titanic)
summary(titanic)
str(titanic)

table(titanic$Survived,titanic$Pclass)
GTest(table(titanic$Survived,titanic$Pclass))

hist(titanic$Age[titanic$Age<16])
titanic$child<- titanic$Age<16
table(titanic$child)

table(titanic$Survived,titanic$Sex)

boxplot(titanic$Fare~titanic$Pclass)

hist(titanic$Age)
boxplot(titanic$Age~titanic$Pclass)

fare_2 <- titanic%>%filter(Pclass==2)%>%select(Fare)
fare_3 <- titanic%>%filter(Pclass==3)%>%select(Fare)
par(mfrow=c(1,2))
boxplot(fare_2)
boxplot(fare_3)
t.test(fare_2,fare_3) # h0 is that 2 and 3 class fares same 

titanic%>%group_by(Pclass)%>%summarise(mean_fare=mean(Fare))

titanic%>%group_by(Survived)%>%summarise(mean_fare=mean(Fare))

titanic%>%group_by(Sex,Pclass)%>%summarise(mean_fare=mean(Fare))
titanic%>%group_by(Sex)%>%filter(Pclass==1)%>%summarise(mean_fare=mean(Fare))

table(titanic$Sex[titanic$Pclass==1])

titanic%>%group_by(Pclass)%>%summarise(mean_Age=mean(Age))

titanic%>%group_by(Pclass,Sex)%>%summarise(no=n())

sum(titanic$Age)/nrow(titanic)

par(mfrow=c(1,2))
Age_3 <- titanic%>%filter(Pclass==3)%>%select(Age)
hist(Age_3$Age)
Age_1 <- titanic%>%filter(Pclass==1)%>%select(Age)
hist(Age_1$Age)
par(mfrow=c(1,1))


?arrange
fares_ordered <- titanic%>%arrange(desc(Fare))
plot(fares_ordered$Fare)
head(fares_ordered)
fares_ordered <- titanic%>%arrange(Fare)
head(fares_ordered)

fare_zero <- titanic%>%filter(Fare==0)
fare_zero
titanic%>%filter(Fare==0,Survived==1)

# logistic model 
str(titanic)
titanic$child <- as.factor(titanic$child)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)

model_fare <- glm(Survived ~Fare+Sex+Pclass+Age,family=binomial(link='logit'),data=titanic) # logistic model fit
summary(model_fare) # check p values for each variable 
model_no_fare <- glm(Survived ~Sex+Pclass+Age,family=binomial(link='logit'),data=titanic)
summary(model_no_fare)
AIC(model_no_fare,model_fare) # so by p value and AIC Fare is a poor predictor


quantile(titanic$Fare,probs = c(0.025,0.9725))
non_zero_fares <- titanic[titanic$Fare>6.75 & titanic$Fare<152.76,]
non_zero_fares$log_Fare <- log(non_zero_fares$Fare)

boxplot(non_zero_fares$log_Fare)

plot(sort(non_zero_fares$log_Fare))

model_log_fare <- glm(Survived ~log_Fare+Sex+Pclass+Age,family=binomial(link='logit'),data=non_zero_fares) 
summary(model_log_fare) # check p values for each variable 
model_no_log_fare <- glm(Survived ~Sex+Pclass+Age,family=binomial(link='logit'),data=non_zero_fares)
AIC(model_no_log_fare,model_log_fare) # log fare is better 
model_no_Age <- glm(Survived ~log_Fare+Sex+Pclass,family=binomial(link='logit'),data=non_zero_fares)
AIC(model_log_fare,model_no_Age)

summary(model_log_fare) # the best model found check p values for each variable 
hist(model_log_fare$fitted.values) # predicted probablities 
boxplot(model_log_fare$fitted.values~non_zero_fares$Survived) # fitted.values are probabilities 
Confusion_mat <- table(model_log_fare$fitted.values>0.5,non_zero_fares$Survived) # final result 
Confusion_mat 
model_log_fare$coefficients
contrasts(non_zero_fares$Pclass)
boxplot(non_zero_fares$log_Fare~non_zero_fares$Sex)

# now look at some predictions
mean_log_fare <- non_zero_fares%>%filter(Sex=="male",Pclass==1)%>%summarise(mean_log_fare=mean(log_Fare))
mean_log_fare
model_log_fare$formula
data.frame(log_Fare=as.numeric(mean_log_fare),Sex="male",Pclass="1",Age=40)


# use predict to do the maths 
predict(model_log_fare,newdata=data.frame(log_Fare=as.numeric(mean_log_fare),Sex="male",Pclass="1",Age=40),type="response")

# or do the maths :
expo <- exp(model_log_fare$coefficients[1]+model_log_fare$coefficients[2]*mean_log_fare+model_log_fare$coefficients[3]+
              model_log_fare$coefficients[6]*40)  # explicit prob for a male 1st class 40 years old at mean fare 
expo/(expo+1)    # note the same as predict above 

model_kids <- glm(Survived ~log_Fare+Sex+Pclass+Age+child+child*Sex,family=binomial(link='logit'),data=non_zero_fares)
AIC(model_log_fare,model_kids)
summary(model_kids)
hist(model_kids$fitted.values)
boxplot(model_kids$fitted.values~non_zero_fares$Survived) # fitted.values are probabilities 
Confusion_mat <- table(model_kids$fitted.values>0.5,non_zero_fares$Survived) # final result 
Confusion_mat 
model_kids$coefficients


model_kids$coefficients
# who had a better chance of surviving according to this model
# A third class 10 yr old male child or first class 40 year old male ? 

# Solution in code 
model_kids$formula

mean_log_fare_1 <- non_zero_fares%>%filter(Sex=="male",Pclass==1)%>%summarise(mean_log_fare=mean(log_Fare))
mean_log_fare_1

# use predict to do the maths 
predict(model_kids,newdata=data.frame(log_Fare=as.numeric(mean_log_fare_1),Sex="male",Pclass="1",Age=40
                                      ,child="FALSE"),type="response")

mean_log_fare_3 <- non_zero_fares%>%filter(Sex=="male",Pclass==3)%>%summarise(mean_log_fare=mean(log_Fare))
mean_log_fare_3
predict(model_kids,newdata=data.frame(log_Fare=as.numeric(mean_log_fare_3),Sex="male",Pclass="3",Age=10
                                      ,child="TRUE"),type="response")

#=================================================================================================================
                              #ALL CODES BY MR.DAN BROWN

#=========================================================================================================
