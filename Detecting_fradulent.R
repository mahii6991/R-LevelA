###################################################
### Detecting Fradulent transaction 
###################################################
#Detecting Fraudulent Transaction - the third case study address an instantiation of the general problem fo 
#detecting unusual obervation of a phenomena, that is, finding rare and quite different observation.The outcome
#of the data mining process will support posterior inspection activities by the company .Given the limited 
#amount of resources that can be alloctated to this inspection activity .

#Problem description and objectives
#fraud detection is an important area for potential application of data mining techniques given the economics
#social consequences that are usually asscociated with these illegeal activites. From the perspective of data
#analysis,fruads, are usually associated with unusal observation as these are activities that are supported
#to be deviation from the norm.



#the amount of dataset is so big that we are addressing around 4 lakh+ rows of the dataset and finding out the insights
#from it


#ID- a factor with the ID of the salesman
#Prod- a factor indicating the ID of the sold product
#Quant - the number of reported sold unit of the prodcut 
#val - the reported total monetary value of the sale.
#Insp - a factor with three possible values : ok if the transaction was inspected and considered valid by 
#by the company , fraud if the transaction was found to be fraudulent and unkn if the transaction was not 
#inspected at all by the company.


###################################################
### Loading the data into R
###################################################

load('sales.Rdata')

#loading the library
library(DMwR)
data(sales)#seeing the dataset

#head of the dataset
head(sales)

#Exploring the dataset
summary(sales)

#we have significant numbers of products and salespeople , as we can confirm using the function nlevels()
nlevels(sales$ID)

nlevels(sales$Prod)

#loading another library
library(psych)

describe(sales)

#first there are a considerable number of unknown values in the column quant and val. This can be 
#particularly problematic if both happen at the same time, as this would represent a transaction report within 
#without the curcial information of the quantities involved in the sale.

#finding out how many na values are present in both the column
length(which(is.na(sales$Quant) & is.na(sales$Val)))

#finding out the na values from the dataset 
sum(is.na(sales$Quant) & is.na(sales$Val))

#using the table function to inspect the results which are also a small proportion overall:
table(sales$Insp)/nrow(sales)*100


#finding out the number of transaction per salesperson
totS <- table(sales$ID)
totP <- table(sales$Prod)
barplot(totS, main = "Transaction per salesperson", names.arg = "",
        xlab = "Salesperson",ylab = "Amount")
barplot(totP, main = "Transaction per product", names.arg = "",
        xlab = "Products",ylab = "Amount")

#the discriptive statistics of Quant and Val show a rather market variablility.
sales$Uprice <- sales$Val/sales$Quant

#the unit price should be relatively constant over the transaction of the same product
summary(sales$Uprice)

#we will use the median at the selling price of an item and try to find out the most expensive and least 
#expensive item in the dataset

attach(sales)
#here we have used the aggregate function to find the relative median value of the dataset
upp <- aggregate(Uprice,list(Prod),median,na.rm=T)
#here we have used the sapply function to calcualte function(o)
topP <- sapply(c(T,F),function(o)
  upp[order(upp[,2],decreasing = 0)[1:5],1])
#providing the column names in the form of cheap and expensive
colnames(topP) <- c('Expensive','Cheap')
#calling it
topP

#we can confirm the completely different price distribution of the top product using a boxplot of their unit price
tops <- sales[Prod %in% topP[1,],c("Prod","Uprice")]
tops$Prod <- factor(tops$Prod)
boxplot(Uprice~Prod,data = tops,ylab = "Uprice",log="y")

#the %in% operator tests if a values belongs to a set. The call to the function factor() is required 
#because otherwise the column prod of the dataframe tops would have the same number of levels as the column

#one thing to note here the scale of prices of the most expensive and cheapest products are rather different
#because of this we have used a log scale in the graph to avoid the values of the cheapest products becoming
#indistinguishable

#we can carry out the similar analysis to discover which sales person are the one who brings more(less)
#money to the company
vs <- aggregate(Val,list(ID),sum,na.rm=T)
scoreSs <- sapply(c(T,F),function(o)
  vs[order(vs$x,decreasing = o)[1:5],1])
colnames(scoreSs) <- c('Most','Least')
scoreSs #it is interesting to find out that top100 salesperson on this list account for almost 40% of the income of the company,while the bottom 2000, out of the 6016,
                  #salesperson generate less than 2% of the income.

sum(vs[order(vs$x,decreasing = T)[1:100],2])/sum(Val,na.rm = T)*100 


sum(vs[order(vs$x,decreasing=F)[1:2000],2])/sum(Val,na.rm=T)*100

#if we carry out a similar analysis in terms of the quantity that is sold for each product , the results are even more unbalance

#with the help of this function we can find out the aggregate or the total quantity of the product
qs <- aggregate(Quant,list(Prod),sum,na.rm=T)


scoresPs <- sapply(c(T,F),function(o) 
  qs[order(qs$x,decreasing=o)[1:5],1])
colnames(scoresPs) <- c('Most','Least')
scoresPs
sum(as.double(qs[order(qs$x,decreasing=T)[1:100],2]))/
  sum(as.double(Quant),na.rm=T)*100
sum(as.double(qs[order(qs$x,decreasing=F)[1:4000],2]))/
  sum(as.double(Quant),na.rm=T)*100

#one of the main assumption we will be making in our analysis to find abnormal transaction report is that
#the unit price of any product should follow a normal distribution curve.This means that we except that the
#transaction of the same product will have roughly the same unit price with some small variability possibly due
#caused by some strageies of the salesperson to achieve their commercial goals.


out <- tapply(Uprice,list(Prod=Prod),
              function(x) length(boxplot.stats(x)$out))


out[order(out,decreasing=T)[1:10]]


sum(out)
sum(out)/nrow(sales)*100
#So, although the analysis is correct, the conclusion may be imparied by low-quality of data. This should be
#taken into account in a real world situation not to provide advice to the company based on data that include error.

#at most we can avoid using the small number of transaction already found to errors in all exploratory 
#analysis of data .This means that this sort of analysis required some form of interaction with the domain
#expert ,particularly when there are doubts regrarding the quality of the data, as is the case in this problem

#Data Problems

###################################################
### Data problems
###################################################
#ways to dealing with the unknown values,there are three alternatives to deal with them
#remove the cases
#fill in the unknown using some strategy
#use tools to handle this kind of values

#the main concern are transaction that have both the values of quant and val missing . removing all 888 cases 
#may be problamatic if this leads to removing most transaction of some product or salesperson 

#the total number of transaction per sales person and product is given by
totS <- table(ID)
totP <- table(Prod)

#the sales person and product involved in the problematic transaction are the following:
nas <- sales[which(is.na(Quant) & is.na(Val)),c('ID','Prod')]

#we obtain the sales person with a larger proportion of transaction with unkown on both val and quant
propS <- 100*table(nas$ID)/totS
propS[order(propS,decreasing=T)[1:10]]

#with respect to the products ,these are the numbers
propP <- 100*table(nas$Prod)/totP
propP[order(propP,decreasing=T)[1:10]]

#there are several products that would have more than 20% of their transaction removed, and in particular 
#product p2689 would have almost 40% of them removed.
detach(sales)
sales <- sales[-which(is.na(sales$Quant) & is.na(sales$Val)),]


nnasQp <- tapply(sales$Quant,list(sales$Prod),
                 function(x) sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]
#there are two products which has the unknown values of the quantity
#we are deleting this two transaction
sales <- sales[!sales$Prod %in% c('p2442','p2443'),]

#given we have removed the two products from the dataset we will update it
nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)

#are there salesperson with all transaction with unknown quantity
nnasQs <- tapply(sales$Quant,list(sales$ID),function(x) sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs,decreasing=T)[1:10]]#we found out that there are many salesperson who have not filled in the information on the quantity in their reports.

#we will now carry out the similar analysis for the transaction with an unknown value in the val column
nnasVp <- tapply(sales$Val,list(sales$Prod),
                 function(x) sum(is.na(x)))
propNAsVp <- nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing=T)[1:10]]

#the numbers are resonable so it does not make sense to delete them, transaction as we may try to fill in these holes using the other transaction with respect to 
                 #sales person the number are as follows:
nnasVs <- tapply(sales$Val,list(sales$ID),function(x) sum(is.na(x)))
propNAsVs <- nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs,decreasing=T)[1:10]]
#at this stage we have removed all reports that had insufficient information to be subject to a fill-in-strategy. For the remaining unknown values , we will apply a method
                 #based on the assumption that transaction of the same product should have a similar unit price.

tPrice <- tapply(sales[sales$Insp != 'fraud','Uprice'],
                 list(sales[sales$Insp != 'fraud','Prod']),
                 median,na.rm=T)
#having a typical unit price for each product , we can use it to calculate any of the two possibly missing values. This is possible because we currently have no 
                 #transaction with both the missing values

noQuant <- which(is.na(sales$Quant))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val'] /
                                    tPrice[sales[noQuant,'Prod']])
noVal <- which(is.na(sales$Val))
sales[noVal,'Val'] <- sales[noVal,'Quant'] *
  tPrice[sales[noVal,'Prod']]
#if you have missing it we have just filled out in 12,900 unknown quantities values plus 294 total values of transaction.we have used the function ceiling() to avoid 
                 #non-integer values of quant

sales$Uprice <- sales$Val/sales$Quant
#given we have all quant and val values we can recalculate the uprice column to fill in the previously unknown unit price

save(sales,file='salesClean.Rdata')

#few transaction of some products
                 
 #the code uses the boxplot.stats() function to obtain the values of the median, first and third quantiles
attach(sales)
notF <- which(Insp != 'fraud')
ms <- tapply(Uprice[notF],list(Prod=Prod[notF]),function(x) {
  bp <- boxplot.stats(x)$stats
  c(median=bp[3],iqr=bp[4]-bp[2])
})
ms <- matrix(unlist(ms),
             length(ms),2,
             byrow=T,dimnames=list(names(ms),c('median','iqr')))
head(ms)


 #we have obtained the figures using the log scale on both axes of the graph, we have done this to overcome the problem of visualization. 
par(mfrow=c(1,2))
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='')
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='',col='grey',log="xy")
smalls <- which(table(Prod) < 20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch='+')

#we will use a non-parametric test to compare the distribution of unit prices,as these tests are more robust to the presence of outliers. 
#the kolmogorov-Sminrov test can be used to compare any two samples to check the validity of the null hypothesis that both came from the same distribution.
dms <- scale(ms)
smalls <- which(table(Prod) < 20)
prods <- tapply(sales$Uprice,sales$Prod,list)
similar <- matrix(NA,length(smalls),7,dimnames=list(names(smalls),
                                                    c('Simil','ks.stat','ks.p','medP','iqrP','medS','iqrS')))
#the kW test is used with the help of ks.test() .This function return significant information ,amoung which we have etracted the value of the statistics of  the test and the 
                 #respective signigicance level.
for(i in seq(along=smalls)) {
  d <- scale(dms,dms[smalls[i],],FALSE)
  d <- sqrt(drop(d^2 %*% rep(1,ncol(d))))
  stat <- ks.test(prods[[smalls[i]]],prods[[order(d)[2]]])
  similar[i,] <- c(order(d)[2],stat$statistic,stat$p.value,ms[smalls[i],],
                   ms[order(d)[2],])
}


head(similar)


levels(Prod)[similar[1,1]]

#we can check how many products are there whose unit price distribution is significantly similar with 90% confidence:
nrow(similar[similar[,'ks.p'] >= 0.9,])

#as we can see the results from the 985 products with less than 20 transaction , we have only managed to find similar products for 117 of them.
sum(similar[,'ks.p'] >= 0.9)

#saving that file in it.
save(similar,file='similarProducts.Rdata')


################################################################Defining the data mining task#########################################################################

###################################################
### Evaluation criteria
###################################################

#clustering is the example of the discriptive data mining task. we can also use to find the outliers in our dataset, and we take the notion that they are too far form the group.
#from this we can see that there is strong correlatin  between the outlier detection and clustering. this means that a good cluster does not include the outliers in a large group of data.                 
#what is the semi-super vised learing that we will going to use here.
#so in order to describe them we can say that they can use both the label and unlabeled dataset. there are many semi-supervised learing algorithm used for the classification model,
#with the given labeled data . the next step is to use this model to classify the unlabeled data.then the new model is obtained and repeated the process until some 
#convergence point is reached.Anothe example of the semi-surpervised learning is the TSVM, .The goal of TSVM is to obtain label for a set of unlabeled data, such as 
#a linear bounday achieves the maximum margin on both the original labeled data and the unlabeled data.
                 
library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance(pred,'prec','rec')
plot(perf)

#the concept of the precision and recall ,usually there is a trade off between the precison and recall. for instance it is quite easy to obtain the 100% recall if all 
#all the test cases are predicted as events.
#it is important to check the performance of the model at the different levels and this may be the useful informtion when comapring them.
                 #the precision recall curve are visual representaion of the performance of a model in terms of the precision and recall curves.
PRcurve <- function(preds,trues,...) {
  require(ROCR,quietly=T)
  pd <- prediction(preds,trues)
  pf <- performance(pd,'prec','rec')
  pf@y.values <- lapply(pf@y.values,function(x) rev(cummax(rev(x))))
  plot(pf,...)
}


PRcurve(ROCR.simple$predictions, ROCR.simple$labels )




pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels )
perf <- performance(pred,'lift','rpp')
plot(perf,main='Lift Chart')


CRchart <- function(preds,trues,...) {
  require(ROCR,quietly=T)
  pd <- prediction(preds,trues)
  pf <- performance(pd,'rec','rpp')
  plot(pf,...)
}  


CRchart(ROCR.simple$predictions, ROCR.simple$labels, 
        main='Cumulative Recall Chart')


########Normalized distance to a typical price
#we will use the IQR to normalize the distance with the typical unit price of the product ,measured by the median unit price of that product. and the IQRs is the inter qutile range 
 

avgNDTP <- function(toInsp,train,stats) {
  if (missing(train) && missing(stats)) 
    stop('Provide either the training data or the product stats')
  if (missing(stats)) {
    notF <- which(train$Insp != 'fraud')
    stats <- tapply(train$Uprice[notF],
                    list(Prod=train$Prod[notF]),
                    function(x) {
                      bp <- boxplot.stats(x)$stats
                      c(median=bp[3],iqr=bp[4]-bp[2])
                    })
    stats <- matrix(unlist(stats),
                    length(stats),2,byrow=T,
                    dimnames=list(names(stats),c('median','iqr')))
    stats[which(stats[,'iqr']==0),'iqr'] <- 
      stats[which(stats[,'iqr']==0),'median']
  }
  
  mdtp <- mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median']) /
                 stats[toInsp$Prod,'iqr'])
  return(mdtp)
}


###################################################
### Experimental Methodology
###################################################
                        
#we will be using the hold out method for our experimental comparison
#a possibility is to use the idea of of the normalized distance to the typical unit price(NDTP) is that it is a unitless metric and thus we can 
                        
evalOutlierRanking <- function(testSet,rankOrder,Threshold,statsProds) {
  ordTS <- testSet[rankOrder,]
  N <- nrow(testSet)
  nF <- if (Threshold < 1) as.integer(Threshold*N) else Threshold
  cm <- table(c(rep('fraud',nF),rep('ok',N-nF)),ordTS$Insp)
  prec <- cm['fraud','fraud']/sum(cm['fraud',])
  rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
  AVGndtp <- avgNDTP(ordTS[nF,],stats=statsProds)
  return(c(Precision=prec,Recall=rec,avgNDTP=AVGndtp))
}


###################################################
### The modified box plot rule
###################################################
BPrule <- function(train,test) {
  notF <- which(train$Insp != 'fraud')
  ms <- tapply(train$Uprice[notF],list(Prod=train$Prod[notF]),
               function(x) {
                 bp <- boxplot.stats(x)$stats
                 c(median=bp[3],iqr=bp[4]-bp[2])
               })
  ms <- matrix(unlist(ms),length(ms),2,byrow=T,
               dimnames=list(names(ms),c('median','iqr')))
  ms[which(ms[,'iqr']==0),'iqr'] <- ms[which(ms[,'iqr']==0),'median']
  ORscore <- abs(test$Uprice-ms[test$Prod,'median']) /
    ms[test$Prod,'iqr']
  return(list(rankOrder=order(ORscore,decreasing=T),
              rankScore=ORscore))
}


notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF],
                      list(Prod=sales$Prod[notF]),
                      function(x) {
                        bp <- boxplot.stats(x)$stats
                        c(median=bp[3],iqr=bp[4]-bp[2])
                      })
globalStats <- matrix(unlist(globalStats),
                      length(globalStats),2,byrow=T,
                      dimnames=list(names(globalStats),c('median','iqr')))
globalStats[which(globalStats[,'iqr']==0),'iqr'] <- 
  globalStats[which(globalStats[,'iqr']==0),'median']


ho.BPrule <- function(form, train, test, ...) {
  res <- BPrule(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}


bp.res <- holdOut(learner('ho.BPrule',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
)


summary(bp.res)
#the result of precision and recall are rather low. on average , only 52% of the known frauds are included in the top10% reprots of the rank produced by the bp rule
#the extreme low values of precision means that this method is putting on the top10% position mostly unkn and ok cases.

par(mfrow=c(1,2))
info <- attr(bp.res,'itsInfo')
PTs.bp <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2)
)
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',avg='vertical')
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',avg='vertical')




###################################################
### Local outlier factors (LOF)
###################################################
#need to write some good explaination regarding it.
                        
                        
  ho.LOF <- function(form, train, test, k, ...) {
  ntr <- nrow(train)
  all <- rbind(train,test)
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2)) 
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],
                                          decreasing=T),...),
            itInfo=list(preds=all[(ntr+1):N,'lof'],
                        trues=ifelse(test$Insp=='fraud',1,0))
  )
}


lof.res <- holdOut(learner('ho.LOF',
                           pars=list(k=7,Threshold=0.1,
                                     statsProds=globalStats)),
                   dataset(Insp ~ .,sales),
                   hldSettings(3,0.3,1234,T),
                   itsInfo=TRUE
)


summary(lof.res)


par(mfrow=c(1,2))
info <- attr(lof.res,'itsInfo')
PTs.lof <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
)
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))



###################################################
### Clustering-based outlier rankings (OR_h)
###################################################
ho.ORh <- function(form, train, test, ...) {
  require(dprep,quietly=T)
  ntr <- nrow(train)
  all <- rbind(train,test)
  N <- nrow(all)
  ups <- split(all$Uprice,all$Prod)
  r <- list(length=ups)
  for(u in seq(along=ups)) 
    r[[u]] <- if (NROW(ups[[u]]) > 3) 
      outliers.ranking(ups[[u]])$prob.outliers 
  else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]])) 
  else NULL
  all$lof <- vector(length=N)
  split(all$lof,all$Prod) <- r
  all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))] <- 
    SoftMax(all$lof[which(!(is.infinite(all$lof) | is.nan(all$lof)))])
  structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],
                                          decreasing=T),...),
            itInfo=list(preds=all[(ntr+1):N,'lof'],
                        trues=ifelse(test$Insp=='fraud',1,0))
  )
}


orh.res <- holdOut(learner('ho.ORh',
                           pars=list(Threshold=0.1,
                                     statsProds=globalStats)),
                   dataset(Insp ~ .,sales),
                   hldSettings(3,0.3,1234,T),
                   itsInfo=TRUE
)


summary(orh.res)


par(mfrow=c(1,2))
info <- attr(orh.res,'itsInfo')
PTs.orh <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
)
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('topright',c('BPrule','LOF','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))


###################################################
### The class imbalance problem
###################################################
#the dataset is very imbalanced proportion of normal and fradulent transaction . the latter are clear minority , roughly 8.1% of the inspected reports
#several sampling methods have been proposed to change the class imbalance of a dataset. Under-sampling methods select the small part of the majority class 
#therby building a dataset with a more balanced class distribution. Over sampling methods works the way around. using some process to replicate the minority class
#a succesful example is the SMOTE method l the general idea of this methods is to artificially generate the new examples of the minority class.
                        
                        
data(iris)
data <- iris[,c(1,2,5)]
data$Species <- factor(ifelse(data$Species == 'setosa','rare','common'))
newData <- SMOTE(Species ~ .,data,perc.over=600)
table(newData$Species)


par(mfrow=c(1,2))
plot(data[,1],data[,2],pch=19+as.integer(data[,3]),main='Original Data')
plot(newData[,1],newData[,2],pch=19+as.integer(newData[,3]),main="SMOTE'd Data")



###################################################
### Naive Bayes
###################################################

                        
#it is a probabilistic classifer based on the baye's theorem that used very strong assumptions on the independece between the predictors. These assumptions rarely
#holds for the real world problems - and thus the name naive . Nevertheless it has been applied to many problems that are related to the real world application.
#             
                        
                        
                        
                        
                        
                        
                        
  nb <- function(train,test) {
  require(e1071,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- naiveBayes(Insp ~ .,data)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}


ho.nb <- function(form, train, test, ...) {
  res <- nb(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}


nb.res <- holdOut(learner('ho.nb',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
)


summary(nb.res)

#the scores are considerebly worse than the best scores obtained previously with the unsupervised methods. a possible cause of the class imbalance problem will be the
#class imbalance problem.
par(mfrow=c(1,2))
info <- attr(nb.res,'itsInfo')
PTs.nb <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2)
)
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('topright',c('NaiveBayes','ORh'),
       lty=1,col=c('black','grey'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh'),
       lty=1,col=c('black','grey'))


nb.s <- function(train,test) {
  require(e1071,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  newData <- SMOTE(Insp ~ .,data,perc.over=700)
  model <- naiveBayes(Insp ~ .,newData)
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}


ho.nbs <- function(form, train, test, ...) {
  res <- nb.s(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}


nbs.res <- holdOut(learner('ho.nbs',
                           pars=list(Threshold=0.1,
                                     statsProds=globalStats)),
                   dataset(Insp ~ .,sales),
                   hldSettings(3,0.3,1234,T),
                   itsInfo=TRUE
)


summary(nbs.res)


par(mfrow=c(1,2))
info <- attr(nbs.res,'itsInfo')
PTs.nbs <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2)
)
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.nbs[,,1],PTs.nbs[,,2],
        add=T,lty=2,
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('topright',c('NaiveBayes','smoteNaiveBayes','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.nbs[,,1],PTs.nbs[,,2],
        add=T,lty=2,
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
legend('bottomright',c('NaiveBayes','smoteNaiveBayes','ORh'),
       lty=c(1,2,1),col=c('black','black','grey'))



###################################################
### AdaBoost
###################################################

#ada boost is a learning algorithm that belongs to the class of ensemble models. These type of models are , in effect , formed by a set of base model that contribute,
#to the prediction of the algorithm using some form of aggregation. It can improve the performace of the model. the case weighting schema used by adaboost is interesting 
#from the perspecitive.
                        
                        
library(RWeka)
WOW(AdaBoostM1)


data(iris)
idx <- sample(150,100)
model <- AdaBoostM1(Species ~ .,iris[idx,],
                    control=Weka_control(I=100))
preds <- predict(model,iris[-idx,])
head(preds)
table(preds,iris[-idx,'Species'])
prob.preds <- predict(model,iris[-idx,],type='probability')
head(prob.preds)


ab <- function(train,test) {
  require(RWeka,quietly=T)
  sup <- which(train$Insp != 'unkn')
  data <- train[sup,c('ID','Prod','Uprice','Insp')]
  data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
  model <- AdaBoostM1(Insp ~ .,data,
                      control=Weka_control(I=100))
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}


ho.ab <- function(form, train, test, ...) {
  res <- ab(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}


ab.res <- holdOut(learner('ho.ab',
                          pars=list(Threshold=0.1,
                                    statsProds=globalStats)),
                  dataset(Insp ~ .,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE
)


summary(ab.res)


par(mfrow=c(1,2))
info <- attr(ab.res,'itsInfo')
PTs.ab <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                c(1,3,2)
)
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.ab[,,1],PTs.ab[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh','AdaBoostM1'),
       lty=c(1,1,2),col=c('black','grey','black'))



###################################################
### Semi-supervised approaches
###################################################
set.seed(1234) # Just to ensrure you get the same results as in the book
library(DMwR)
library(e1071)
data(iris)
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]
nb <- naiveBayes(Species ~ .,tr)
table(predict(nb,ts),ts$Species)
trST <- tr
nas <- sample(100,90)
trST[nas,'Species'] <- NA
func <- function(m,d) {
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
}
nbSTbase <- naiveBayes(Species ~ .,trST[-nas,])
table(predict(nbSTbase,ts),ts$Species)
nbST <- SelfTrain(Species ~ .,trST,learner('naiveBayes',list()),'func')
table(predict(nbST,ts),ts$Species)


pred.nb <- function(m,d) {
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}
nb.st <- function(train,test) {
  require(e1071,quietly=T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(Insp ~ .,train,
                     learner('naiveBayes',list()),'pred.nb')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='raw')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}
ho.nb.st <- function(form, train, test, ...) {
  res <- nb.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}


nb.st.res <- holdOut(learner('ho.nb.st',
                             pars=list(Threshold=0.1,
                                       statsProds=globalStats)),
                     dataset(Insp ~ .,sales),
                     hldSettings(3,0.3,1234,T),
                     itsInfo=TRUE
)


summary(nb.st.res)


par(mfrow=c(1,2))
info <- attr(nb.st.res,'itsInfo')
PTs.nb.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                   c(1,3,2)
)
PRcurve(PTs.nb[,,1],PTs.nb[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.nb.st[,,1],PTs.nb.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.nb.st[,,1],PTs.nb.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))


pred.ada <- function(m,d) {
  p <- predict(m,d,type='probability')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}
ab.st <- function(train,test) {
  require(RWeka,quietly=T)
  train <- train[,c('ID','Prod','Uprice','Insp')]
  train[which(train$Insp == 'unkn'),'Insp'] <- NA
  train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(Insp ~ .,train,
                     learner('AdaBoostM1',
                             list(control=Weka_control(I=100))),
                     'pred.ada')
  preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],
                   type='probability')
  return(list(rankOrder=order(preds[,'fraud'],decreasing=T),
              rankScore=preds[,'fraud'])
  )
}
ho.ab.st <- function(form, train, test, ...) {
  res <- ab.st(train,test)
  structure(evalOutlierRanking(test,res$rankOrder,...),
            itInfo=list(preds=res$rankScore,
                        trues=ifelse(test$Insp=='fraud',1,0)
            )
  )
}
ab.st.res <- holdOut(learner('ho.ab.st',
                             pars=list(Threshold=0.1,
                                       statsProds=globalStats)),
                     dataset(Insp ~ .,sales),
                     hldSettings(3,0.3,1234,T),
                     itsInfo=TRUE
)


summary(ab.st.res)


par(mfrow=c(1,2))
info <- attr(ab.st.res,'itsInfo')
PTs.ab.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                   c(1,3,2)
)
PRcurve(PTs.ab[,,1],PTs.ab[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
PRcurve(PTs.ab.st[,,1],PTs.ab.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('topright',c('AdaBoostM1','ORh','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.ab[,,1],PTs.ab[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=1,col='grey',
        avg='vertical')        
CRchart(PTs.ab.st[,,1],PTs.ab.st[,,2],
        add=T,lty=2,
        avg='vertical')        
legend('bottomright',c('AdaBoostM1','ORh','AdaBoostM1-ST'),
       lty=c(1,1,2),col=c('black','grey','black'))


#summary
#outliers detection and ranking
#clustering methods
#semi-supervised learning
#semi-supervised classification through self-learing
#Imabalance class distribution and methods for handling this type of problems
#Naive bayes classification
#adaboost classifiers
#precision/recall and cumulative recall curves
#hold out experiments
