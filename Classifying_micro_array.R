#so starting off with my project Classification Microarray samples
#loading the biobase dataset from biomanager package and loading the package from it.

#library biobase
library(Biobase)

BiocManager::install("Biobase")
BiocManager::install("biocLite")

#library(biocLite)
#loading the dataset
data(ALLb)


#storing the dataset in the different variable
pD <- phenoData(ALLb)

#loading the function with the help of varMetadata
varMetadata(pD)


table(ALLb$BT)
table(ALLb$mol.biol)
table(ALLb$BT,ALLb$mol.bio)

#the first two statement obtain the names and description of the existing co-variates. we then obtain
#some information on the distribution of the sample across the two main co-variates: the BT variable 
#that determines the type of acute lymphoblastic leukemia, and the mol.bio variable that describe the 
#cytogenetic abnormality found on each sample.

#we also obtain some information on the gene and samples:
featureNames(ALLb)[1:10]
sampleNames(ALLb)[1:5]
#the code shows the name of first 10 genes and the names of the first 5 samples


#the first statement obtains the set of cases that we will consider.These are the samples with specific
#values of BT and mol.bio variable. check the call to the table() function we have shown before to see which
#ones we are selecting.
#this can be used for the other form of data extraction process 
#tgt.cases <- which(ALL$BT %in% levels(ALLb$BT)[1:5] & 
                     #ALL$mol.bio %in% levels(ALL$mol.bio)[1:4])
#ALLb <- ALL[,tgt.cases]
#ALLb

#in this context , we should update the available levels of these two factors on our new ALLb object:
ALLb$BT <- factor(ALLb$BT)
ALLb$mol.bio <- factor(ALLb$mol.bio)

#the ALLb object will be the dataset we will use throughout this chapter. IT may eventually be a good 
#idea to save this object in a local file on your computer

save(ALLb,file="myALL.Rdata")

#the function exprs() allows us to access the gene expression levels matrix:
es <- exprs(ALLb)
dim(es)

#in terms of dimensionality , the main challenge of this problem is the fact that there are far too many
#variable for the number of available cases(94),.With these dimensions, most modelling techniiques will have
#a hard time obtaining any meaningful result. In this context, one of the first goals will be to reduce
#the number of variable.


#finding out the summary of the dataset
summary(as.vector(es))

#a better view of the distribution of the expression levels can be obtained graphically.
library(genefilter)#it is very important library

hist(as.vector(es),breaks=80,prob=T,
     xlab='Expression Levels',
     main='Histogram of Overall Expression Levels')
abline(v=c(median(as.vector(es)),
           shorth(as.vector(es)),
           quantile(as.vector(es),c(0.25,0.75))),
       lty=2,col=c(2,3,4,4))
legend('topright',c('Median','Shorth','1stQ','3rdQ'),
       lty=2,col=c(2,3,4,4))


#are the distribution of the gene expression levels of the samples with a particular mutation
#different from each other ? the following code answer this quesion:
sapply(levels(ALLb$mol.bio),function(x) summary(as.vector(es[,which(ALLb$mol.bio == x)])))

#as we see things are rather similar across these subsets of samples and, moreover, they are similar
#to the golbal distribution of expression level

#Gene feature selection
#Features selection is an important task in many data mining problems. The general problem is to select
#the subset of features of a problem that is more relevant for the analysis of the data.This can be regarded
#as instantiation of the more general problem of deciding the weights of the features in the subsequent
#modelling stages.  Generally there are two types of approaches to features selection 1) filter method
#2) wrapper . 

###################################################
### Gene (Feature) Selection
###################################################
rowIQRs <- function(em) 
  rowQ(em,ceiling(0.75*ncol(em))) - rowQ(em,floor(0.25*ncol(em)))
plot(rowMedians(es),rowIQRs(es),
     xlab='Median expression level',
     ylab='IQR expression level',
     main='Main Characteristics of Genes Expression Levels')

#the function rowMedians() from package Biobase obtain a vector of the median per row of a matrix.
#This is an efficient implementation of this task. A less efficient alternative would be to use a function
#apply(), we have used the funtion floor and ceiling(), to obtain the corresponding order in the number
#of values of each row.
library(genefilter)
ALLb <- nsFilter(ALLb,
                 var.func=IQR,
                 var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
ALLb

#finding out the dimension of the filtered dataset
ALLb <- ALLb$eset
es <- exprs(ALLb)
dim(es)

#As you can see, we are only left with only with 4035 genes from the intial 12625.This is a rather
#significant reduction. Nevertheless, we are still far from the dataset that is managable by most
#classification models, given that we only have 94 observation.

#the nsFilter() function is a list with several components. Amoung therse we have several containing
#information on the removed genes, and also the component eset with the "filtered" object.



###################################################
                 #THE ANOVA Filter
####################################################


#the function anova creates a new funciton for carrying out anova filtering. It requires a factor that
#determines the subgroups of our dataset and a statistical significance level. The resulting function is 
#stored in the variable f. The filterfun() function works in similar manner.It generates a filtering 
#function that can be applied to an expression matrix.


f <- Anova(ALLb$mol.bio,p=0.01)
ff <- filterfun(f)
selGenes <- genefilter(exprs(ALLb),ff)


sum(selGenes) 
ALLb <- ALLb[selGenes,]
ALLb


es <- exprs(ALLb)
plot(rowMedians(es),rowIQRs(es),
     xlab='Median expression level',
     ylab='IQR expression level',
     main='Distribution Properties of the Selected Genes')

#namely, any method relying on distance between observation will suffer from this type of problem
#as distance function typically sum up differences between variable values. In this context, variable
#with a higher average values will end up having a larger influence on the distance between observation.
#to avoid this effect , it is usual to standardize the data. This transformation consists of substraction
#the typical values of the variable and dividing the result by a measure of spread. Given that not all 
#modelling techniques are affected by this data characterstics. We will leave this transformation to the 
#modelling stages, making it depend on the tool to be used.

#############################################################################
             #filtering using randomForest
##############################################################################


#Random forests can be used to obtain a ranking of the features in terms of their usefulness for a 
#classification task.The function make.name() can be used to solve this problem as follows:
featureNames(ALLb) <- make.names(featureNames(ALLb))
es <- exprs(ALLb)

#the function featureName(), provides access to the names of the genes in an expression set.

library(randomForest)
dt <- data.frame(t(es),Mut=ALLb$mol.bio)
rf <- randomForest(Mut ~  .,dt,importance=T)
imp <- importance(rf)
imp <- imp[,ncol(imp)-1]
rf.genes <- names(imp)[order(imp,decreasing=T)[1:30]]


sapply(rf.genes,function(g) tapply(dt[,g],dt$Mut,median))

#we obtained a random forest with the parameter importance set to TRUE to obtain estimates of the 
#importance of the variable. The function importance(), is used to obtain the relevance of each variable.

#we obtained several intresting differences between the median expression level across the type of mutation
#which provides a good indication of the discriminative power of these genes.
#we can obtain even more details by grphically inspecting the concrete expression values of these genes
#for the 94



#plotting the graph for several genes with marked differences in expression level across that there are
#several genes with marked differeces in expression level across.
library(lattice)
ordMut <- order(dt$Mut)
levelplot(as.matrix(dt[ordMut,rf.genes]),
          aspect='fill', xlab='', ylab='',
          scales=list(
            x=list(
              labels=c('+','-','*','|')[as.integer(dt$Mut[ordMut])],
              cex=0.7,
              tck=0)
          ),
          main=paste(paste(c('"+"','"-"','"*"','"|"'),
                           levels(dt$Mut)
          ),
          collapse='; '),
          col.regions=colorRampPalette(c('white','orange','blue'))
)


#filtering using features clustering ensembles

#in the below described process we will going to use the clustering method in order to group it in 30 groups
#of varibles that are supposed to be similar. These 30 variables clusters will then be used to obtian an
#ensemble classification model where m models will be obtained with 30 variables, each one randomly 
#chosen form one of the 30 clusters.

library(Hmisc)
vc <- varclus(t(es))
clus30 <- cutree(vc$hclust,30)
table(clus30)

plot(clus30)
#we used the cut tree to obtain a clustering formed by 30 groups of variables. we then checked how many 
#variable belong to each clusters. Based on the clustering we create the sets of predictors by randomly 
#selecting one variable from each clusters.

getVarsSet <- function(cluster,nvars=30,seed=NULL,verb=F) 
{
  if (!is.null(seed)) set.seed(seed)
  
  cls <- cutree(cluster,nvars)
  tots <- table(cls)
  vars <- c()
  vars <- sapply(1:nvars,function(clID)
  {
    if (!length(tots[clID])) stop('Empty cluster! (',clID,')')
    x <- sample(1:tots[clID],1)
    names(cls[cls==clID])[x]
  })
  if (verb)  structure(vars,clusMemb=cls,clusTots=tots)
  else       vars
}
getVarsSet(vc$hclust)
getVarsSet(vc$hclust)

#each time we call this function , we will get a "new" set of 30 variable. Using this function it is easy
#to generate a set of datasets formed by different predictors and then obtain a model using each of these
#sets


#predicting cryptogenetic Abnormalities
#Defining the prediction tasks
###################################################
### Predicting Cytogenetic Abnormalities
###################################################

#The Experimental Procedure

#in this context , the more adequate experimental methodology to obtain raliable estimates of the error
#rate is the Leave-one-out cross validation(LOOCV).LOOCV is a special case of the k-fold cross validation

data(iris)
rpart.loocv <- function(form,train,test,...) {
  require(rpart,quietly=T)
  m <- rpart(form,train,...)
  p <- predict(m,test,type='class')
  c(accuracy=ifelse(p == resp(form,test),100,0))
}

library(Ecdat) #need to load the library before loading the function
library(caret)
library(DMwR)#loocv is a part of this package
exp <- loocv(learner('rpart.loocv',list()),
             dataset(Species~.,iris),
             loocvSettings(seed=1234,verbose=F))


summary(exp)


#the modelling techniques
#to handle the problem we have selected three modelling techniques. Two of them already been used before.
#they are random forests and support vector machines(SVMs). They are recognised as some of the best off-the-self
#prediction methods. The third algorithms we will try on this problem is new. It is a method based on 
#distances between observation , known as k-nearest neighbors.

#random forest can handle problems with a large number of features. This property adequate to handle problems
#with a large number of features.


#the k-nearest neighbour algorithms belongs to the class of so-called lazy learning. These type of techniques
#do not actually obtain a model from the training data. They simply store this dataset. The main
#work happens at the prediction time. Given, a new test case, its prediction is obtained by searching for 
#similar cases in the training data that was stored.


library(class)
data(iris)
idx <- sample(1:nrow(iris),as.integer(0.7*nrow(iris)))
tr <- iris[idx,]
ts <- iris[-idx,]
preds <- knn(tr[,-5],ts[,-5],tr[,5],k=3)
table(preds,ts[,5])


kNN <- function(form,train,test,norm=T,norm.stats=NULL,...) {
  require(class,quietly=TRUE)
  tgtCol <- which(colnames(train)==as.character(form[[2]]))
  if (norm) {
    if (is.null(norm.stats)) tmp <- scale(train[,-tgtCol],center=T,scale=T)
    else tmp <- scale(train[,-tgtCol],center=norm.stats[[1]],scale=norm.stats[[2]])
    train[,-tgtCol] <- tmp
    ms <- attr(tmp,"scaled:center")
    ss <- attr(tmp,"scaled:scale")
    test[,-tgtCol] <- scale(test[,-tgtCol],center=ms,scale=ss)
  }
  knn(train[,-tgtCol],test[,-tgtCol],train[,tgtCol],...)
}
preds.norm <- kNN(Species ~ .,tr,ts,k=3)
table(preds.norm,ts[,5])
preds.notNorm <- kNN(Species ~ .,tr,ts,norm=F,k=3)
table(preds.notNorm,ts[,5])

#the function allows the user to indicate if the data should be normalized prior to the call to the Knn()
#function. This is done thorugh the parameter norm. A third alternative is to provide the centrality and 
#spread statistics as  a list with two components in the argument norm.stats. 


#comparing the model
#This section describe the process we have used to compare the selected models using a LOOCV experimental
#methodology . For each iteration of the LOOCV process , a features selection process , is carried out
# prior to predictive model construction, using only the training data provided by the LOOCV routines.

vars <- list()
vars$randomForest <- list(ntree=c(500,750,100),
                          mtry=c(5,15,30),
                          fs.meth=list(list('all'),
                                       list('rf',30),
                                       list('varclus',30,50)))
vars$svm <- list(cost=c(1,100,500),
                 gamma=c(0.01,0.001,0.0001),
                 fs.meth=list(list('all'),
                              list('rf',30),
                              list('varclus',30,50)))
vars$knn <- list(k=c(3,5,7,11),
                 norm=c(T,F),
                 fs.meth=list(list('all'),
                              list('rf',30),
                              list('varclus',30,50)))

#the list has three component, one for each component, one for each of the algoritms being compared.
#the first uses the features resulting from the anova ,statistical test. The secound select 30 genes from the set
#obtained from the anova test, using the variable clustering ensemble strategy that we described previously

varsEnsembles <- function(tgt,train,test,
                          varsSets,
                          baseLearner,blPars,
                          verb=F)
{
  preds <- matrix(NA,ncol=length(varsSets),nrow=NROW(test))
  for(v in seq(along=varsSets)) {
    if (baseLearner=='knn')
      preds[,v] <- knn(train[,varsSets[[v]]],
                       test[,varsSets[[v]]],
                       train[,tgt],blPars)
    else {
      m <- do.call(baseLearner,
                   c(list(as.formula(paste(tgt,
                                           paste(varsSets[[v]],
                                                 collapse='+'),
                                           sep='~')),
                          train[,c(tgt,varsSets[[v]])]),
                     blPars)
      )
      if (baseLearner == 'randomForest')
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])],
                                  type='response'))
      else
        preds[,v] <- do.call('predict',
                             list(m,test[,c(tgt,varsSets[[v]])]))
    }
  }
  ps <- apply(preds,1,function(x)
    levels(factor(x))[which.max(table(factor(x)))])
  ps <- factor(ps,
               levels=1:nlevels(train[,tgt]),
               labels=levels(train[,tgt]))
  if (verb) structure(ps,ensemblePreds=preds) else ps
}


#these prediction are obtained through the voting mechanism amoung the members of the ensemble. The difference
#between the members of the ensemble lies only in the predictors that are used, which are determined by 
#the varssets parameter.these sets results from a variable clustering process.

#we have created a single user-defined modelling function that will receive as one of the parameter the 
#learner that is to be used.


genericModel <- function(form,train,test,
                         learner,
                         fs.meth,
                         ...)
{
  cat('=')
  tgt <- as.character(form[[2]])
  tgtCol <- which(colnames(train)==tgt)
  
  # Anova filtering  
  f <- Anova(train[,tgt],p=0.01)
  ff <- filterfun(f)
  genes <- genefilter(t(train[,-tgtCol]),ff)
  genes <- names(genes)[genes]
  train <- train[,c(tgt,genes)]
  test <- test[,c(tgt,genes)]
  tgtCol <- 1
  
  # Specific filtering 
  if (fs.meth[[1]]=='varclus') {
    require(Hmisc,quietly=T)
    v <- varclus(as.matrix(train[,-tgtCol]))
    VSs <- lapply(1:fs.meth[[3]],function(x)
      getVarsSet(v$hclust,nvars=fs.meth[[2]]))
    pred <- varsEnsembles(tgt,train,test,VSs,learner,list(...))
    
  } else {
    if (fs.meth[[1]]=='rf') {
      require(randomForest,quietly=T)
      rf <- randomForest(form,train,importance=T)
      imp <- importance(rf)
      imp <- imp[,ncol(imp)-1]
      rf.genes <- names(imp)[order(imp,decreasing=T)[1:fs.meth[[2]]]]
      train <- train[,c(tgt,rf.genes)]
      test <- test[,c(tgt,rf.genes)]
    }
    
    if (learner == 'knn') 
      pred <- kNN(form,
                  train,
                  test,
                  norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                                  rowIQRs(t(as.matrix(train[,-tgtCol])))),
                  ...)
    else {
      model <- do.call(learner,c(list(form,train),list(...)))
      pred <- if (learner != 'randomForest') predict(model,test)
      else predict(model,test,type='response')
    }
    
  }
  
  c(accuracy=ifelse(pred == resp(form,test),100,0))
}


#the user defined function will be called from within the loocv routines for each iteration of the process.
#the experiment with all these variants on the microarray data will take a long time to complete.

require(class,quietly=TRUE)
require(randomForest,quietly=TRUE)
require(e1071,quietly=TRUE)

load('myALL.Rdata')
es <- exprs(ALLb)
# simple filtering
ALLb <- nsFilter(ALLb,
                 var.func=IQR,var.cutoff=IQR(as.vector(es))/5, 
                 feature.exclude="^AFFX")
ALLb <- ALLb$eset

# the data set
featureNames(ALLb) <- make.names(featureNames(ALLb))
dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)
DSs <- list(dataset(Mut ~ .,dt,'ALL'))
# The learners to evaluate
library(MASS)
library(nnet)

TODO <- c('knn','svm','randomForest')
for(td in TODO) {
  assign(td,
         experimentalComparison(
           DSs,
           c(
             do.call('variants',
                     c(list('genericModel',learner=td),
                       vars[[td]],
                       varsRootName=td))
           ),
           loocvSettings(seed=1234,verbose=F)
         )
  )
  save(list=td,file=paste(td,'Rdata',sep='.'))
}


#need to load the dataset form the website because the above code will take around 3 days to fun
#on the normal computer,so we need to download the dataset and run it from there in our computer.

load('knn.Rdata')
load('svm.Rdata')
load('randomForest.Rdata')


rankSystems(svm,max=T)

#the function rank system() takes an object of class compExp and obtain the best performing variats
#for each of the statistics that were estimated in the experimental process. 

#in order to have the overall perspective of all trials , we can join the three objects:

all.trials <- join(svm,knn,randomForest,by='variants')

#with the compExp object we can check the best overall score of our trials:
rankSystems(all.trials,top=10,max=T)

#the top score obtained by a variant of the k-nearest neighbour method. Let us check its characterstics
getVariant('knn.v2',all.trials)
#the varient uses 30 genes filtered by a random forest , five neighbors and normalization of the gene
#expression values. It is also interesting to observe that amoung the top ten scores only the last one 
#does not use the the 30 gene filters with a random forest. The tenth best model uses all genes resulting
#from the anova filtering.

#It may be interesting to know which error were made by the models, for instance, the best model, confusion
#matrix provide this type of information, TO obtain a confusion matrix we need to know what the actual 
#prediction of the model are. 

bestknn.loocv <- function(form,train,test,...) {
  require(Biobase,quietly=T)
  require(randomForest,quietly=T)
  cat('=')
  tgt <- as.character(form[[2]])
  tgtCol <- which(colnames(train)==tgt)
  # Anova filtering
  f <- Anova(train[,tgt],p=0.01)
  ff <- filterfun(f)
  genes <- genefilter(t(train[,-tgtCol]),ff)
  genes <- names(genes)[genes]
  train <- train[,c(tgt,genes)]
  test <- test[,c(tgt,genes)]
  tgtCol <- 1
  # Random Forest filtering
  rf <- randomForest(form,train,importance=T)
  imp <- importance(rf)
  imp <- imp[,ncol(imp)-1]
  rf.genes <- names(imp)[order(imp,decreasing=T)[1:30]]
  train <- train[,c(tgt,rf.genes)]
  test <- test[,c(tgt,rf.genes)]
  # knn prediction
  ps <- kNN(form,train,test,norm=T, 
            norm.stats=list(rowMedians(t(as.matrix(train[,-tgtCol]))),
                            rowIQRs(t(as.matrix(train[,-tgtCol])))),
            k=5,...)
  structure(c(accuracy=ifelse(ps == resp(form,test),100,0)),
            itInfo=list(ps)
  )
}
resTop <- loocv(learner('bestknn.loocv',pars=list()),
                dataset(Mut~.,dt),
                loocvSettings(seed=1234,verbose=F),
                itsInfo=T)
 
#the bestknn.loocv() function is essentially a specialization of the function genericModel()
#we have seen before, but focused , on 5 -nearest neighbors with random forest filtering and 
#noramlization using median and IQRs. Most of the code is the same as in generic model, return a structure.





#in the end we can inspect this information and in this case see what were the actual prediction of the 
#best model on iteraction.

#we can check the content of the attribue containing the wanted information as follows

attr(resTop,'itsInfo')[1:4]


dt <- data.frame(t(exprs(ALLb)),Mut=ALLb$mol.bio)


preds <- unlist(attr(resTop,'itsInfo'))
table(preds,dt$Mut)

#The confusion matrix can be used to inspect the type of errors that the model makes. For instances, we can
#observe that the model correctly predicts all cases with the ALL1/AF4 mutation. Moreover, we can also 
#obsrve that most of the error of the model consists of predicting the class NEG for a case with some 
#mutation. Nevertheless, the reverse also happens with five samples with no mutation.


#SUMMARY
#1) Feature selection methods for problems with a very large number of predictors
#2) classification model
#3)Random forest
#4) k - nearest neighbour 
#4) svms
#5) ensembles using different subsets of predictors 
#6) leave - one- out cross- validatioin experiments
