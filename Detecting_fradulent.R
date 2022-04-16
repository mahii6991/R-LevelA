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


