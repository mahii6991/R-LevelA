data(nhanes, package = "dsEssex") #loading the dataset into r

#loading the required packages
require(plotly)
require(ggcorrplot)
require(htmlwidgets)

#loading the library ggplot2

library(ggplot2)#loading it
g <- ggplot(data= nhanes,aes(x=BMI,y=BPDiaAve)) + geom_point(size=2, shape=9) #plotting the graph

#using the interective library to plot the graph
ggplotly(g)

#using the diplyr library
library(dplyr)
#finding out the condition according to the given question
new_nhanes <- nhanes %>%
  filter(BPDiaAve == 0, is.na(Diabetes), is.na(AgeDecade) | Age < 20)

#using the mutate function to finding out the levels of the dataset
as.factor(nhanes$Diabetes)

str(nhanes)
#converting the levels into two groups
levels(nhanes$Diabetes) <- c("Non-Diabetic", "Diabetic")

# Plot the BPDiaAve Vs. BMI (colored by Gender) with facets based on Gender on rows and Diabetes on columns.
p <- ggplot(data = nhanes, aes(BPDiaAve,BMI ,color= Gender)) + geom_point()
M <-p + facet_grid(Gender ~ Diabetes) 

#producing the interactive plot for this data
N <-ggplotly(M)


#saving the above generated file 
htmlwidgets::saveWidget(widget = N, "corr.html")


#making the histogram
O<-ggplot(nhanes, aes(x= Age)) + geom_histogram(bins = 15)

#saving the interactive plot to different file
Fig_hist <- ggplotly(O)
#the the file with the variable name in it
Fig_hist

#saving the histogram
htmlwidgets::saveWidget(widget = Fig_hist, "corr.html")


#Generate a box-plot for the BPSysAve data by Gender
A <- ggplot(nhanes, aes(x=BPSysAve,y= Gender)) + geom_boxplot()
#saving the interactive plot in the other variable
B_hist <- ggplotly(A)
B_hist
