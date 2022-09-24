#stuff for graphs
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
##############################################################
#Q1
#Old Faithful histogram
hist(oldfaithful$TimeToNext,col=c("hotpink"),
     main="Figure 1a: Histogram of TimeToNext",xlab="minutes")

#Olf Faithful boxplot
boxplot(oldfaithful$TimeToNext,col="azure",horizontal=T,main="Figure 1b: Boxplot for TimetoNext",xlab="minutes", cex.lab = 1.5, cex.main = 2)

#Big 5 for Old Faithful
summary(oldfaithful)
sd(oldfaithful$Duration, na.rm = TRUE)
sd(oldfaithful$TimeToNext)
################################################################
#Q2
#Percentage table for gunlaw
LawTable <- prop.table(table(Gss$gunlaw))*100

#Barplot for gunlaw
barplot(LawTable, col="azure",main="Figure 2a: Barplot for GunLaw",xlab="status")

#Percent of Total
prop.table(table(Gss$sex, Gss$gunlaw))*100
#Percent of Gender (row)
prop.table(table(Gss$sex, Gss$gunlaw), 1)*100
#Percent of gunLaw (column)
prop.table(table(Gss$sex, Gss$gunlaw), 2)*100

#5NS for tvhours\
summary(Gss$tvhours, na.rm = TRUE)
sd(Gss$tvhours, na.rm = TRUE)

#Boxplot of tvhours
boxplot(Gss$tvhours,col="azure",horizontal=T,main="Figure 2b: Boxplot for tvhours",xlab="Hours", cex.lab = 1.5, cex.main = 2)
####################################################################
#Q3
#histogram of CD
hist(pennstate2$CDs,col=c("hotpink"),
     main="Figure 3a: Histogram of CDs",xlab="# of Cds")

#summary of pennstat2 for mean and median
summary(pennstate2$CDs)
#####################################################################
#Q4
#histogram of HrsSleep
hist(pennstate1$HrsSleep,col=c("hotpink"),
     main="Figure 4a: Histogram of Hours of Sleep",xlab="# of hours")

#summary of hrssleep
summary(pennstate1$HrsSleep)

#histogram of height
hist(pennstate1$Height,col=c("hotpink"),
     main="Figure 4b: Histogram of Height",xlab="Height in inches")

#intialize subset
height = pennstate1$Height

results = c(0)            # initiate home for the results
for (n in 1:5000)        # do 5,000 re-samples
{
  sample = sample(height, 60, replace = TRUE) # select a random sample
  p_hat = sum(sample) / length(sample)     # Calculate sample proportion
  results[n] = p_hat       # store the sample proportion in the results file
}

#histogram of samples of height
hist(results,col=c("hotpink"),
     main="Figure 4c: Histogram of Height Samples",xlab="Height in inches")
##############################################################################
#Q5
#random samples

#Exponential Sample of size 5
exp_results = c(0)            # initiate home for the results
for (n in 1:2000)        # do 2,000 re-samples
{
  sample = rexp(5,0.1) # select a random sample
  p_hat = sum(sample) / length(sample)     # Calculate sample proportion
  exp_results[n] = p_hat       # store the sample proportion in the results file
}

#Histogram for 5 points
hist(exp_results,col=c("hotpink"),
     main="Figure 5a: Histogram of n=5",xlab="Height in inches")

#Exponential Sample of size 50
exp_results = c(0)            # initiate home for the results
for (n in 1:2000)        # do 2,000 re-samples
{
  sample = rexp(50,0.1) # select a random sample
  p_hat = sum(sample) / length(sample)     # Calculate sample proportion
  exp_results[n] = p_hat       # store the sample proportion in the results file
}

#Histogram for 50 points
hist(exp_results,col=c("hotpink"),
     main="Figure 5b: Histogram of n=50",xlab="Height in inches")

#Exponential Sample of size 500
exp_results = c(0)            # initiate home for the results
for (n in 1:2000)        # do 2,000 re-samples
{
  sample = rexp(500,0.1) # select a random sample
  p_hat = sum(sample) / length(sample)     # Calculate sample proportion
  exp_results[n] = p_hat       # store the sample proportion in the results file
}

#Histogram for 500 points
hist(exp_results,col=c("hotpink"),
     main="Figure 5c: Histogram of n=500",xlab="Height in inches")


