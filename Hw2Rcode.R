#Question 1
#Showing the Descriptive Statistics for the Cancer Risk data set
#changed the name to Cancer for ease of use
#copied values into excel to make the table
summary(Cancer)
##################################################################
#Question 2: Histograms and Boxplots

install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
#Age Plots
hist(Cancer$Age,col=c("hotpink","aquamarine","grey67"),
     main="Figure 1a: Histogram of Age",xlab="Age", breaks=8)
#,
 #    breaks=seq(10,90,15), cex.lab = 1.5, cex.main = 2)

boxplot(Cancer$Age,col="azure",horizontal=T,main="Figure 1b: Boxplot for Age",xlab="Age", cex.lab = 1.5, cex.main = 2)

#Calories Plots
hist(Cancer$Calories,col=c("hotpink","aquamarine","grey67"),
     main="Figure 2a: Histogram of Calories",xlab="Calories", breaks=8)
       #seq(0,7000,875), cex.lab = 1.5, cex.main = 2)

boxplot(Cancer$Calories,col="azure",horizontal=T,main="Figure 2b: Boxplot for Calories",xlab="Calories", cex.lab = 1.5, cex.main = 2)

#Fat Plots
hist(Cancer$Fat,col=c("hotpink","aquamarine","grey67"),
     main="Figure 3a: Histogram of Fat",xlab="Fat",
     breaks=seq(0,250,31.25),  cex.lab = 1.5, cex.main = 2)

ggplot(Cancer, aes(x = Fat)) + #Set up plot base. Add new layer.
  geom_histogram(binwidth = 30, center = 15, colour = "white", fill="red") +  
  ggtitle("GGplot Fat") #Add title 

boxplot(Cancer$Fat,col="azure",horizontal=T,main="Figure 3b: Boxplot for Fat",xlab="Fat",  cex.lab = 1.5, cex.main = 2)


#Quesion 3
#creating a new variable agecat to categorize the age
Cancer$agecat[Cancer$Age<40] <- "Youngest"
Cancer$agecat[Cancer$Age>=40 & Cancer$Age<48] <- "Young"
Cancer$agecat[Cancer$Age>=48 & Cancer$Age<62] <- "Old"
Cancer$agecat[Cancer$Age>=62] <- "Oldest"

#3a
#Orders the agecat variable from youngest to oldest
ORDJT <- ordered(Cancer$agecat,c('Youngest','Young','Old','Oldest'))
#creates a standart table
table(ORDJT)
#rounded proportional table out of 100 rouned to 2 decimal places
round(prop.table(table(ORDJT))*100,2)

#3b: Pie chart for agecat
library(ggplot2)
age_table = (table(ORDJT)) #Build frequency table
at_df = as.data.frame(age_table) #Save table as a data frame
names(at_df) <- c("Age", "Frequency")  #Change data frame variable names
ggplot(at_df, aes (x="", y = Frequency, fill = factor(Age))) +  
  #Plot the frequency values for Plant
  geom_bar(width = 1, stat = "identity") + #Build the chart
  #Convert frequencies to percentages and add % sign
  #Position percentages vertically justified at .5 (center them)
  geom_text(aes(label = paste(round(Frequency/ sum(Frequency) * 100, 1), "%")),  
            position = position_stack(vjust = 0.5)) + 
  theme_classic() + #Set theme
  theme(plot.title = element_text(hjust=0.5), #Center chart title
        axis.line = element_blank(), #Do not include axis line
        axis.text = element_blank(),#Do not include axis text
        axis.ticks = element_blank()) + #Do not include axis tick marks
  labs(fill = "Age Category", #Make legend title say Plant
       x = NULL, y = NULL, size = 12,
       title = ("Figure 4a: Pie Chart for agecat")) + #Create graph title
  scale_fill_manual(values = c("lightblue", "tomato1",'aquamarine4','hotpink'), 
                    labels = c('Youngest','Young','Old','Oldest')) + 
  coord_polar("y") #Make the pie 

#3c: Barplot for age cat
ACtable <- table(ORDJT)
barplot(ACtable, main = 'Figure 4b: Barplot for AgeCat',
        xlab = 'Age Category',
        col=c("lightblue", "tomato1",'aquamarine4','hotpink'))

#####################################################################################

table(Cancer$SmokeStat,Cancer$Gender)
round(prop.table(table(Cancer$SmokeStat,Cancer$Gender)),2)
round(prop.table(table(Cancer$SmokeStat,Cancer$Gender),1),2)
round(prop.table(table(Cancer$SmokeStat,Cancer$Gender),2),2)
######################################################################################
proportions <- round(prop.table(table(Cancer$SmokeStat,Cancer$Gender),2)*100,2)
proportions_df  <- as.data.frame(proportions) #Make table values into a data frame
names(proportions_df) <- c("SmokeStat","Gender","Percent")#Change variable names

library(ggplot2)
library(ggthemes)
ggplot(data = proportions_df, aes(x = Gender, y = Percent, 
                                  fill = SmokeStat, label = Percent)) +
  geom_bar(stat = "identity",show.legend = TRUE) +
  geom_text(size = 8, position = position_stack(vjust = 0.5))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("darkcyan","darkorchid","tomato1"), 
                    labels = c("Never","Former","Current")) +
  scale_x_discrete(labels = c("1" = "Male","2" = "Female"))  +
  ggtitle("Figure 5: Stacked Bar Chart of Gender by SmokeStat")
######################################################################################


set.seed(8675309)
sample1 <- Cancer[sample(1:315,80,replace=F),]
sample1
dim(sample1)

CI <- function(x, alpha = .05, dec = 3){ 
  n <- sum(!is.na(x))
  conf_level <- (1-alpha)*100
  me <- qt(1-alpha/2, n-1)*sd(x, na.rm=T)/sqrt(n)
  mean <- round(mean(x, na.rn = T), digits = dec)
  lower <- round(mean(x, na.rn = T) - me, digits = dec)
  upper <- round(mean(x, na.rn = T) + me, digits = dec)
  {limits <-data.frame(cbind(variable = deparse(substitute(x)), n, 
                             c.level = conf_level, mean, me = round(me, digits = dec), 
                             lower, upper))}
  print(limits) 
  rm(n, conf_level, lower, upper, mean)}

CI(sample1$Calories, 0.05, 2)
round(prop.table(table(Cancer$SmokeStat,Cancer$Gender)),2)*100
round(prop.table(table(Cancer$SmokeStat,Cancer$Gender),1),2)*100
round(prop.table(table(Cancer$SmokeStat,Cancer$Gender),2),2)*100

#####################################################################################


boxplot(Cancer$Calories~Cancer$agecat,horizontal=T,
        col=c("lightblue","tomato1"))
summary(Cancer$Calories)

Cancer$calcat[Cancer$Calories<1000] <- "Low"
Cancer$calcat[Cancer$Calories>=1000 & Cancer$Calories<2000] <- "Medium"
Cancer$calcat[Cancer$Calories>=2000 & Cancer$Calories<4000] <- "High"
Cancer$calcat[Cancer$Calories>=4000] <- "Damn"

ORDCC <- ordered(Cancer$calcat,c('Low','Medium','High','Damn'))
#creates a standard table
table(ORDCC)
#rounded proportional table out of 100 rouned to 2 decimal places
round(prop.table(table(ORDCC))*100,2)

##################################################################################
#Question 7: Scatter plot
library(ggplot2)
ggplot(data = Cancer, aes(x = ORDJT, y = ORDCC)) + 
  geom_point() + # Add the data points
  #Add the line of best fit
  geom_smooth(method=lm, se=F, linetype="solid", color="gray62") + 
  ggtitle("Age Calories Scatter") # Add title



boxplot(ORDJT~ORDCC,horizontal=T,
        col=c("lightblue","tomato1"))
