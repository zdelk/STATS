################################################################
#Zachary Delk
#Stat 4030
#Midterm Exam Code
###############################################################
#loading library
library(readr)
library(haven)
#importing and cleaning dataset
titanic <- read_csv("D:/stats/Stat_4030/Midterm/titanic.csv")
View(titanic)
titanic = titanic[complete.cases(titanic)]

jun03 <- read_csv("D:/stats/Stat_4030/Midterm/midterm_dental_clinic/charge_data_Jun03.csv")
View(charge_data_Jun03)

jun04 <- read_csv("D:/stats/Stat_4030/Midterm/midterm_dental_clinic/charge_data_Jun04.csv")
View(charge_data_Jun04)

clinical_data <- read_csv("D:/stats/Stat_4030/Midterm/midterm_dental_clinic/clinical_data.csv")
View(clinical_data)

who_data <- read_csv("D:/stats/Stat_4030/Midterm/WHO_Life_Expectancy_Data_2000.csv")
View(who_data)

Stroke <- read_csv("D:/stats/Stat_4030/Midterm/Framingham_Stroke_Data.csv")
View(Stroke)
#sub-setting into living passengers
lived = subset(titanic, titanic$Survived == 1)
lived <- lived[complete.cases(lived),]
avg_age = mean(lived$Age)

#sub-setting into dead passengers
died = subset(titanic, titanic$Survived == 0)
died <- died[complete.cases(died),]

#data for lived datset
summary(lived)

#t.test for lived vs died
t.test(lived$Age, died$Age, 
       alternative = "two.sided", var.equal = FALSE)


#merging jun03 and jun04
concat <- rbind(jun03, jun04)
#looking at how many unique patients there are
length(!unique(concat$Patient_ID))
#creating dataset of all unique patients
inner_merged <- merge(x = jun03, y = jun04, by = "Patient_ID", all.x = TRUE)
length(inner_merged)

#removing the country variable from the data
who_data <- who_data[ , -which(colnames(who_data)=="Country")]

#building regression model for who_data
lm_results <- lm(who_data$`Life expectancy` ~ who_data$`infant deaths` + who_data$GDP + who_data$`HIV/AIDS`+
                   who_data$Schooling, data=who_data)
summary(lm_results)

#cleaning Stroke datset
Stroke = Stroke[complete.cases(Stroke), ]
#Creating subset for patients that need to be screened for a stroke
stroke_need_screening <- Stroke[(Stroke$AGE>=55) & (Stroke$AGE <= 74) & (Stroke$CIGPDAY>=30), ]

