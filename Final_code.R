setwd("D:/stats/Stat_4030/Final Project")

library(readr)
hospital = read.csv("hospital_readmission.csv")
view(hospital)

females = hospital[hospital$female =='TRUE',]


# 2 categorical
addmargins(table(females$diabetes, females$race_minority))

prop.test(x = c(2312, 7925), n = c(2989, 10448), alternative = "greater", correct = FALSE)

addmargins(table(females$race_minority))

#2 numerical 
plot(females$time_in_hospital, females$num_medications, 
     main="Figure 1: Time in hospital by Number of Medications", 
     xlab="Time in Hospital", ylab="Number of Medications", #Add axis labels
     pch=20, col= "green4")
#grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
abline(lm(females$num_medications ~ females$time_in_hospital), col = "darkorange3", lty = 2, lwd = 2)

t_co2 = lm(females$num_medications ~ females$time_in_hospital)
summary(t_co2)



#cat and numerical
readd_out = females[females$readmitted_after_discharge == 1,]$number_previous_inpatient_visit
noreadd_out = females[females$readmitted_after_discharge == 0,]$number_previous_inpatient_visit


# Normality test
## shapiro test: p value < .05 data not normally distributed
## p value > .05, can assume normality
hist(readd_out)
hist(noreadd_out)
shapiro.test(readd_out)
shapiro.test(noreadd_out)

## Box plot for visualizing the difference
boxplot(readd_out, noreadd_out,
        names = c("Readmitted", "Not readmitted"),
        col = "yellow",
        border = "dark orange",
        ylab = "Previous Inpatient Visits",
        main = "Fig 2a: Inpatient Visit Difference (w/ outliers)")


boxplot(readd_out, noreadd_out, outline = FALSE,
        names = c("Readmitted", "Not readmitted"),
        col = "yellow",
        border = "dark orange",
        ylab = "Previous Inpatient Visits",
        main = "Fig 2b: Inpatient Visit Difference (w/o outliers)")

t.test(readd_out, noreadd_out, 
       alternative = "greater", var.equal = FALSE)

head(hospital)