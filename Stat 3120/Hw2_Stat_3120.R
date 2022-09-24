###############################################################################
#Zachary Delk
#Stat 3120
#Homework 2
###############################################################################
#install.packages("fastR2")
library(fastR2)
#Problem 1:    GSS-18
#95% CI for proportion of Americans in 2018 for Dem Party

demData = subset(GSS_18, !is.na(polparty))
count1 = length(demData$polparty)
count2 = length(which(demData$polparty == 'Democrat'))
wald.ci(x = count2, n = count1, conf.level = 0.95)



#99% CI for prop who opposed to legalization of marijuana

addmargins(table(GSS_18$marijuan))

marData = subset(GSS_18, !is.na(GSS_18$marijuan))
count3 = length(marData$marijuan)
count4 = length(which(marData$marijuan == 'NotLegal'))
wald.ci(x = count4, n = count3, conf.level = 0.99)


#two-way table between political party and gun ownership (remove missing vals)

gunpolData = subset(demData, !is.na(demData$owngun))

prop.table(table(gunpolData$polparty, gunpolData$owngun),1)

#90% CI for diff in prop. of Rep. and Dem who owned guns
count5 = length(which(gunpolData$polparty == 'Democrat'))
count6 = length(which(gunpolData$polparty == 'Republican'))
count7 = length(which(gunpolData$polparty == 'Democrat' & gunpolData$owngun == 'Yes'))
count8 = length(which(gunpolData$polparty == 'Republican' & gunpolData$owngun == 'Yes'))
prop.test(x = c(count7, count8), n = c(count5, count6), alternative = "two.sided", correct = FALSE)

###############################################################################
#Problem 2:      Beans
#90% CI for mean diff in beans for dom and non dom hand for 15 secs

t.test(beans$Dom, beans$NonDom, conf.level = 0.90, paired = TRUE)

###############################################################################
#Problem 3:      pennstate1
#95% CI for diff in mean response between men and women

t.test(pennstate1$fastest ~ pennstate1$Sex, conf.level = 0.95)
###############################################################################
#Problem 4:      Deprived
#99% CI for avg num of hour sleep per night

t.test(deprived$SleepHrs, conf.level = 0.99)

#means of sleep deprived and non sleep deprived.
dep = subset(deprived, deprived$Deprived == 'Yes')
notdep = subset(deprived, deprived$Deprived == 'No')
depmean = mean(dep$SleepHrs)
notdepmean = mean(notdep$SleepHrs)

dep_diff = notdepmean - depmean
#95% CI for diff in pop mean of hours sleep for deprived and non

t.test(deprived$SleepHrs ~ deprived$Deprived, conf.level = 0.95)

###############################################################################
#Problem 5:     cars
#test stat and p val

prop.test(60, 150, alternative = "greater", p =0.3, conf.level = 0.95)

###############################################################################
#Problem 6:     UCDavis1
#hypothesis test for seat

addmargins(table(UCDavis1$Sex, UCDavis1$Seat))


malefront = length(which(UCDavis1$Sex == 'Male' & UCDavis1$Seat == 'Front'))
femalefront = length(which(UCDavis1$Sex == 'Female' & UCDavis1$Seat == 'Front'))
totalmale = length(which(UCDavis1$Sex == 'Male'))
totalfemale = length(which(UCDavis1$Sex == 'Female'))

prop.test(x = c(femalefront, malefront), n = c(totalfemale, totalmale), conf.level = 0.99, alternative = 'greater', correct = FALSE)
###############################################################################
#Problem 7:    UCDavis1
#hypothesis test. diff of mean in gpa for front and back students

nomid = subset(UCDavis1, UCDavis1$Seat != 'Middle')

t.test(nomid$GPA ~ nomid$Seat, conf.level = 0.90)
###############################################################################
#Problem 8:    blood pressure
#hypothesis test. mean blood pressure for waiting for dentist

t.test(blood_pressure$bp_before, blood_pressure$bp_after, paired = TRUE)

