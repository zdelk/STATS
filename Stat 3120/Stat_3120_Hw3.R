####################################################################
#Zachary Delk
#Stat 3120
#Homework 3
####################################################################
#loading libraries
library(readr)
library("modelr")
##################################################################
#loading datasets
GSS <- read_csv("D:/stats/Stat_3120/Hw/Hw 3/GSS-18.csv")
heightfoot <- read_csv("D:/stats/Stat_3120/Hw/Hw 3/heightfoot.csv")
poverty <- read_csv("D:/stats/Stat_3120/Hw/Hw 3/poverty.csv")
UCDavis1 <- read_csv("D:/stats/Stat_3120/Hw/Hw 3/UCDavis1.csv")
UCDavis2 <- read_csv("D:/stats/Stat_3120/Hw/Hw 3/UCDavis2.csv")
#################################################################3
#Q1
ucd1 = subset(UCDavis1, UCDavis1$TVHrs <= 90)
summary(lm(ucd1$GPA ~ ucd1$TVHrs))

plot(ucd1$TVHrs, ucd1$GPA, xlab = "Hours watched per week",
      ylab = "GPA", main = "TvHours vs Gpa")
#Q2
plot(heightfoot$footlength, heightfoot$height, xlab ='footlength',
     ylab = 'height', main = 'footlength vs height')

summary(heightfoot)

hf1 = subset(heightfoot, heightfoot$height <80)

cor(hf1$footlength, hf1$height)
summary(lm(hf1$height ~ hf1$footlength))


hfsum = lm(hf1$height ~ hf1$footlength)

summary(hfsum)

hf1 = add_predictions(hf1, hfsum, var = "pred")

predict_height = data.frame(footlength = 28)
predict(hfsum, newdata = predict_height, interval = "predict", level = 0.9)


hf1$residual = hf1$height - hf1$pred
plot(hf1$footlength, hf1$residual, xlab = 'foot length',
     ylab = 'residual', main = 'footlength vs residual')

#Q3
summary(lm(heightfoot$height ~ heightfoot$footlength))
hffullsum = lm(heightfoot$height ~ heightfoot$footlength)


heightfoot = add_predictions(heightfoot, hffullsum, var = "pred")

predict_height = data.frame(footlength = 28)
predict(hffullsum, newdata = predict_height, interval = "predict", level = 0.9)


plot(hffullsum, which = 4)
cooks_d = cooks.distance(hffullsum)
cooks_d

#Q4
drop <- "Loaction"
poverty_quant = poverty[-c(1)]
cor(poverty_quant)

summary(lm(PovPct ~ Brth15to17 + Brth18to19 + ViolCrime + TeenBrth, data = poverty_quant))

summary(lm(PovPct ~ Brth18to19 + ViolCrime + TeenBrth, data = poverty_quant))
summary(lm(PovPct ~ Brth18to19 + TeenBrth, data = poverty_quant))

summary(lm(PovPct ~ Brth18to19, data = poverty_quant))
summary(lm(PovPct ~ TeenBrth, data = poverty_quant))

#Q5
observed = c(147, 33, 120)
biggie_smalls = 1-(0.55+0.1)
expected = c(0.55, 0.1, biggie_smalls)

test_exp = 300 * expected

chisq.test(x = observed, p = expected, correct = FALSE)

#Q6
gss_subset = subset(GSS, !is.na(GSS$owngun)& !is.na(GSS$polparty) )

addmargins(table(gss_subset$owngun, gss_subset$polparty))

gun_pol = table(gss_subset$owngun, gss_subset$polparty)
chisq.test(x = gun_pol, correct = FALSE)

#Q7
ucd2 = subset(UCDavis2, !is.na(UCDavis2$Hand) & !is.na(UCDavis2$Friends))

hand_friend = table(ucd2$Hand, ucd2$Friends)

chisq.test(x = hand_friend, correct = FALSE)