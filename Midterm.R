#Zachary Delk
#Stat 3120
#Midterm

#install.packages("fastR2")
library(fastR2)
#********************************************
#Q1) Create a 90% confidence interval for the proportion 
#of charging events that begin in the afternoon.

summary(charging_duration_data)
table(charging_duration_data$period)

cdc = length(charging_duration_data$period)
afternoon_charging = charging_duration_data$period == 'Afternoon' = TRUE
length(afternoon_charging)

wald.ci(x = 527, cdc, conf.level = 0.90)

#*********************************************
#Q2) Test the hypothesis that the proportion of 
#charging events that exceeds two hours is greater than 0.40. 

over_two_hours = charging_duration_data$charge_duration < 2

table1 = table(over_two_hours)
proportions(table1)

addmargins(table1)
n = length(over_two_hours)
p_hat = 2399/n
p_0 = 0.40
se = sqrt(p_0*(1-p_0)/n)
test_stat = (p_hat - p_0) / se

prop.test(table1, alternative = "greater", p = 0.40, correct = FALSE)

#**********************************************
#Q3) 99% CI for differences of afternoon charging 
#from weekday to weekend

charging_duration_data$weekend = 0
charging_duration_data$weekend[charging_duration_data$day_of_week == "01 Sunday"] = 1
charging_duration_data$weekend[charging_duration_data$day_of_week == "07 Saturday"] = 1

charging_duration_data$afternoon_charging = charging_duration_data$period == 'Afternoon'

addmargins(table(charging_duration_data$weekend, charging_duration_data$afternoon_charging))
proportions(table(charging_duration_data$weekend, charging_duration_data$afternoon_charging),1)

prop.test(x = c(1198, 378), n = c(3129, 890), alternative = "two.sided", conf.level = 0.99,
          correct = FALSE)

#*************************************************
#Q4)Create an 85% confidence interval for the average energy 
#usage (kWh) during hour 16.

summary(hourly_interval_data)

hourly_interval_data$hour_sixteen = hourly_interval_data$hour == 16

data_sixteen = subset(hourly_interval_data, hourly_interval_data$hour_sixteen == TRUE)

t.test(data_sixteen$kwh, conf.level = 0.85)

#*************************************************
#Q5) Suppose electricity consumption peaks during hours 13-17 
#in Boulder. The city council wants to know if the average energy 
#usage at this charging station differs from 2 kWh during the
#peak period. Run a hypothesis test to investigate.

hourly_interval_data$XIItoXVII = 0
hourly_interval_data$XIItoXVII[hourly_interval_data$hour >= 13 & hourly_interval_data$hour <= 17] = 1

something = subset(hourly_interval_data, hourly_interval_data$XIItoXVII == TRUE)

t.test(something$kwh, mu = 2, alternative = "two.sided")

#*************************************************
#Q6)Calculate the average daily kWh by year. Using 2019 as a 
#proxy for a "pre-COVID" year and 2020 as a proxy for a "COVID" 
#year, run a paired t-test to determine if daily energy consumption 
#under COVID was less than daily energy consumption pre-COVID.
#Assume x ??_D = 14.25, s_D = 25.84, and n = 365. 
#(Calculate the test statistic and p-value.)

x_d = 14.25
s_d = 25.84
n = 365
d_f = n - 1
preCovid = subset(daily_interval_data, daily_interval_data$year == 2019)
covid = subset(daily_interval_data, daily_interval_data$year == 2020)

mu_one = mean(preCovid$kwh)
mu_two = mean(covid$kwh)

mu = mu_one - mu_two

se = s_d / sqrt(n)

ts = (x_d - mu)/ se

ts
pt(ts, d_f, lower.tail = TRUE)

#**************************************************
#Q7) Is there a difference in daily EV charging loads when it 
#snows relative to days without snow? Create a 92% confidence 
#interval for the difference between the average daily kWh 
#consumption on days with and without snow.


daily_interval_data$did_snow = daily_interval_data$snow != 0

t.test(daily_interval_data$kwh ~ daily_interval_data$did_snow, conf.level = 0.92)

#***************************************************
#Q8)Are weekday charging loads greater than weekend 
#charging loads? Run a hypothesis test to investigate. 
#(Be mindful of how you set up the alternative hypothesis if you use the t.test function.)

daily_interval_data$weekend = 0
daily_interval_data$weekend[daily_interval_data$day_of_week == "01 Sunday"] = 1
daily_interval_data$weekend[daily_interval_data$day_of_week == "07 Saturday"] = 1


t.test(daily_interval_data$kwh ~ daily_interval_data$weekend, alternative = "greater")

#*****************************************************
#Q9) Find the p-values associated with the following tests: 
#(1) z = -1.323, left-tailed test, (2) z = 0.677, right-tailed test, 
#(3) z = 1.817, two-tailed test, and (4) z = -0.445, left-tailed test.

pnorm(-1.323, lower.tail = TRUE)
pnorm(0.677, lower.tail = FALSE)
2 * pnorm(1.817, lower.tail = FALSE)
pnorm(-0.445, lower.tail = TRUE)





