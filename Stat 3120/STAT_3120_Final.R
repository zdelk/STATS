library(readr)
admissions_data <- read_csv("D:/stats/Stat_3120/Final/admissions_data.csv", 
                            col_types = cols(year = col_character()))

daily_interval_data <- read_csv("D:/stats/Stat_3120/Final/daily_interval_data.csv")

enrollment_data <- read_csv("D:/stats/Stat_3120/Final/enrollment_data.csv")

monthly_energy_data <- read_csv("D:/stats/Stat_3120/Final/monthly_energy_data.csv")


View(daily_interval_data)
View(admissions_data)
View(enrollment_data)
View(monthly_energy_data)

#Problem 1: 95% CI
admissions_data[1:2]
x1 = admissions_data$accepted[7]
x2 = admissions_data$accepted[4]
n1 = admissions_data$applicants[7]
n2 = admissions_data$applicants[4]
prop.test(x = c(x1, x2), n = c(n1, n2), conf.level = 0.95, correct = FALSE)

x1/n1

#Problem 2: Scatterplot, TrendLine, Correlation

plot(monthly_energy_data$avg_temp, monthly_energy_data$daily_kwh)
abline(lm(monthly_energy_data$daily_kwh ~ monthly_energy_data$avg_temp))
cor(monthly_energy_data$avg_temp, monthly_energy_data$daily_kwh)

summary(lm(monthly_energy_data$daily_kwh ~ monthly_energy_data$avg_temp))

#Problem 3: Correlation Matrix, Multiple regression
p3_use = monthly_energy_data[,c(4,6:7)]
cor(p3_use)

model1 = lm(p3_use$daily_kwh ~ p3_use$avg_temp + p3_use$daily_cdd)
summary(model1)
#Problem 4: Backwards elimination procedure
model2 = lm(monthly_energy_data$daily_kwh ~ monthly_energy_data$daily_cdd + monthly_energy_data$daily_hdd + 
               monthly_energy_data$fall_semester + monthly_energy_data$spring_semester +
               monthly_energy_data$covid)
summary(model2)

model3 = lm(monthly_energy_data$daily_kwh ~ monthly_energy_data$daily_hdd + 
              monthly_energy_data$fall_semester + monthly_energy_data$spring_semester +
              monthly_energy_data$covid)
summary(model3)

model4 = lm(monthly_energy_data$daily_kwh ~ monthly_energy_data$daily_hdd + 
              monthly_energy_data$fall_semester +
              monthly_energy_data$covid)
summary(model4)

#Problem 5: only 2019. Grouped boxplots. anova table

p5_use = subset(daily_interval_data, daily_interval_data$year == 2019)

boxplot(p5_use$kwh ~ p5_use$day_of_week, main = 'KWH by Day of the Week',
        xlab = 'KWH',ylab = 'Day of the Week', 
        col= 'lightblue', horizontal = TRUE)

model5 = aov(kwh ~ day_of_week, data = p5_use)
summary(model5)
#Problem 6: only 2021. table. Independence test
p6_use = subset(enrollment_data, enrollment_data$semester == "Fall 2021")

p6_table = table(p6_use$gender, p6_use$level)
chisq.test(x = p6_table, correct = FALSE)

p6_table2 = table(p6_use$level, p6_use$gender)
chisq.test(x = p6_table2, correct = FALSE)
#Problem 7: t-test

pt(3.1389, 58, lower.tail = FALSE)
2 * pt(3.1389, 58, lower.tail = FALSE)

pt(2.254, 58, lower.tail = FALSE)
2 * pt(2.254, 58, lower.tail = FALSE)

pt(1.783, 58, lower.tail = FALSE)
2 * pt(1.783, 58, lower.tail = FALSE)
