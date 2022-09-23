library(readr)
titanic <- read_csv("D:/stats/Stat_4030/Midterm/titanic.csv")
View(titanic)

titanic = titanic[complete.cases(titanic)]

lived = subset(titanic, titanic$Survived == 1)
lived <- lived[complete.cases(lived),]
avg_age = mean(lived$Age)



died = subset(titanic, titanic$Survived == 0)
died <- died[complete.cases(died),]


summary(lived)


t.test(lived$Age, died$Age, 
       alternative = "two.sided", var.equal = FALSE)

library(haven)

jun03 <- read_csv("D:/stats/Stat_4030/Midterm/midterm_dental_clinic/charge_data_Jun03.csv")
View(charge_data_Jun03)

jun04 <- read_csv("D:/stats/Stat_4030/Midterm/midterm_dental_clinic/charge_data_Jun04.csv")
View(charge_data_Jun04)

clinical_data <- read_csv("D:/stats/Stat_4030/Midterm/midterm_dental_clinic/clinical_data.csv")
View(clinical_data)

concat <- rbind(jun03, jun04)

length(!unique(concat$Patient_ID))

inner_merged <- merge(x = jun03, y = jun04, by = "Patient_ID", all.x = TRUE)
length(inner_merged)


library(readr)
who_data <- read_csv("D:/stats/Stat_4030/Midterm/WHO_Life_Expectancy_Data_2000.csv")
View(who_data)

who_data <- who_data[ , -which(colnames(who_data)=="Country")]

#View(who_data)
lm_results <- lm(who_data$`Life expectancy` ~ who_data$`infant deaths` + who_data$GDP + who_data$`HIV/AIDS`+
                   who_data$Schooling, data=who_data)
summary(lm_results)


Stroke <- read_csv("D:/stats/Stat_4030/Midterm/Framingham_Stroke_Data.csv")
View(Stroke)

Stroke = Stroke[complete.cases(Stroke), ]


stroke_need_screening <- Stroke[(Stroke$AGE>=55) & (Stroke$AGE <= 74) & (Stroke$CIGPDAY>=30), ]

