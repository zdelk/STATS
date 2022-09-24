#q2
!(5 %% 2 == TRUE)

library(readr)
library(haven)
library(readxl)
iris_csv <- read_csv("D:/stats/Stat_4030/QUIZ 1/quiz_1_iris/Iris.csv")
iris_excel <- read_excel("D:/stats/Stat_4030/QUIZ 1/quiz_1_iris/Iris.xlsx")
iris_sas <- read_sas("D:/stats/Stat_4030/QUIZ 1/quiz_1_iris/iris.sas7bdat")

#Q3
avg_csv = mean(iris_csv$PetalWidthCm)
avg_excel = mean(iris_excel$PetalWidthCm)
avg_sas = mean(iris_sas$Petal_Width)

(avg_csv + avg_excel + avg_sas)/3

#Q4

clinical_data <- read_csv("D:/stats/Stat_4030/QUIZ 1/quiz_1_dental_clinic/clinical_data.csv")
charge_data_Jun04 <- read_csv("D:/stats/Stat_4030/QUIZ 1/quiz_1_dental_clinic/charge_data_Jun04.csv")
charge_data_Jun03 <- read_csv("D:/stats/Stat_4030/QUIZ 1/quiz_1_dental_clinic/charge_data_Jun03.csv")

concat = rbind(charge_data_Jun03, charge_data_Jun04)

charge_concat_sum <- aggregate(Total_Charge ~ Patient_ID, 
                               concat, sum)

final_clinical_charge_data <- merge(x = charge_concat_sum, 
                                    y = clinical_data, 
                                    by = "Patient_ID", 
                                    all.x = TRUE)

mean(final_clinical_charge_data$age)

#Q5
waiting <- read_csv("D:/stats/Stat_4030/QUIZ 1/quiz_1_restaurant_waiting.csv")

bar_customer = 0


bar_customer = 0
customer_number = 0
while (bar_customer < 6)  {
  customer_number = customer_number + 1
  number_of_party = waiting[customer_number, 3]
  bar_seat = waiting[customer_number, 4]
  if ((number_of_party < 3) & (bar_seat == 'Yes')) {
    bar_customer = bar_customer + number_of_party
    print(waiting[customer_number, 2])
    print(bar_customer)
  }
}

#Q6
nov_files = list.files("D:/stats/Stat_4030/QUIZ 1/Restaurant_Vip",
                           pattern = "2021_11.*\\.csv",
                           full.names = TRUE)
nov_files

nov_one_year = data.frame()

for (daily_data_path in nov_files) {
  all_customer_daily <- read.csv(daily_data_path)
  nov_one_year <- rbind(nov_one_year, all_customer_daily)
}


justin = subset(nov_one_year, nov_one_year$name == 'John Smith')

mean(justin$amount)

#Q7

aggregated_dataframe <- aggregate(amount ~ cust_id + name, nov_one_year, sum)

aggregated_dataframe[which.max(aggregated_dataframe$amount),]