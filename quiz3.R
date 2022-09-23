library(readr)
library(dplyr)

bodyfat_train <- read_csv("D:/stats/Stat_4030/quiz 3/bodyfat_train.csv")
View(bodyfat_train)

bodyfat_test <- read_csv("D:/stats/Stat_4030/quiz 3/bodyfat_test.csv")
View(bodyfat_test)

options(scipen = 99) # Prevents scientific notation
install.packages("glmnet")


reg_model <- lm(BodyFat ~ ., data=bodyfat_train)

predicted <- predict(reg_model, newdata = bodyfat_test)

bodyfat_test$predicted <- predicted

plot(bodyfat_test$predicted, bodyfat_test$BodyFat)

mean((bodyfat_test$BodyFat - bodyfat_test$predicted)^2)
################################################
#neck

neck_model <- lm(BodyFat ~ Age + Weight+ Height+ Neck, data=bodyfat_train)

predicted_neck <- predict(neck_model, newdata = bodyfat_test)

mean((bodyfat_test$BodyFat - predicted_neck)^2)

################################################
#chest

chest_model <- lm(BodyFat ~ Age + Weight+ Height+ Chest, data=bodyfat_train)

predicted_chest <- predict(chest_model, newdata = bodyfat_test)

mean((bodyfat_test$BodyFat - predicted_chest)^2)

################################################
#hip

hip_model <- lm(BodyFat ~ Age + Weight+ Height+ Hip, data=bodyfat_train)

predicted_hip <- predict(hip_model, newdata = bodyfat_test)

mean((bodyfat_test$BodyFat - predicted_hip)^2)
#################################################
Head_Neck_Cancer_Clinics_test <- read_csv("D:/stats/Stat_4030/quiz 3/Head_Neck_Cancer_Clinics_test.csv")
View(Head_Neck_Cancer_Clinics_test)

Head_Neck_Cancer_Clinics_train <- read_csv("D:/stats/Stat_4030/quiz 3/Head_Neck_Cancer_Clinics_train.csv")
View(Head_Neck_Cancer_Clinics_train)


clinic_test <- Head_Neck_Cancer_Clinics_train[complete.cases(Head_Neck_Cancer_Clinics_train),]

clinic_test$status_binary = 0
clinic_test$status_binary[clinic_test$Status == 'Alive'] = 1

clinic_model = glm(status_binary ~ Age_at_diagnosis + Smoking_Hx + Drinking_hx+ Stage+ 
                    Time_interval_diagnosis_to_treatment + Sex, data= clinic_test, family = "binomial")


Head_Neck_Cancer_Clinics_test$predicted <- predict(clinic_model, newdata=Head_Neck_Cancer_Clinics_test, 
                                 type = "response")


Head_Neck_Cancer_Clinics_test$predicted_alive <- ifelse(Head_Neck_Cancer_Clinics_test$predicted > 0.5, 1, 0)
# Total number of patients who are predicted to get stroke
sum(Head_Neck_Cancer_Clinics_test$predicted_alive)


clinic_test1 <- Head_Neck_Cancer_Clinics_test[complete.cases(Head_Neck_Cancer_Clinics_test),]

clinic_test1$status_binary = 0
clinic_test1$status_binary[clinic_test1$Status == 'Alive'] = 1


install.packages("caret")

library(caret)
confusionMatrix(factor(clinic_test1$predicted_alive), 
                factor(clinic_test1$status_binary),
                dnn = c("Prediction", "Reference"),
                positive = "1")

# Build a function that decides whether a prediction is correct or wrong
match <- function (actual, predicted) {
  if (actual==predicted) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}





