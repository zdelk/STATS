####################################################################
#Zahary Delk
#Stat 4030
#Quiz 2
###################################################################
#Setting working directory
setwd("D:/stats/Stat_4030")

options(scipen = 99)
#Installing and loading library
install.packages("glmnet")
library(glmnet)

#Importing datasets
wine_data_train = read.csv("wine_quality_train.csv")
wine_data_test = read.csv("wine_quality_test.csv")
wine_data_full = read.csv("wine_quality_full.csv")

#Running regression model
reg_model = lm(quality ~ citric.acid + residual.sugar + alcohol,
               data = wine_data_train)

summary(reg_model)

#Using regression model to predict values
predicted = predict(reg_model, newdata = wine_data_test)

#Putting predictions into the dataset
wine_data_test$predicted = predicted

#Calculating the mean square of the differences between actual and predicted values
mean((wine_data_test$quality - predicted)^2)


##################################################################################
#Using wine_data_full
#Creating model matrix with wine_data_full dataset
x = model.matrix(quality ~ ., data = wine_data_full)

#removing last column of the dataset
x = x[,-1]


glmnet1 = cv.glmnet(x=x, y=wine_data_full$quality, type.measure='mse',
                    nfolds = 5, alpha=1)

lasso_coefficients = coef(glmnet1, s='lambda.min', exact =TRUE)
lasso_coefficients

