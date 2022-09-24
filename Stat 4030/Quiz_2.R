setwd("D:/stats/Stat_4030")

options(scipen = 99)
install.packages("glmnet")
library(glmnet)

wine_data_train = read.csv("wine_quality_train.csv")
wine_data_test = read.csv("wine_quality_test.csv")

reg_model = lm(quality ~ citric.acid + residual.sugar + alcohol,
               data = wine_data_train)

summary(reg_model)

predicted = predict(reg_model, newdata = wine_data_test)


wine_data_test$predicted = predicted

mean((wine_data_test$quality - predicted)^2)

wine_data_full = read.csv("wine_quality_full.csv")

x = model.matrix(quality ~ ., data = wine_data_full)

x = x[,-1]

glmnet1 = cv.glmnet(x=x, y=wine_data_full$quality, type.measure='mse',
                    nfolds = 5, alpha=1)

lasso_coefficients = coef(glmnet1, s='lambda.min', exact =TRUE)
lasso_coefficients

