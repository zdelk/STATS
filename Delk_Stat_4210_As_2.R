start.time <- Sys.time()
child_data <- read.csv("D:/Stats Stuff/Stat_4210/datasets/FC2020v1.csv")
end.time <- Sys.time()
time.taken <- end.time - start.time
print(time.taken)

summary(child_data)

library(statar)

sum_up(child_data)

child_data$new_variable <- as.Date(as.character(child_data$cursetdt), format="%Y-%m-%d") - 
  as.Date(as.character(child_data$latremdt), format="%Y-%m-%d")
child_data$new_variable <- as.numeric(child_data$new_variable)

colnames(child_data)[colnames(child_data) == 'new_variable'] <- 'timeGap'

child_data = child_data[!child_data$timeGap <0,]
sum_up(child_data, timeGap)

tab(child_data, curplset)

options(scipen=99)
child_data$ageatstart <-as.numeric(child_data$ageatstart)
regression_results <- lm(child_data$timeGap ~  child_data$ageatstart)
print(summary(regression_results))
