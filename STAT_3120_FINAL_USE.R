#Zachary Delk
#Stat 3120
#Final Project
############################
#Packages install
#install.packages("hydroTSM")                       
#install.packages("tidyverse")
#install.packages("ggplot2")
############################
#Run Libraries
library("hydroTSM")
library("tidyverse")
library("ggplot2")
library(readr)
#############################
#Pull in data set and view
weather <- read_csv("D:/stats/Stat_3120/final project/2908553.csv")
View(weather)
summary(weather)
library(readr)
co2 <- read_csv("co2.csv")
View(co2)
#############################
#build new variables
weather$day_of_week <- weekdays(weather$DATE) #creates day of week var from DATE
weather$DATE2 = format(weather$DATE, "%m-%d") #creates Month-Day date from DATE
weather$season <- time2season(weather$DATE,out.fmt = "seasons") #creates seasons from dates
#dealing with two digit year
weather$year = format(weather$DATE, "%y") #isolates year in new var
weather$year = as.integer(format(weather$year)) # converts year var to int
weather$year4 = weather$year + 2000
weather$year4[weather$year4 >= 2050] = weather$year4 - 100
#creates var for if it rained and populates it
weather$rain = "No" 
weather$rain[weather$PRCP > 0] = "Yes"
#subset data by season
winter = subset(weather, weather$season == 'winter')
##############################################
#Creating a running average
rows = nrow(weather)
weather$runavg = 0
weather$TMAX[1]

for (i in 1:rows)
{
  if (i <= 4){
    weather$runavg[i] = weather$TMAX[i]
  }else if (i >= (rows-4)){
    weather$runavg[i] = weather$TMAX[i]
  }else{
    ping = (1/7) * ( weather$TMAX[i-3] + weather$TMAX[i-2] + weather$TMAX[i-1] 
                     + weather$TMAX[i] + weather$TMAX[i+1]
                     + weather$TMAX[i+2] + weather$TMAX[i+3])
    weather$runavg[i] = ping
  }
}
################################################################
###########Correlation for Avg Max temp and year################
###########Along with Co2 by year###############################
year_avg = aggregate(winter$runavg, list(winter$year4), FUN = mean)

plot(winter_avg$Group.1, winter_avg$x ,
     main="Figure 1: Average Yearly Winter Max Temperature and Co2 Levels by Year", 
     xlab="Year", ylab="Avg Winter Max Temp", #Add axis labels
     pch=20, col= "deeppink")

legend("bottomright", legend=c("Temperature", "Co2 Level"),
       col=c("deeppink", "blue4"), lty= 1:2, cex = 0.8)

abline(lm(winter_avg$x ~ winter_avg$Group.1), col = "deeppink", lty = 2, lwd = 2)
par(new = TRUE)

plot(co2$year, co2$mean, ylab= "", xlab = "", axes = FALSE, bty = "n", col = 'blue4')

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted") 

axis(side=4, at = pretty(range(co2$mean)))

mtext("Co2 Level", side=4, line=3)

abline(lm(winter_avg$x ~ winter_avg$Group.1), col = "pink", lty = 2, lwd = 2)

bozo = summary(lm(winter_avg$Group.1 ~ winter_avg$x))
lm(winter_avg$x ~ winter_avg$Group.1)
bozo
cor(winter_avg$x, winter_avg$Group.1)

#plot for temp vs co2 level  #USE
plot(winter_avg$co2, winter_avg$x, 
     main="Figure 3: Average Yearly Winter Max Temperature by Co2 Levels", 
     xlab="Co2 Levels", ylab="Avg Max Temp", #Add axis labels
     pch=20, col= "green4", xlim = c(340,420))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
abline(lm(winter_avg$x ~ winter_avg$co2), col = "darkorange3", lty = 2, lwd = 2)

t_co2 = lm(winter_avg$x ~ winter_avg$co2)
summary(t_co2)
#######################################################################
############Does it rain more on weekends?############################
################BOXPLOT FOR EACH DAY
################T TEST FOR WEEKEND VS WEEK DAY
weather$weekend = 0
weather$weekend[weather$day_of_week == 'Saturday' | weather$day_of_week == 'Sunday'] = 1
counts = table(weather$rain, weather$day_of_week)
rain_data = subset(weather, weather$rain == 'Yes')
view(rain_data)
#Organize week
ORDDW <- ordered(rain_data$day_of_week,c("Sunday","Monday","Tuesday", "Wednesday",
                                         "Thursday", "Friday", "Saturday"))
#boxplot for each day
boxplot(rain_data$PRCP ~ ORDDW, main = 'Figure 4: Rain by Day of the Week',
        xlab = 'Average Amount of Rain (inches)',ylab = 'Day of the Week', 
        col= 'lightblue', horizontal = TRUE, outline = FALSE)


rain_avg = aggregate(rain_data$PRCP, list(rain_data$weekend), FUN = mean)
t.test(rain_data$PRCP ~ rain_data$weekend, conf.level = 0.9)


prop.test(x = 628 + 682, n = 4847, p = 0.50, alternative = "greater", correct = FALSE)

prop.test(x = c(628 + 682, 4847 - 628 - 682), n = c(2191+2191, 15341 - 2191 - 2191), alternative = "greater", correct = FALSE)
prop.test(x = c(628 + 682, 4847 - 628 - 682), n = c(2191+2191, 15341 - 2191 - 2191), conf.level = 0.95, correct = FALSE)

prop.test(x = c(1310, 3537), n = c(4382, 10959), alternative = "greater", conf.level = 0.90, correct = FALSE)

prop.test(x = c(1310, 3537), n = c(4382, 10959), conf.level = 0.90, correct = FALSE)


addmargins(table(weather$rain, weather$day_of_week))

addmargins(table(weather$weekend, weather$rain))
########################################################
########test plot for difference vs co2 levle
wrows = nrow(winter_avg)
winter_avg$pos = 0
winter_avg$neg = 0

for (i in 1:wrows)
{
  if (winter_avg$lmao[i] > 0){
    winter_avg$pos[i] = winter_avg$lmao[i]
  }else if (winter_avg$lmao[i] < 0){
    winter_avg$neg[i] = winter_avg$lmao[i]
  }else{
    winter_avg$neg[i] = 0
    winter_avg$pos[i] = 0
  }
}

ggplot(winter_avg)  + 
  geom_bar(aes(Group.1, neg),stat="identity", fill="blue",colour="#006000")+
  geom_bar(aes(Group.1, pos),stat="identity", fill="red",colour="#006000")+
  labs(title= "Figure 2: Temperature Difference from Average Winter Temps by Year",x="Year",y="Temperature Diff")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(sec.axis=sec_axis(~.))
