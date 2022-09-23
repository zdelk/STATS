#Zachary Delk
#Stat 3120
#Final Project
############################
#Packages install
install.packages("hydroTSM")                       
install.packages("tidyverse")
install.packages("ggplot2")
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

library(readr)
weather <- read_csv("stats_stuff/2908553.csv", 
                  col_types = cols(DATE = col_date(format = "%m/%d/%Y")))
View(weather)
#############################
#build new variables
weather$day_of_week <- weekdays(weather$DATE)
weather$DATE2 = format(weather$DATE, "%m-%d")
weather$year = format(weather$DATE, "%y")
weather$season <- time2season(weather$DATE,out.fmt = "seasons")
weather$year = as.integer(format(weather$year))
weather$year4 = weather$year + 2000
weather$year4[weather$year4 >= 2050] = weather$year4 - 100
weather$rain = "No"
weather$rain[weather$PRCP > 0] = "Yes"
#############################################
#subset data by season
summer = subset(weather, weather$season == 'summer')
fall = subset(weather, weather$season == 'autumm')
winter = subset(weather, weather$season == 'winter')
spring = subset(weather, weather$season == 'spring')
##############################################
#Start analysis
spring_t_max = aggregate(spring$TMAX, list(spring$year4), FUN=mean)
summer_t_max = aggregate(summer$TMAX, list(summer$year4), FUN=mean)

plot(spring_t_max$Group.1, spring_t_max$x, main="Avg Temp in Spring over 40 years", 
     xlab="date", ylab="TAVG", #Add axis labels
     pch=20)

plot(summer_t_max$Group.1, summer_t_max$x, main="Average temp in Summer over 40 years", 
     xlab="date", ylab="TAVG", #Add axis labels
     pch=20)

plot(spring$DATE, spring$TMAX, main="All Temps in Spring", 
     xlab="date", ylab="TAVG", #Add axis labels
     pch=20)
########################################################################################
#test for tavg vs avg(tmax,tmin)



pre_t_avg = subset(weather, !is.na(weather$TAVG))

weather$testavg = 0.5 * weather$TMIN + .5 * weather$TMAX

plot(weather$testavg, weather$TAVG, main="test correlation", 
     xlab="testavg", ylab="TAVG", #Add axis labels
     pch=20)
#########
plot(weather$testavg, weather$TAVG, xlab = "testavg", ylab = "tavg",
     main = "", pch = 21, ylim = c(16,26), xlim = c(55,80), las = 1)

# 03. Add grid
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted") 

# 04. Add a trend line  
abline(lm(weather$TAVG ~ weather$testavg), col = "#9DACBB", lty = 2, lwd = 2)

cor(pre_t_avg$TAVG, pre_t_avg$testavg)
########


plot(weather$DATE, weather$testavg, main="Base R Scatterplot", 
     xlab="date", ylab="TAVG", #Add axis labels
     pch=20)

########################################################################################
december = subset(weather, weather$DATE2 >= '12-01')
december$dayint = format(december$DATE, "%d")
december$dayint = as.integer(format(december$dayint, "%d"))
decmeber$stuff = mod(december$year, 5)
december$year_string = as.character(december$year4)

december5 = subset(december, december$year%%5 == 0)

plot(december$dayint, december$testavg, xlab = "Day", ylab = "Avg Temp",
     main = "", pch = 21, ylim = c(0,80))

ggplot(december5, aes(x = dayint, y = testavg, color = year_string)) +
  geom_point()


mod(5,5)

summary(december)
#######################################################
weather$ten_avg_max = weather$TMAX

weather$ten_avg_max = rollmean(weather$TMAX, 5)

plot(weather$DATE, weather$max_avg, main="Base R Scatterplot", 
     xlab="date", ylab="TAVG", #Add axis labels
     pch=20)

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

plot(weather$DATE, weather$runavg, main="Base R Scatterplot", 
     xlab="date", ylab="runavg", #Add axis labels
     pch=20)

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted") 

# 04. Add a trend line  
abline(lm(weather$runavg ~ weather$DATE), col = "blue", lty = 2, lwd = 2)

lm(weather$runavg ~ weather$DATE)

table(weather$day_of_week, weather$rain, type = "l", axes = FALSE, bty = "n")

################################################################
###########Correlation for Avg Max temp and year################
###########Along with Co2 by year###############################
################################################################
#Use this

year_avg = aggregate(weather$runavg, list(weather$year4), FUN = mean)

plot(year_avg$Group.1, year_avg$x ,main="Yearly average", 
     xlab="year", ylab="Avg Max Temp", #Add axis labels
     pch=20, col= "red")

abline(lm(year_avg$x ~ year_avg$Group.1), col = "red", lty = 2, lwd = 2)
par(new = TRUE)

plot(co2$year, co2$mean, ylab= "", xlab = "", axes = FALSE, bty = "n", col = 'blue')

grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted") 
axis(side=4, at = pretty(range(co2$mean)))
mtext("Co2 Level", side=4, line=3)

abline(lm(year_avg$x ~ year_avg$Group.1), col = "red", lty = 2, lwd = 2)

lm(year_avg$x ~ year_avg$Group.1)
################################################################
###########CI for average max temp on Birthday #################
################################################################


Bday = subset(weather, weather$DATE2 == '03-02')

table(Bday$rain)
t.test(Bday$runavg, conf.level = 0.90)

####################################################################
############Does it rain more in spring?############################
####################################################################

szn_rain = addmargins(table(weather$season , weather$rain))

prop.test(x = 1188, n = 4847,p = .25,  alternative = "greater", correct = FALSE)

#####################################################################





set.seed(101)
x <- 1:10
y <- rnorm(10)
## second data set on a very different scale
z <- runif(10, min=1000, max=10000) 
par(mar = c(5, 4, 4, 4) + 0.3)  # Leave space for z axis
plot(x, y) # first plot
par(new = TRUE)
plot(x, z, type = "l", axes = FALSE, bty = "n", xlab = "", ylab = "")
axis(side=4, at = pretty(range(z)))
mtext("z", side=4, line=3)