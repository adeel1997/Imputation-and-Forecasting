# Installing and Importing the relevant package
library(dplyr)
library(zoo)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(timeDate)
# These last two libraries are for accuracy measurement
library(Metrics)
library(HydroGOF)
# Setting the working directory 
setwd("E:/Forecasting Paper Analysis")
# Reading the data and putting the None value as NA
anand <- read.csv("CPCB Data/2016_20_Anand_Vihar.csv",na.strings = "None")
# Converting date into proper format
anand$date <- as.POSIXct(anand$date,format="%d-%m-%Y %H:%M")
# Adding different datetime variables in the data 
anand$month  <- month(anand$date)
anand$year  <- year(anand$date)
anand$hour  <- hour(anand$date)
anand$day  <- day(anand$date)

# Normazition fuction 
normalize <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}

###################### HOURLY SEASONALITY ############################

# Group by year, month, day, hour and calculating the PM10 value for the hour
hourly_s <- anand %>% group_by(year,month,day,hour) %>% summarize(average=mean(PM10,na.rm = TRUE))%>% spread(year, average)
head(hourly_s)
# Normalizing the PM10 values for all the year
hourly_s[4:8] <- apply(hourly_s[4:8], 2, normalize)
# Cacluating the mean hourly index over the years
hourly_s$hourlyindex <- rowMeans(hourly_s[,4:8],na.rm = TRUE)
# Replacing the null value for an hour in which last 5 year value isn't there through locf
hourly_s$hourlyindex <- na.locf(hourly_s$hourlyindex)
# Creating a variable which later can be used for joining
hourly_s$month_day_hour <- paste(hourly_s$month,"_",hourly_s$day,"_",hourly_s$hour)
# Subseting the hourly index and the common variable
hourly_s <- subset(hourly_s,select = c(hourlyindex,month_day_hour))
plot(hourly_s$hourlyindex,type="l")
nrow(hourly_s)

############################ Daily Seasonality ##########################

#Group by year, month and day then calculating the pollutant average and finally spreading it over the year
daily_s <- anand %>% group_by(year,month,day) %>% summarize(average=mean(PM10,na.rm = TRUE))%>% spread(year, average)
head(daily_s)
# Normalizing the value 
daily_s[3:7] <- apply(daily_s[3:7], 2, normalize)
# Cacluating the mean daily index over the years
daily_s$dailyindex <- rowMeans(daily_s[,3:7],na.rm = TRUE)
# Creating the common variable which can later be used for merging
daily_s$month_day <- paste(daily_s$month,"_",daily_s$day)
# Now subsetting the index and common variable
daily_s <- subset(daily_s,select = c(dailyindex,month_day))
plot(daily_s$dailyindex,type="l")

############################ Monthly seasonality#####################

#Group by year, month then calculating the pollutant average and finally spreading it over the year
monthly_s <- anand %>% group_by(year,month) %>% summarize(average=mean(PM10,na.rm = TRUE))%>% spread(year, average)
# Normalizing the value 
monthly_s[2:6] <- apply(monthly_s[2:6], 2, normalize)
head(monthly_s)
# Cacluating the mean hourly index over the years
monthly_s$monthlyindex <- rowMeans(monthly_s[,2:6],na.rm = TRUE)
monthly_s <- subset(monthly_s,select = c(monthlyindex,month))
plot(monthly_s$monthlyindex,type="l")

##################################### Adding Common variable through which merge can be done #####################

# Adding the common variable in the main data frame
anand$month_day <- paste(anand$month,"_",anand$day)
anand$month_day_hour <- paste(anand$month,"_",anand$day,"_",anand$hour)

## Merging the data frames, starting with hourly index
hourly_merge <- merge(x = anand, y = hourly_s, by = "month_day_hour", all = TRUE)
## Merging the daily index
day_hourly_merge <- merge(x = hourly_merge, y = daily_s, by = "month_day", all = TRUE)
## Finally merge the monthly index
month_day_hourly_merge <- merge(x = day_hourly_merge, y = monthly_s, by = "month", all = TRUE)
# Seeing the final merge file
head(month_day_hourly_merge)
# Checking whether the number of row matches with raw data
nrow(month_day_hourly_merge) == nrow(anand)

# Naming the converted data into a new variable called new_data
new_data <- month_day_hourly_merge
# Sort the data based in the asscending order by date
new_data <- arrange(new_data,date)
head(new_data)
# Creating factor variables of Weekday and Weekend also the Year to capture the variablity
new_data$Week <- as.factor(as.numeric(isWeekday(new_data$date, wday=1:5)))
new_data$Year <- as.factor(year(new_data$date))
str(new_data)

# Will fit a simple linear model here 

reg1 <- lm(formula = PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, data = new_data)
reg1
summary(reg1)

# Creating a separate dataframe for missing and not missing values for the Pollutant
new_data_non_missing <- new_data[!is.na(new_data$PM10),]
new_data_missing <- new_data[is.na(new_data$PM10),]
head(new_data_missing)

# Subseting the dataframe for prediction variable
data_to_predict <- subset(new_data_missing, select = c(hourlyindex,dailyindex, monthlyindex, Week,Year))
# Fitting the regression model on the missing value in future you would like to predict different models
missing_prediction <- predict(reg1, newdata=new_data_missing)

# Substituting the missing value back in the data frame 
new_data_missing$PM10 <- missing_prediction
# Combining the rows to have a combined data frame
new_data_filled <- rbind(new_data_missing,new_data_non_missing)
# Filtering the dataframe through the date
new_data_filled <- arrange(new_data_filled,date)
# Checking the structure of the dataframe
str(new_data_filled)

#Plotting the Imputation values
ggplot() + geom_line(data=new_data_missing,aes(x=date,y=PM10,color="Missing Imputation")) + 
  geom_line(data=new_data_non_missing,aes(x=date,y=PM10,color="Original Data"))+ labs(color="Linear Regression Imputation")


#write.csv(new_data_filled,"RandomForestImputation.csv")

# Missing Prediction variable to predict the data on training set to see the accuracy 
missing_prediction2 <- predict(reg1)

# Different accuracy methods
R <- rPearson(new_data_non_missing$PM10 ,missing_prediction2)
R2 <- rsq(new_data_non_missing$PM10 ,missing_prediction2)
RMSE <- rmse(new_data_non_missing$PM10 ,missing_prediction2)
NRMSE <- nrmse(new_data_non_missing$PM10 ,missing_prediction2)
Bias <- bias(new_data_non_missing$PM10 ,missing_prediction2)
IOA <- md(new_data_non_missing$PM10 ,missing_prediction2)
MAPE <- mape(new_data_non_missing$PM10 ,missing_prediction2)

# Finally create a data frame for different accuracy methods
RESULTS <- data.frame(R,R2,RMSE,NRMSE,Bias,IOA,MAPE)
RESULTS
