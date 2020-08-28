# Loading relavent packages
library(xts)
library(caret)
library(dplyr)
library(zoo)
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(timeDate)
library(Metrics)
library(hydroGOF)
library(imputeTS)

# Setting the Working directory
setwd("E:/Forecasting Paper Analysis")
# Reading the data and putting the None value as NA
anand <- read.csv("CPCB Data/2016_20_Anand_Vihar.csv",na.strings = "None")
# Converting date into proper format
anand$date <- as.POSIXct(anand$date,format="%d-%m-%Y %H:%M")
anand$month  <- month(anand$date)
anand$year  <- year(anand$date)
anand$day  <- day(anand$date)

# Normazition fuction 
normalize <- function(x){(x-mean(x, na.rm = T))/sd(x, na.rm = T)}
head(anand)
# We will convert the data into time series 
ts <- as.xts(anand,order.by = anand$date)
# We will use apply.daily to convert the hourly time series to daily time series
ts1 <- apply.daily(ts,mean)
# Checking the total missing values
sum(is.na(ts1$PM2.5))
# Converting the time series back to data frame
ts2 <- data.frame(Y=as.matrix(ts1), date=time(ts1))
head(ts2)

# Now we calculate the daily and monthly sesonality
daily_s_pm2 <- ts2 %>% group_by(Y.year,Y.month,Y.day) %>% summarize(average=mean(Y.PM2.5,na.rm = TRUE))%>% spread(Y.year, average)
head(daily_s_pm2)
# Normalizing the value 
daily_s_pm2[3:7] <- apply(daily_s_pm2[3:7], 2, normalize)
# Cacluating the mean daily index over the years
daily_s_pm2$dailyindex <- rowMeans(daily_s_pm2[,3:7],na.rm = TRUE)
daily_s_pm2$dailyindex <- na.locf(daily_s_pm2$dailyindex)
# Creating the common variable which can later be used for merging
daily_s_pm2$month_day <- paste(daily_s_pm2$Y.month,"_",daily_s_pm2$Y.day)
# Now subsetting the index and common variable
daily_s_pm2 <- subset(daily_s_pm2,select = c(dailyindex,month_day))
plot(daily_s_pm2$dailyindex,type="l")

#Group by year, month then calculating the pollutant average and finally spreading it over the year
monthly_s_pm2 <- ts2 %>% group_by(Y.year,Y.month) %>% summarize(average=mean(Y.PM2.5,na.rm = TRUE))%>% spread(Y.year, average)
# Normalizing the value 
monthly_s_pm2[2:6] <- apply(monthly_s_pm2[2:6], 2, normalize)
head(monthly_s_pm2)
# Cacluating the mean hourly index over the years
monthly_s_pm2$monthlyindex <- rowMeans(monthly_s_pm2[,2:6],na.rm = TRUE)
monthly_s_pm2 <- subset(monthly_s_pm2,select = c(monthlyindex,Y.month))
plot(monthly_s_pm2$monthlyindex,type="l")

# Adding the common variable in the main data frame
ts2$month_day <- paste(ts2$Y.month,"_",ts2$Y.day)

## Merging the daily index
day_merge_pm2 <- merge(x = ts2, y = daily_s_pm2, by = "month_day", all = TRUE)
## Finally merge the monthly index
month_day_merge_pm2 <- merge(x = day_merge_pm2, y = monthly_s_pm2, by = "Y.month", all = TRUE)
# Seeing the final merge file
head(month_day_merge_pm2)

# Checking whether the number of row matches with raw data
nrow(month_day_merge_pm2) == nrow(ts1)

# Naming the converted data into a new variable called new_data_pm2
new_data_pm2 <- month_day_merge_pm2
# Sort the data based in the asscending order by date
new_data_pm2 <- arrange(new_data_pm2,date)
head(new_data_pm2)
# We would like to have Year as a factor variable in the data 
new_data_pm2$Year <- as.factor(year(new_data_pm2$date))


# Creating a separate dataframe for missing and not missing values for the Pollutant
new_data_pm2_non_missing <- new_data_pm2[!is.na(new_data_pm2$Y.PM2.5),]
new_data_pm2_missing <- new_data_pm2[is.na(new_data_pm2$Y.PM2.5),]
head(new_data_pm2_missing)

# Defining train control for 10 fold Crossvalidation 
tc <- trainControl(method="cv", number = 10)

# Random Forest Model
rf_cv_pm2_d <- train(Y.PM2.5 ~ dailyindex+monthlyindex+Year, method="rf",
                   data = new_data_pm2_non_missing,trControl=tc)
# Linear Model
lm_cv_pm2_d <- train(Y.PM2.5 ~ dailyindex+monthlyindex+Year, method="lm",
                   data = new_data_pm2_non_missing,trControl=tc)
lm_cv_pm2_d

# Lasso Model
lasso_cv_pm2_d <- train(Y.PM2.5 ~ dailyindex+monthlyindex+Year, method="glmnet",data = new_data_pm2_non_missing,trControl=tc,
                      tuneGrid=expand.grid(alpha=1,lambda=0))

tg_gbm_d <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
                      interaction.depth = c(1, 3, 7, 10),
                      n.minobsinnode = c(2, 5, 10),
                      n.trees = c(100, 300, 500))
# Gradient Boosting method
gbm_cv_pm2_d <- caret::train(Y.PM2.5 ~ dailyindex+monthlyindex+Year, method="gbm",tuneGrid=tg_gbm_d,
                           data = new_data_pm2_non_missing,trControl=tc,verbose=FALSE)

### Runnig the XGB Boost model 
set.seed(123)
xgb_cv_pm2_d <-train(Y.PM2.5 ~ dailyindex+monthlyindex+Year, method="xgbTree",
                   data =new_data_pm2_non_missing,trControl=tc,verbose=FALSE)

### Runnig the Neural Network model 
# design the parameter tuning grid
nnetGrid <-  expand.grid(size = seq(from = 5, to = 40, by = 10),
                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
set.seed(123)
# Neural Network model
nn_cv_pm2_d <- caret::train(Y.PM2.5 ~ dailyindex+monthlyindex+Year, method="nnet",
                          data = new_data_pm2_non_missing,tuneGrid=nnetGrid,trControl=tc,linout=TRUE, linear.output=TRUE,trace = FALSE,metric="RMSE",verbose=FALSE)

# List the model you have run 
model_list <- list(lm=lm_cv_pm2_d,glmnet=lasso_cv_pm2_d,rf=rf_cv_pm2_d,xgbTree=xgb_cv_pm2_d,nnet=nn_cv_pm2_d,gbm=gbm_cv_pm2_d)
res <- resamples(model_list)
# Getting the summary of the models
summary(res)
plot(varImp(rf_cv_pm2_d))


# Box plot of the Metrics
boxplot(res$values[c(2,5,8,11,14)],ylab="MAE", col="orange",border="brown")
boxplot(res$values[c(3,6,9,12,15)],ylab="RMSE", col="orange",border="brown")
boxplot(res$values[c(4,7,10,13,16)],ylab="R2", col="orange",border="brown")

# Fitting the regression model on the missing value 
missing_prediction_pm2 <- predict(nn_cv_pm2_d, newdata=new_data_pm2_missing)

# Substituting the missing value back in the data frame 
new_data_pm2_missing$Y.PM2.5 <- missing_prediction_pm2
# Combining the rows to have a combined data frame
new_data_pm2_filled <- rbind(new_data_pm2_missing,new_data_pm2_non_missing)
# Filtering the dataframe through the date
new_data_pm2_filled <- arrange(new_data_pm2_filled,date)
# Checking the structure of the dataframe
str(new_data_pm2_filled)

# calculating all days so that we can show Non-missing values and Imputed values distinctively
all_days = data.frame(date = seq.POSIXt(from = min(new_data_pm2_missing$date), to = max(new_data_pm2_missing$date), by = "day"))
dd_complete = left_join(all_days, new_data_pm2_missing, by = "date")

#Plotting the Imputation values
ggplot() + geom_line(data=ts2,aes(x=date,y=Y.PM2.5,color="Non_Missing Values")) + 
    geom_line(data=dd_complete, aes(date, Y.PM2.5,color="Imputed values"))+
    labs(color="Random Forest")

# Using IMPUTE TS functionalities
plot(na_kalman(ts1$PM2.5, model = "StructTS"))
plot(na_seadec(ts1$PM2.5, algorithm = "interpolation",find_frequency = TRUE))

# Here we are fitting the model on training and checking the accuracy 
missing_prediction_2 <- predict(nn_cv_pm2_d)
length(missing_prediction_2) ==length(new_data_pm2_non_missing$Y.PM2.5)

# Different accuracy methods
R <- rPearson(new_data_pm2_non_missing$Y.PM2.5,missing_prediction_2)
R2 <- R2(new_data_pm2_non_missing$Y.PM2.5 ,missing_prediction_2)
RMSE <- rmse(new_data_pm2_non_missing$Y.PM2.5 ,missing_prediction_2)
NRMSE <- nrmse(new_data_pm2_non_missing$Y.PM2.5 ,missing_prediction_2)
Bias <- bias(new_data_pm2_non_missing$Y.PM2.5 ,missing_prediction_2)
IOA <- md(new_data_pm2_non_missing$Y.PM2.5 ,missing_prediction_2)
MAPE <- mape(new_data_pm2_non_missing$Y.PM2.5 ,missing_prediction_2)

# Finally create a data frame for different accuracy methods
RESULTS <- data.frame(R,R2,RMSE,NRMSE,Bias,IOA,MAPE)
RESULTS



