## The purpose of this script is to apply different imputation techniques on the New_Seasonality Data 
## Run the Indexing.R script 
# You can go to the script and run it first or just run the source command below
source("Seasonality/Indexing.R")
# Importing caret Package
library(caret)

# Set a seed so that random generation is common
set.seed(123)
# Subsetting the dataset and run the model on the subset data first
subset <- new_data_non_missing[1:1000,]

# Defining train control for 10 fold Crossvalidation 
tc <- trainControl(method="cv", number = 10)
# Measuring the time as well of running the model  
start_time <- Sys.time()
## Commented out all models remove the comment for which you want to run

#### Fitting the linear model
#lm_cv <- train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="lm",data = new_data_non_missing,trControl=tc)

#### By keeping Alpha zero and lambda 0 will run the ridge model
#ridge_cv <- train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="glmnet",data = new_data_non_missing,trControl=tc,
#                   tuneGrid=expand.grid(alpha=0,lambda=0))

####Running Lasso model
#lasso_cv <- train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="glmnet",data = new_data_non_missing,trControl=tc,
#                   tuneGrid=expand.grid(alpha=1,lambda=0))

#### Fitting the Random Forest model (Caution that it takes atleast 3-4 hours to run) 
#rf_cv <- train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="rf",data = new_data_non_missing,trControl=tc)

### Running the Gradient Boosting Model
# We use expand grid to create a table of all possible combinations
#tg_gbm <- expand.grid(shrinkage = seq(0.1, 1, by = 0.2), 
#                      interaction.depth = c(1, 3, 7, 10),
#                      n.minobsinnode = c(2, 5, 10),
#                      n.trees = c(100, 300, 500,1000))
#gbm_cv <- caret::train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="gbm",tuneGrid=tg_gbm,
#                       data = new_data_non_missing,trControl=tc,verbose=FALSE)

### Runnig the XGB Boost model 
#xgb_cv <-train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="xgbTree",data = new_data_non_missing,trControl=tc,verbose=FALSE)

### Runnig the Neural Network model 
# design the parameter tuning grid
#nnetGrid <-  expand.grid(size = seq(from = 5, to = 40, by = 10),
#                         decay = seq(from = 0.1, to = 0.5, by = 0.1))
#nn_cv <- caret::train(PM10 ~ hourlyindex+dailyindex+monthlyindex+Week+Year, method="nnet",
#                      data = new_data_non_missing,tuneGrid=nnetGrid,trControl=tc,linout=TRUE, linear.output=TRUE,trace = FALSE,metric="RMSE",verbose=FALSE)

end_time <- Sys.time()
end_time - start_time

# List the model you have run 
model_list <- list(lm=lm_cv,glmnet=ridge_cv,rf=rf_cv,xgbTree=xgb_cv,nnet=nn_cv,gbm=gbm_cv)
res <- resamples(model_list)
summary(res)



### If you comment out all then the Whole Script will take about 10-12 hours depending on the Computer###
### Therefore try to run the model on subset data by just changing giving subset as argument in Data###