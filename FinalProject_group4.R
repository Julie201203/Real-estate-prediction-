
## Real Estate  Multiple Linear Regression
# load the necessary packages
#library(GGally)
#library(car)
library(MASS)
library(olsrr)

## 1) Read csv file into R using

original_RealEstate <- read.csv("E:\\Ella BC\\DA 310 Introduction to DAta Analytics\\files\\RealEstate.csv",header = TRUE)
print (original_RealEstate)

##clean the dataset by removing NA values, negative values

# removing NA records and negative values
original_RealEstate[original_RealEstate < 0] <- NA
RealEstate <- na.omit(original_RealEstate)

# Summary
RealEstate_summary <- summary(RealEstate)
print(RealEstate_summary)

## creating a head  ( for general view )
RealEstate_head <- head(RealEstate)
print(RealEstate_head)

## creating a plot that can give us overall view of variables 
plot(RealEstate)

# after eliminating the NA values and Negative values , we decided to calculate the outliers (Mild and Extreme)

# calculating outliers for house_price_of_unit_area (dependent variable)
upper_outer_fence_price <- quantile(RealEstate$house_price_of_unit_area,0.75) + 3 * (quantile(RealEstate$house_price_of_unit_area,0.75)- quantile(RealEstate$house_price_of_unit_area,0.25))
upper_inner_fence_price <-quantile(RealEstate$house_price_of_unit_area,0.75) + 1.5 * (quantile(RealEstate$house_price_of_unit_area,0.75)- quantile(RealEstate$house_price_of_unit_area,0.25))
lower_outer_fence_price <-quantile(RealEstate$house_price_of_unit_area,0.25) - 3 * (quantile(RealEstate$house_price_of_unit_area,0.75)- quantile(RealEstate$house_price_of_unit_area,0.25))
lower_inner_fence_price <-quantile(RealEstate$house_price_of_unit_area,0.25) - 1.5 * (quantile(RealEstate$house_price_of_unit_area,0.75)- quantile(RealEstate$house_price_of_unit_area,0.25))

# calculating outliers for transaction_date (independent variable)
upper_outer_fence_transaction_date <- quantile(RealEstate$transaction_date,0.75) + 3 * (quantile(RealEstate$transaction_date,0.75)- quantile(RealEstate$transaction_date,0.25))
upper_inner_fence_transaction_date <-quantile(RealEstate$transaction_date,0.75) + 1.5 * (quantile(RealEstate$transaction_date,0.75)- quantile(RealEstate$transaction_date,0.25))
lower_outer_fence_transaction_date <-quantile(RealEstate$transaction_date,0.25) - 3 * (quantile(RealEstate$transaction_date,0.75)- quantile(RealEstate$transaction_date,0.25))
lower_inner_fence_transaction_date <-quantile(RealEstate$transaction_date,0.25) - 1.5 * (quantile(RealEstate$transaction_date,0.75)- quantile(RealEstate$transaction_date,0.25))

# calculating outliers for house_age (independent variable)
upper_outer_fence_house_age <- quantile(RealEstate$house_age,0.75) + 3 * (quantile(RealEstate$house_age,0.75)- quantile(RealEstate$house_age,0.25))
upper_inner_fence_house_age <-quantile(RealEstate$house_age,0.75) + 1.5 * (quantile(RealEstate$house_age,0.75)- quantile(RealEstate$house_age,0.25))
lower_outer_fence_house_age <-quantile(RealEstate$house_age,0.25) - 3 * (quantile(RealEstate$house_age,0.75)- quantile(RealEstate$house_age,0.25))
lower_inner_fence_house_age <-quantile(RealEstate$house_age,0.25) - 1.5 * (quantile(RealEstate$house_age,0.75)- quantile(RealEstate$house_age,0.25))

# calculating outliers for distance_to_the_nearest_MRT_station (independent variable)
upper_outer_fence_distance_to_the_nearest_MRT_station <- quantile(RealEstate$distance_to_the_nearest_MRT_station,0.75) + 3 * (quantile(RealEstate$distance_to_the_nearest_MRT_station,0.75)- quantile(RealEstate$distance_to_the_nearest_MRT_station,0.25))
upper_inner_fence_distance_to_the_nearest_MRT_station <-quantile(RealEstate$distance_to_the_nearest_MRT_station,0.75) + 1.5 * (quantile(RealEstate$distance_to_the_nearest_MRT_station,0.75)- quantile(RealEstate$distance_to_the_nearest_MRT_station,0.25))
lower_outer_fence_distance_to_the_nearest_MRT_station <-quantile(RealEstate$distance_to_the_nearest_MRT_station,0.25) - 3 * (quantile(RealEstate$distance_to_the_nearest_MRT_station,0.75)- quantile(RealEstate$distance_to_the_nearest_MRT_station,0.25))
lower_inner_fence_distance_to_the_nearest_MRT_station <-quantile(RealEstate$distance_to_the_nearest_MRT_station,0.25) - 1.5 * (quantile(RealEstate$distance_to_the_nearest_MRT_station,0.75)- quantile(RealEstate$distance_to_the_nearest_MRT_station,0.25))


# calculating outliers for number_of_convenience_stores (independent variable)
upper_outer_fence_number_of_convenience_stores <- quantile(RealEstate$number_of_convenience_stores,0.75) + 3 * (quantile(RealEstate$number_of_convenience_stores,0.75)- quantile(RealEstate$number_of_convenience_stores,0.25))
upper_inner_fence_number_of_convenience_stores <-quantile(RealEstate$number_of_convenience_stores,0.75) + 1.5 * (quantile(RealEstate$number_of_convenience_stores,0.75)- quantile(RealEstate$number_of_convenience_stores,0.25))
lower_outer_fence_number_of_convenience_stores <-quantile(RealEstate$number_of_convenience_stores,0.25) - 3 * (quantile(RealEstate$number_of_convenience_stores,0.75)- quantile(RealEstate$number_of_convenience_stores,0.25))
lower_inner_fence_number_of_convenience_stores <-quantile(RealEstate$number_of_convenience_stores,0.25) - 1.5 * (quantile(RealEstate$number_of_convenience_stores,0.75)- quantile(RealEstate$number_of_convenience_stores,0.25))



# calculating outliers for latitude (independent variable)
upper_outer_fence_latitude <- quantile(RealEstate$latitude,0.75) + 3 * (quantile(RealEstate$latitude,0.75)- quantile(RealEstate$latitude,0.25))
upper_inner_fence_latitude <-quantile(RealEstate$latitude,0.75) + 1.5 * (quantile(RealEstate$latitude,0.75)- quantile(RealEstate$latitude,0.25))
lower_outer_fence_latitude <-quantile(RealEstate$latitude,0.25) - 3 * (quantile(RealEstate$latitude,0.75)- quantile(RealEstate$latitude,0.25))
lower_inner_fence_latitude <-quantile(RealEstate$latitude,0.25) - 1.5 * (quantile(RealEstate$latitude,0.75)- quantile(RealEstate$latitude,0.25))


# calculating outliers for longitude (independent variable)
upper_outer_fence_longitude <- quantile(RealEstate$longitude,0.75) + 3 * (quantile(RealEstate$longitude,0.75)- quantile(RealEstate$longitude,0.25))
upper_inner_fence_longitude <-quantile(RealEstate$longitude,0.75) + 1.5 * (quantile(RealEstate$longitude,0.75)- quantile(RealEstate$longitude,0.25))
lower_outer_fence_longitude <-quantile(RealEstate$longitude,0.25) - 3 * (quantile(RealEstate$longitude,0.75)- quantile(RealEstate$longitude,0.25))
lower_inner_fence_longitude <-quantile(RealEstate$longitude,0.25) - 1.5 * (quantile(RealEstate$longitude,0.75)- quantile(RealEstate$longitude,0.25))


extreme_outliers_price <- RealEstate$house_price_of_unit_area [RealEstate$house_price_of_unit_area > upper_outer_fence_price | RealEstate$house_price_of_unit_area  < lower_outer_fence_price]
print(extreme_outliers_price)
mild_outliers_price<- RealEstate$house_price_of_unit_area [(RealEstate$house_price_of_unit_area > upper_inner_fence_price |RealEstate$house_price_of_unit_area < lower_inner_fence_price) & !RealEstate$house_price_of_unit_area %in% extreme_outliers_price]
print(mild_outliers_price)

extreme_outliers_transaction_date <- RealEstate$transaction_date [RealEstate$transaction_date > upper_outer_fence_transaction_date | RealEstate$transaction_date  < lower_outer_fence_transaction_date]
print(extreme_outliers_transaction_date)
mild_outliers_transaction_date<- RealEstate$transaction_date [(RealEstate$transaction_date  > upper_inner_fence_transaction_date | RealEstate$transaction_date < lower_inner_fence_transaction_date) & !RealEstate$transaction_date %in% extreme_outliers_transaction_date]
print(mild_outliers_transaction_date)


extreme_outliers_house_age <- RealEstate$house_age [RealEstate$house_age > upper_outer_fence_house_age | RealEstate$house_age  < lower_outer_fence_house_age]
print(extreme_outliers_house_age)
mild_outliers_house_age<- RealEstate$house_age [(RealEstate$house_age  > upper_inner_fence_house_age | RealEstate$house_age < lower_inner_fence_house_age) & !RealEstate$house_age %in% extreme_outliers_house_age]
print(mild_outliers_house_age)

extreme_outliers_distance_to_the_nearest_MRT_station <- RealEstate$distance_to_the_nearest_MRT_station [RealEstate$distance_to_the_nearest_MRT_station > upper_outer_fence_distance_to_the_nearest_MRT_station | RealEstate$distance_to_the_nearest_MRT_station  < lower_outer_fence_distance_to_the_nearest_MRT_station]
print(extreme_outliers_distance_to_the_nearest_MRT_station)
mild_outliers_distance_to_the_nearest_MRT_station <- RealEstate$distance_to_the_nearest_MRT_station [(RealEstate$distance_to_the_nearest_MRT_station  > upper_inner_fence_distance_to_the_nearest_MRT_station | RealEstate$distance_to_the_nearest_MRT_station < lower_inner_fence_distance_to_the_nearest_MRT_station) & !RealEstate$distance_to_the_nearest_MRT_station %in% extreme_outliers_distance_to_the_nearest_MRT_station]
print(mild_outliers_distance_to_the_nearest_MRT_station)


extreme_outliers_number_of_convenience_stores <- RealEstate$number_of_convenience_stores [RealEstate$number_of_convenience_stores > upper_outer_fence_number_of_convenience_stores | RealEstate$number_of_convenience_stores  < lower_outer_fence_number_of_convenience_stores]
print(extreme_outliers_number_of_convenience_stores)
mild_outliers_number_of_convenience_stores<- RealEstate$number_of_convenience_stores [(RealEstate$number_of_convenience_stores  > upper_inner_fence_number_of_convenience_stores | RealEstate$number_of_convenience_stores < lower_inner_fence_number_of_convenience_stores) & !RealEstate$number_of_convenience_stores %in% extreme_outliers_number_of_convenience_stores]
print(mild_outliers_number_of_convenience_stores)

extreme_outliers_latitude <- RealEstate$latitude [RealEstate$latitude > upper_outer_fence_latitude | RealEstate$latitude  < lower_outer_fence_latitude]
print(extreme_outliers_latitude)
mild_outliers_latitude<- RealEstate$latitude [(RealEstate$latitude  > upper_inner_fence_latitude | RealEstate$latitude < lower_inner_fence_latitude) & !RealEstate$latitude %in% extreme_outliers_latitude]
print(mild_outliers_latitude)

extreme_outliers_longitude <- RealEstate$longitude [RealEstate$longitude > upper_outer_fence_longitude | RealEstate$longitude  < lower_outer_fence_longitude]
print(extreme_outliers_longitude)
mild_outliers_longitude <- RealEstate$longitude [(RealEstate$longitude  > upper_inner_fence_longitude | RealEstate$longitude < lower_inner_fence_longitude) & !RealEstate$longitude %in% extreme_outliers_longitude]
print(mild_outliers_longitude)




## histogram for house_price_of_unit_area, transaction_date  , house_age, distance_to_the_nearest_MRT_station, number_of_convenience_stores, latitude, longitude 

hist (RealEstate$house_price_of_unit_area,xlab="Price of unit area",main = paste("Histogram of Price of unit area"))
hist (RealEstate$transaction_date,xlab="Transaction date ",main = paste("Histogram of transaction_date "))
hist (RealEstate$house_age,xlab="House Age",main = paste("Histogram of House Age"))
hist (RealEstate$distance_to_the_nearest_MRT_station,xlab="distance to thenearest MRT station ",main = paste("Histogram of distance to the nearest MRT station "))
hist (RealEstate$number_of_convenience_stores,xlab="Number of convenience stores ",main = paste("Histogram of Number of convenience stores "))
hist (RealEstate$latitude,xlab="latitude ",main = paste("Histogram of latitude "))
hist (RealEstate$longitude,xlab="longitude ",main = paste("Histogram of longitude "))

## Scatter Plot for transaction_date, house_age, distance_to_the_nearest_MRT_station, number_of_convenience_stores, latitude, longitude among price
scatter.smooth(x=RealEstate$transaction_date, 
               y=RealEstate$house_price_of_unit_area
               , main="Transaction Date Vs.House price") 

scatter.smooth(x=RealEstate$house_age, 
               y=RealEstate$house_price_of_unit_area
               , main="House Age Vs.House price")

scatter.smooth(x=RealEstate$distance_to_the_nearest_MRT_station, 
               y=RealEstate$house_price_of_unit_area
               , main="Distance to the nearest MRT station Vs.House price")

scatter.smooth(x=RealEstate$number_of_convenience_stores, 
               y=RealEstate$house_price_of_unit_area
               , main="Number of convenience stores Vs.House price") 

scatter.smooth(x=RealEstate$latitude, 
               y=RealEstate$house_price_of_unit_area
               , main="Latitude Vs.House price") 

scatter.smooth(x=RealEstate$longitude, 
               y=RealEstate$house_price_of_unit_area
               , main="Longitude Vs.Houseprice") 

## Box Plot for price, transaction_date, house_age, distance_to_the_nearest_MRT_station, number_of_convenience_stores, latitude, longitude 

boxplot(RealEstate$house_price_of_unit_area, main= "House price of unit area", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$house_price_of_unit_area)$out)) 

boxplot(RealEstate$transaction_date, main= "Transaction Date", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$transaction_date)$out)) 

boxplot(RealEstate$house_age, main= "House Age", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$house_age)$out)) 

boxplot(RealEstate$distance_to_the_nearest_MRT_station, main= "Distance to the nearest MRT station", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$distance_to_the_nearest_MRT_station)$out)) 

boxplot(RealEstate$number_of_convenience_stores, main= "Number of convenience stores", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$number_of_convenience_stores)$out)) 


boxplot(RealEstate$latitude, main= "Latitude", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$latitude)$out)) 


boxplot(RealEstate$longitude , main= "Longitude ", 
        sub=paste("Outlier rows:", boxplot.stats(RealEstate$longitude )$out)) 


## Density Plot for price, transaction_date, house_age, distance_to_the_nearest_MRT_station, number_of_convenience_stores, latitude, longitude 

plot(density(RealEstate$house_price_of_unit_area), main= "Density Plot: 
House price of unit area", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$house_price_of_unit_area), 2))) 

plot(density(RealEstate$transaction_date), main= "Density Plot: 
Transaction Date", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$transaction_date), 2))) 

plot(density(RealEstate$house_age), main= "Density Plot: 
House Age", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$house_age), 2))) 

plot(density(RealEstate$distance_to_the_nearest_MRT_station), main= "Density Plot: 
Distance to the nearest MRT station", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$distance_to_the_nearest_MRT_station), 2))) 

plot(density(RealEstate$number_of_convenience_stores), main= "Density Plot: 
Number of convenience stores", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$number_of_convenience_stores), 2))) 

plot(density(RealEstate$latitude), main= "Density Plot: 
Latitude", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$latitude), 2))) 

plot(density(RealEstate$longitude ), main= "Density Plot: 
Longitude", ylab= "Frequency", sub=paste("Skewness:",round(e1071::skewness(RealEstate$longitude ), 2))) 


## Split the dataset into 4:1 ratio as training dataset and testing dataset 
sample_size = floor(0.80*nrow(RealEstate)) 
set.seed(123) 
train_ind = sample(seq_len(nrow(RealEstate)),size = sample_size) 
train =RealEstate[train_ind,] 
test=RealEstate[-train_ind,] 

## Build up linear regression model on the training dataset 
linearmodel<-lm(house_price_of_unit_area ~  transaction_date + house_age + distance_to_the_nearest_MRT_station + number_of_convenience_stores +latitude +longitude , data=train) 
summary(linearmodel) 
plot(linearmodel)

# There are 2^k possible candidate modeles. It is not efficient to manually test all possible combinations.
# We use stepwise regression method here
stepReg <- ols_step_both_p(linearmodel, details = TRUE)

# build the lm model for the best model found in stepwise:
model_step1<-lm(house_price_of_unit_area ~  transaction_date + house_age + distance_to_the_nearest_MRT_station + number_of_convenience_stores +latitude , data=train) 

## Check the four assumptions of linear regression (If one or more assumptions are not met, please explain the situation in the conclusion) 
#a. Independence of Observations: 
#b. Normality: explain about density plot
#c. Linearity: explain about scatter plot 
#d. Homoscedasticity: 
par(mfrow=c(2,2))
plot(model_step1)
summary(model_step1)

# transformation of data
# used the materials in reference: 
#https://www.css.cornell.edu/faculty/dgr2/_static/files/R_html/Transformations.html
bc <- boxCox(train$house_price_of_unit_area ~ 1, plotit = TRUE)

lambda_optimal  <- bc$x[which.max(bc$y)]
BCTransform <- function(y, lambda=0) {
  if (lambda == 0L) { log(y) }
  else { (y^lambda - 1) / lambda }
}
BCTransformInverse <- function(yt, lambda=0) {
  if (lambda == 0L) { exp(yt) }
  else { exp(log(1 + lambda * yt)/lambda) }
}

y_after <- BCTransform(train$house_price_of_unit_area, lambda_optimal)
par(mfrow=c(3,1))
hist (train$house_price_of_unit_area,xlab="Price of unit area",main = paste("Histogram of Price of unit area"))
hist (y_after,xlab="Price of unit area",main = paste("After Square Root Transformation"))
#The skewness has been removed
#Compare to the log-transform:
hist (log(train$house_price_of_unit_area),xlab="Price of unit area",main = paste("After Log Transformation"))
# Square-root transformation is more similar to the Normal

### transformation for predictors

x_after <- BCTransform(train$distance_to_the_nearest_MRT_station, 0)
x_root_after <- BCTransform(train$distance_to_the_nearest_MRT_station, 0.5)
hist (train$distance_to_the_nearest_MRT_station,xlab="Distance to thenearest MRT station",main = paste("Histogram distance to thenearest MRT station"))
hist (x_root_after,xlab="Distance to thenearest MRT station",main = paste("After Square Root Transformation"))
#The skewness has been removed
#Compare to the log-transform:
hist (log(train$distance_to_the_nearest_MRT_station),xlab="Distance to thenearest MRT station",main = paste("After Log Transformation"))
#This suggests to use log transformation for distance_to_the_nearest_MRT_station

# Re-run the best stepwise model with transformation:

# consider square root of y:
model_2<-lm(y_after ~  transaction_date + house_age + distance_to_the_nearest_MRT_station + number_of_convenience_stores +latitude , data=train) 
par(mfrow=c(2,2))
plot(model_2)
summary(model_2)

### result of stepwise is similar to model_2
model_step2 <- ols_step_both_p(model_2, details = TRUE)
summary(model_step2)

# consider log variable
model_3<-lm(y_after ~  transaction_date + house_age + log(distance_to_the_nearest_MRT_station) + number_of_convenience_stores +latitude , data=train) 
plot(model_3)
summary(model_3)

### result of stepwise is different now. number_of_convenience_stores is removed from the model
model_step3 <- ols_step_both_p(model_3, details = TRUE)
summary(model_step3)

model_final<-lm(y_after ~  transaction_date + house_age + log(distance_to_the_nearest_MRT_station) +latitude , data=train) 
plot(model_final)
summary(model_final)

# The residuals are close to normally-distributed

## check the P-value and Multiple-R
summary(model_final) 

## Run prediction on the testing dataset and check the performance  

predicted_root <- predict(model_final, test) 
predicted <- BCTransformInverse(predicted_root, lambda_optimal)
summary(predicted)

par(mfrow=c(1,1))
plot(x=predicted, y=test$house_price_of_unit_area, 
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values For Final Model',
     col = "darkorange", lwd = 2)
abline(a=0, b=1)

## for model_step1
predicted_step1 <- predict(model_step1, test) 
par(mfrow=c(1,1))
plot(x=predicted_step1, y=test$house_price_of_unit_area, 
     xlab='Predicted Values',
     ylab='Actual Values',
     main='Predicted vs. Actual Values For Linear Model',
     col = "darkorange", lwd = 2)
abline(a=0, b=1)

