# empty the environment
rm(list=ls())                                    
# set work directory
setwd("C:/Users/rafsa/OneDrive/Desktop/DSBA Fall'23/DSBA 6201(Cornelia)/Assignment 01")           
getwd()
#view the file in the default directory
list.files()

# read house data
house = read.csv("house_value.csv", header=TRUE) 

# explore the characteristics of the dataframe
# structure of dataframe
str(house)

# summary stats of dataframe
summary(house)

# data preparation
#check if there is missing values in the data
sum(is.na(house))      

#Explore the visual charachtersistics of the Dataframe
#distribution of meadian_house _value  using Histogram
hist(house$median_house_value,
     main = "Frequency distrbution of Median house value",
     xlab = "Median_house _value",
     ylab = "Frequency")

#converted median_house_value into logarithm
house$log_median_house_value = log(house$median_house_value)
# histogram of the target variable
hist(house$log_median_house_value ,
     main = "Log-Transformed Median House Value Distribution",
     xlab = "Log Median House Value",
     ylab = "Frequency")


# scatterplot for visualizing correlation among variables
plot(house)

# scatterplot among two independent variables with target variable
plot(house[c(11,3,6)])

###multiple linear regression###
#to address overfitting of the estimated model, we split the data into training &test
#we use simple random sampling & use 65% of data for training
#we use the other 35% for test in order to assess the performance of the model
#find the number of observations in house
n = nrow(house)

#create an index of 65% of n rows
trainIndex = sample(n, as.integer (n*0.65), replace = FALSE)
#dividing the dataset
train = house[trainIndex, ]
test = house [-trainIndex, ]


# fit a multiple linear regression on house_value_csv data (log(median_house_value) as the target variable &
#                                                 ocean_proximity+ any 5 of the other independent variables)

#DV = log_median_house_value, IVs = median_income, population, housing_median_age, households,
#                                               total_bedrooms_per_household, ocean_proximity

#Y = B0 + B1*median_income+ B2*population + B3*housing_median_age+ B4*households + B5*total_bedrooms_per_household 
#                                                                                             + B6*ocean_proximity
  
lm.fit1 = lm(log_median_house_value ~ 
               median_income+
               population+
               housing_median_age+
               households+
               total_bedrooms_per_household+
               ocean_proximity, data = train)
lm.fit1 <- lm(log(median_house_value)~ocean_proximity+median_income+total_rooms+population+households+median_house_value, data = train)

#summary output of the estimation
summary(lm.fit1)
#verifying linear regression assumptions:
#view the residual plots
plot(lm.fit1)

#Assess multicollinearity by plotting the correlations of independent variables 
plot(house[c(11,4,5,6,8,10,1)])



## Assess multicollinearity (with all independent numeric variables)
lm.fit2= lm(log_median_house_value ~ 
              median_income+
              population+
              housing_median_age+
              total_rooms+
              total_bedrooms+
              households+
              total_rooms_per_household+
              total_bedrooms_per_household, data = train)

#summary output of the estimation
summary(lm.fit2)
#view the residual plots
plot(lm.fit2) 
# plot the correlations of all numeric independent variables
plot(house[c(1,2,3,4,5,6,9,10,11)])

# checking multicollinearity
install.packages("car")
library(car)

vif(lm.fit2) 

#VIF value >= 5 is considered as significant multicollinearity in the data
# drop the variable with highest VIF
#let us drop households and rerun the model

lm.fit3= lm(log_median_house_value ~
              median_income+
              population+
              housing_median_age+
              total_rooms+
              total_bedrooms+
              total_rooms_per_household+
              total_bedrooms_per_household, data = train)
summary(lm.fit3)
vif(lm.fit3)


# to get the various sums of squares 
anova(lm.fit3)
























