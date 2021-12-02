
library(ggrepel)
library(dplyr) 
library(ggplot2)
library(tidyr)
library(openxlsx)
library(reshape)
library(gbm)
library(randomForest)
library(rpart)
library(rpart.plot)
library(cutpointr)
library(yardstick)


# Importing the raw data from csv file
SALES_RAW <- read.csv("ZILLOW SALES DATA.csv")

###### EXPLORING THE DATA ##########

# Exploring the variables of the raw data
str(SALES_RAW)

# Cleaning the data and selecting the relevant variables
SALES <- SALES_RAW %>%
  select(zip = address.zipcode, address = address.streetAddress, bathrooms, bedrooms, area = livingArea, yearBuilt, price) %>%
  drop_na() 

# The price will be the target variable

# Plotting the sales price vs the number of bedrooms  ---- NOTE: should actually be a categorical variable
ggplot(SALES, aes(x = bedrooms, y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Bedrooms (Seize the Data)", 
       y = "PRICE", 
       x = "BEDROOMS")

# Plotting the sales price vs the number of bathrooms  ---- NOTE: should actually be a categorical variable
ggplot(SALES, aes(x = bathrooms, y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Bathrooms (Seize the Data)", 
       y = "PRICE", 
       x = "BATHROOMS")

# Plotting the sales price vs the zip ---- NOTE: should actually be a categorical variable
ggplot(SALES, aes(x = as.factor(zip), y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Zip Code (Seize the Data)", 
       y = "PRICE", 
       x = "ZIPCODE")

# Plotting the sales price vs the yearBuilt 
ggplot(SALES, aes(x = yearBuilt, y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Year Built (Seize the Data)", 
       y = "PRICE", 
       x = "YEAR BUILT")

# Plotting the sales price vs the area ----- NOTE: there are some 5 clear outliers that should be eliminated
ggplot(SALES, aes(x = area, y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Square Footage (Seize the Data)", 
       y = "PRICE", 
       x = "SQ FT")

###### CLEANING THE DATA BY REMOVING OUTLIERS
# Create a not in operator
`%notin%` <- Negate(`%in%`)

# Removing some of the outliers
SALES2 <- SALES %>%
  filter(price < 1000000) %>%
  filter(area < 10000) %>%
  filter(yearBuilt > 1900) %>%
  filter(bedrooms < 10) %>%
  filter (bathrooms < 10) %>%
  filter(area > 0) %>%
  filter(zip %notin% c("27217","27249","27263","27265","27284","27313","27317","27357","27377","27411"))

str(SALES2)

###### REPLOTTING THE DATA

# Plotting the sales price vs the number of bedrooms  
ggplot(SALES2, aes(x = as.factor(bedrooms), y = price))+
  geom_point() +
  labs(title = "Greensboro Home Prices: Bedrooms (Seize the Data)", 
       y = "PRICE", 
       x = "BEDROOMS")

# Plotting the sales price vs the number of bathrooms 
ggplot(SALES2, aes(x = bathrooms, y = price))+
  geom_point() +
  labs(title = "Greensboro Home Prices: Bathrooms (Seize the Data)", 
       y = "PRICE", 
       x = "BATHROOMS")+
  geom_smooth(method = "lm", formula = y ~ x)

# Plotting the sales price vs the zip 
ggplot(SALES2, aes(x = as.factor(zip), y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Zip Code (Seize the Data)", 
       y = "PRICE", 
       x = "ZIP CODE")


# Plotting the sales price vs the yearBuilt 
ggplot(SALES2, aes(x = yearBuilt, y = price))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Year Built (Seize the Data)", 
       y = "PRICE", 
       x = "YEAR BUILT") +
  geom_smooth(method = "lm", formula = y ~ x)

# Plotting the sales price vs the area 
ggplot(SALES2, aes(x = area, y = price, color = bedrooms))+
  geom_point()+
  labs(title = "Greensboro Home Prices: Square Footage (Seize the Data)", 
       y = "PRICE", 
       x = "SQ FT")+
  geom_smooth(method = "lm", formula = y ~ x)

####### SEPARATE THE DATA SET INTO TEST DATA AND MODEL DATA

#1 RANDOMIZE THE TABLE JUST IN CASE IT IS ORDERED BY SOME VARIABLE SUCH AS DATE OR PRICE TO NOT SKEW THE MODEL

# Create a random seed so that it is reproducible 
set.seed(42)

# Shuffle the row indices of the table
ROWS <- sample(nrow(SALES2)) 

# Reorder the table rows by the seed vector
SALES3 <- SALES2[ROWS,]

# Count the number of rows
ROWCOUNT <- count(SALES3)

# Select the first 1/4 rows from the randomized sales data to be test data
SALES_TEST_DATA <-head(SALES3, as.integer(ceiling(ROWCOUNT/4)))

# Select the last 3/4 rows from the randomized sales data to be model data

SALES_TRAIN_DATA <-tail(SALES3, as.integer(ROWCOUNT) - as.integer(ceiling(ROWCOUNT/4))) 



####### CREATING A SIMPLE LINEAR REGRESSION MODEL WITH ALL VARIABLES OF THE MODEL DATA 

SALES_REGRESSION <- lm(price ~  bathrooms + area + yearBuilt + as.factor(bedrooms) + as.factor(zip), data = SALES_TRAIN_DATA)

summary(SALES_REGRESSION)

predicted_linear_regression <- SALES_REGRESSION %>%
  predict(newdata = SALES_TEST_DATA)


##### CREATING A GRADIENT BOOSTING MODEL #######

SALES_GRADIENT_BOOST <- gbm(price ~  bathrooms + area + yearBuilt + as.factor(bedrooms) + as.factor(zip), data = SALES_TRAIN_DATA, distribution = "gaussian", n.trees = 1000)

summary(SALES_GRADIENT_BOOST)

predicted_grad_boost <- SALES_GRADIENT_BOOST %>%
  predict(newdata = SALES_TEST_DATA,  n.trees = 1000)


##### CREATING A RANDOM FOREST MODEL ########

SALES_RANDOM_FOREST <- randomForest(price ~  bathrooms + area + yearBuilt + bedrooms + zip, data = SALES_TRAIN_DATA, ntree = 1000, importance = TRUE)

plot(SALES_RANDOM_FOREST)
varImpPlot(SALES_RANDOM_FOREST)

predicted_random_forest <- SALES_RANDOM_FOREST %>%
  predict(newdata = SALES_TEST_DATA)


###### CREATING A DECISION TREE MODEL #######

SALES_RTREE <- rpart(price ~  bathrooms + area + yearBuilt + as.factor(bedrooms) + as.factor(zip), data = SALES_TRAIN_DATA, method = "anova")

summary(SALES_RTREE)
plot(SALES_RTREE)

predicted_rtee <- SALES_RTREE %>%
  predict(newdata = SALES_TEST_DATA)

###### Running the Test data agianst all four models to compare forecast to actual price

SALES_TEST_DATA <- SALES_TEST_DATA %>%
  mutate(liner_regresion = predicted_linear_regression) %>%
  mutate(gradient_boosting = predicted_grad_boost) %>%
  mutate(random_forest = predicted_random_forest) %>%
  mutate(regression_tree = predicted_rtee) 


#### RMSE of all four models #####

SALES_TEST_DATA %>%
  rmse(liner_regresion, price)

SALES_TEST_DATA %>%
  rmse(gradient_boosting, price)

SALES_TEST_DATA %>%
  rmse(random_forest, price)

SALES_TEST_DATA %>%
  rmse(regression_tree, price)

head(SALES_TEST_DATA)