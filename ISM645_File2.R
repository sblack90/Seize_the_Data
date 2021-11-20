

library(ggrepel)
library(dplyr) 
library(ggplot2)
library(tidyr)
library(openxlsx)
library(reshape)

# Importing the raw data from xlsx file
house_series_data <- read.xlsx("PA Data Merge.xlsx")

###### EXPLORING THE DATA ##########

# Exploring the variables of the raw data
str(house_series_data)

# Cleaning the data and selecting the relevant variables

hsd_2 <- house_series_data %>% 
  mutate(median_price=(`Median_House_Price_1`+`Median_House_Price_2`+`Median_House_Price_3`+ `Median_House_Price_4`+`Median_House_Price_5`)/5, Mort_15_Year=as.numeric(Mort_15_Year)) %>%
  mutate(price_change=(lead(median_price)/median_price-1)) %>% 
  select(Year,CPI,BitCoin,Unemployment,GDP_Billions,Population,Mort_15_Year,Mort_30_Year,Crime,Nasdaq,Dow,price_change)

str(hsd_2)
summary(hsd_2)

###### LINEAR REGRESSION ##########

##TASK - ADD IN TEST (RMSE)

#Train on older data (2010-2020)
hsd_train <- hsd_2 %>% 
  filter(Year<=2020)

str(hsd_train)

#Test newer data (2021)
hsd_test <- hsd_2 %>% 
  filter(Year>2020)

str(hsd_test)

#Build Linear Regression Model
#Use Backwards Variable Selection by starting with all variables

price_prediction1 <- lm(price_change ~ . - price_change - Year, data = hsd_train)
summary(price_prediction1)

#Remove least significant variables
price_prediction2 <- lm(price_change ~ . - price_change - Year, data = hsd_train)
summary(price_prediction2)

price_prediction3 <- lm(price_change ~ . - price_change - Year, data = hsd_train)
summary(price_prediction3)

price_prediction4 <- lm(price_change ~ .  - price_change - Year, data = hsd_train)
summary(price_prediction4)

price_prediction5 <- lm(price_change ~ .  - price_change - Year, data = hsd_train)
summary(price_prediction5)

price_prediction6 <- lm(price_change ~ .  - price_change - Year, data = hsd_train)
summary(price_prediction6)

price_prediction7 <- lm(price_change ~ .  - price_change - Year, data = hsd_train)
summary(price_prediction7)

#Test Model
##HERE

###### LOGISTIC REGRESSION ##########

##TASK: ADD IN ROC AND AUC

#Create Binary Variable (1 for increase in price, 0 for decrease)
hsd_3 <- hsd_2 %>% 
  mutate(change_type=if_else(price_change>=0, 1, 0))

#Split data by year

#Train on older data (2010-2020)
hsd_train2 <- hsd_3 %>% 
  filter(Year<=2020)

str(hsd_train2)

#Test on newer data (2021)
hsd_test2 <- hsd_3 %>% 
  filter(Year>2020)

str(hsd_test2)

#Build Logistic Regression Model
#Use Backwards Variable Selection by starting with all variables
log_price1 <- glm(change_type ~ . -change_type - price_change - Year, data=hsd_train2)
summary(log_price1)

log_price2 <- glm(change_type ~ . -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price2)

log_price3 <- glm(change_type ~ . -CPI -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price3)

log_price4 <- glm(change_type ~ . -BitCoin -CPI -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price4)

log_price5 <- glm(change_type ~ . -Mort_30_Year -BitCoin -CPI -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price5)

log_price6 <- glm(change_type ~ . -Dow -Mort_30_Year -BitCoin -CPI -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price6)

log_price7 <- glm(change_type ~ . -Nasdaq -Dow -Mort_30_Year -BitCoin -CPI -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price7)

#Test Model
##HERE

###### REGRESSION TREE MODEL ##########

###### RANDOM FOREST MODEL ##########
