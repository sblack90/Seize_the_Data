

library(ggrepel)
library(dplyr) 
library(ggplot2)
library(tidyr)
library(openxlsx)
library(reshape)
library(yardstick)
library(rpart)
library(rpart.plot)
library(cutpointr)
library(caret)


# Importing the raw data from xlsx file
house_series_data <- read.xlsx("PA Data Merge.xlsx")

###### EXPLORING THE DATA ##########

# Exploring the variables of the raw data
str(house_series_data)

# Review Variables vs Year

ggplot(data=house_series_data, aes(x=Year, y=CPI)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=BitCoin)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Unemployment)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=GDP_Billions)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Population)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Mort_15_Year)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Mort_30_Year)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Crime)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Nasdaq)) +
  geom_point()

ggplot(data=house_series_data, aes(x=Year, y=Dow)) +
  geom_point()


##Weird issue where log(BitCoin) gave me negatives, so I set it at 0 as the lowest
hsd_2 <- house_series_data %>% 
  mutate(median_price=(`Median_House_Price_1`+`Median_House_Price_2`+`Median_House_Price_3`+ `Median_House_Price_4`+`Median_House_Price_5`)/5, Mort_15_Year=as.numeric(Mort_15_Year)) %>%
  mutate(price_change=100*(lead(median_price)/median_price-1)) %>% 
  mutate(BitCoin=if_else(log(BitCoin)>0, log(BitCoin), 0))  
  
##Select 15 or 30 Year Mortgage
Mort_15 <- lm(price_change ~ Mort_15_Year, data=hsd_2)
Mort_30 <- lm(price_change ~ Mort_30_Year, data=hsd_2)
summary(Mort_15)
summary(Mort_30)

hsd_2 <- hsd_2 %>% 
  select(Year,CPI,BitCoin,Unemployment,GDP_Billions,Population,Mort_30_Year,Crime,Nasdaq,Dow,price_change)

str(hsd_2)
summary(hsd_2)

###### LINEAR REGRESSION ##########

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
price_prediction2 <- lm(price_change ~ . -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction2)

price_prediction3 <- lm(price_change ~ . -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction3)

price_prediction4 <- lm(price_change ~ .  -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction4)

price_prediction5 <- lm(price_change ~ .  -Dow -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction5)

price_prediction6 <- lm(price_change ~ .  -Nasdaq -Dow -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction6)

price_prediction7 <- lm(price_change ~ .  -Crime -Nasdaq -Dow -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction7)

price_prediction8 <- lm(price_change ~ .  -CPI -Crime -Nasdaq -Dow -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction8)

#Test Model
##HERE, started, RMSE very small
price_pred_test <- hsd_test %>% 
  mutate(predicted_price_lin =predict(price_prediction8, newdata=hsd_test))

rmse(price_pred_test, price_change, predicted_price_lin)

###### LOGISTIC REGRESSION ##########

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

log_price3 <- glm(change_type ~ . -BitCoin -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price3)

log_price4 <- glm(change_type ~ . -CPI -BitCoin -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price4)

log_price5 <- glm(change_type ~ . -Dow -CPI -BitCoin -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price5)

log_price6 <- glm(change_type ~ . -Nasdaq -Dow -CPI -BitCoin -Crime -change_type - price_change - Year, data=hsd_train2)
summary(log_price6)


#Test Model
##GOT A WEIRD ERROR, STILL REVIEWING 
##KC will work on this
hsd_test2 <- hsd_test2 %>%
  mutate(predict_change_type = predict(log_price6, newdata = hsd_test2, type = "response"))

roc <- roc(hsd_test2, x= predict_change_type, class = change_type, pos_class = 1, neg_class = 0, silent = TRUE)

plot(roc)
auc(roc)

plot(roc) + 
  geom_line(data = roc, color = "red") +
  geom_abline(slope = 1) +
  labs(title = "ROC Curve for log_price6")

###### REGRESSION TREE MODEL ##########

#Regression Tree
price_rtree <- rpart(price_change  ~ .  - price_change - Year, data=hsd_train, method="anova")
rpart.plot(price_rtree, cex=0.8)

#Test Model
##HERE

#Classification Tree
price_ctree <- rpart(change_type  ~ . -change_type - price_change - Year, data=hsd_train2, method="class")
rpart.plot(price_ctree, cex=0.8)

#Test Model
##HERE

###### RANDOM FOREST MODEL ##########

