

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
library(ROSE)
library(randomForest)
library(ROCR)

set.seed(645)


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
  filter(Year<=2019)

str(hsd_train)

#Test newer data (2021)
hsd_test <- hsd_2 %>% 
  filter(Year>2019) %>% 
  drop_na()

str(hsd_test)


#Build Linear Regression Model
#Use Backwards Variable Selection by starting with all variables

price_prediction1 <- lm(price_change ~ . - price_change - Year, data = hsd_train)
summary(price_prediction1)

#Remove least significant variables
price_prediction2 <- lm(price_change ~ . -BitCoin - price_change - Year, data = hsd_train)
summary(price_prediction2)

price_prediction3 <- lm(price_change ~ . -Nasdaq -BitCoin - price_change - Year, data = hsd_train)
summary(price_prediction3)

price_prediction4 <- lm(price_change ~ .  -Crime -Nasdaq -BitCoin - price_change - Year, data = hsd_train)
summary(price_prediction4)

price_prediction5 <- lm(price_change ~ .  -Unemployment -Crime -Nasdaq -BitCoin  - price_change - Year, data = hsd_train)
summary(price_prediction5)

price_prediction6 <- lm(price_change ~ .  -CPI -Unemployment -Crime -Nasdaq -BitCoin  - price_change - Year, data = hsd_train)
summary(price_prediction6)

price_prediction7 <- lm(price_change ~ .  -Crime -Nasdaq -Dow -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction7)

price_prediction8 <- lm(price_change ~ .  -Dow -Crime -Nasdaq -Dow -BitCoin -Unemployment -GDP_Billions - price_change - Year, data = hsd_train)
summary(price_prediction8)

#Test Model
price_pred_test <- hsd_test %>% 
  mutate(predicted_price_lin =predict(price_prediction8, newdata=hsd_test))

rmse(price_pred_test, price_change, predicted_price_lin)

###### LOGISTIC REGRESSION ##########

#Create Binary Variable (1 for increase in price, 0 for decrease)
hsd_3 <- hsd_2 %>% 
#  mutate(change_type=if_else(price_change>=.7, 1, 0)) 
  mutate(change_type=if_else(price_change>=.25, 1, 0)) 

#Split data by year

#Train on older data (2010-2019)
hsd_train2 <- hsd_3 %>% 
  filter(Year<=2019)

str(hsd_train2)

#Test on newer data (2020-2021)
hsd_test2 <- hsd_3 %>% 
  filter(Year>2019) %>% 
  drop_na()

str(hsd_test2)

#Build Logistic Regression Model
#Use Backwards Variable Selection by starting with all variables and then comparing all to ROC
log_price1 <- glm(change_type ~ . -change_type - price_change - Year, data=hsd_train2)
summary(log_price1)

log_price2 <- glm(change_type ~ . -Nasdaq -change_type - price_change - Year, data=hsd_train2)
summary(log_price2)

log_price3 <- glm(change_type ~ . -CPI -Nasdaq  -change_type - price_change - Year, data=hsd_train2)
summary(log_price3)

log_price4 <- glm(change_type ~ . -Mort_30_Year -CPI -Nasdaq -change_type - price_change - Year, data=hsd_train2)
summary(log_price4)

log_price5 <- glm(change_type ~ . -Unemployment -Mort_30_Year -CPI -Nasdaq -change_type - price_change - Year, data=hsd_train2)
summary(log_price5)

log_price6 <- glm(change_type ~ . -Dow -Unemployment -Mort_30_Year -CPI -Nasdaq -change_type - price_change - Year, data=hsd_train2)
summary(log_price6)

log_price7 <- glm(change_type ~ . -BitCoin -Dow -Unemployment -Mort_30_Year -CPI -Nasdaq -change_type - price_change - Year, data=hsd_train2)
summary(log_price7)

#Test Model
hsd_test2 <- hsd_test2 %>%
  mutate(predict_change_type1 = predict(log_price1, newdata = hsd_test2, type = "response")) %>% 
  mutate(predict_change_type2 = predict(log_price2, newdata = hsd_test2, type = "response")) %>% 
  mutate(predict_change_type3 = predict(log_price3, newdata = hsd_test2, type = "response")) %>% 
  mutate(predict_change_type4 = predict(log_price4, newdata = hsd_test2, type = "response")) %>% 
  mutate(predict_change_type5 = predict(log_price5, newdata = hsd_test2, type = "response")) %>% 
  mutate(predict_change_type6 = predict(log_price6, newdata = hsd_test2, type = "response")) %>% 
  mutate(predict_change_type7 = predict(log_price7, newdata = hsd_test2, type = "response"))

roc1 <- roc(hsd_test2, x= predict_change_type1, class = change_type, pos_class = 0, neg_class = 1)
roc2 <- roc(hsd_test2, x= predict_change_type2, class = change_type, pos_class = 0, neg_class = 1)
roc3 <- roc(hsd_test2, x= predict_change_type3, class = change_type, pos_class = 0, neg_class = 1)
roc4 <- roc(hsd_test2, x= predict_change_type4, class = change_type, pos_class = 0, neg_class = 1)
roc5 <- roc(hsd_test2, x= predict_change_type5, class = change_type, pos_class = 0, neg_class = 1)
roc6 <- roc(hsd_test2, x= predict_change_type6, class = change_type, pos_class = 0, neg_class = 1)
roc7 <- roc(hsd_test2, x= predict_change_type7, class = change_type, pos_class = 0, neg_class = 1)

auc(roc1)
auc(roc2)
auc(roc3)
auc(roc4)
auc(roc5)
auc(roc6)
auc(roc7)

plot(roc1)
plot(roc2)
plot(roc3)
plot(roc4)
plot(roc5)
plot(roc6)
plot(roc7)

plot(roc6) + 
  geom_line(data = roc6, color = "red") +
  geom_abline(slope = 1) +
  labs(title = "ROC Curve for Logistic Regression Home Price Forecast Model")

# ###### REGRESSION TREE MODEL ##########
# 
###SARAH COMMENT - I DON'T THINK A REGRESSION TREE MODEL QUITE WORKS HERE AND I COMMENTED IT OUT


# #Regression Tree
# price_rtree <- rpart(price_change  ~ .  - price_change - Year, data=hsd_train, method="anova")
# rpart.plot(price_rtree, cex=0.8)
# 
# 
# 
# #Test Model
# ##HERE


#Classification Tree
price_ctree <- rpart(change_type  ~ . -change_type - price_change - Year, data=hsd_train2, method="class")
rpart.plot(price_ctree, cex=0.8)

change_train_over <-ovun.sample(change_type ~ . -price_change - change_type - Year, data=hsd_train2, method="over", p=0.5)$data
table(change_train_over$change_type)

price_ctree_over <- rpart(change_type  ~ . -change_type - price_change - Year, data=change_train_over, method="class")
rpart.plot(price_ctree, cex=0.8)

predicted_ctree <- price_ctree %>% 
  predict(newdata =hsd_test2, type = "prob")

predicted_ctree_over <- price_ctree_over %>% 
  predict(newdata=hsd_test2, type = "prob")

head(predicted_ctree)
head(predicted_ctree_over)

hsd_test2 <- hsd_test2 %>%  
  mutate(predicted_prob_ctree = predicted_ctree[, 2]) %>% 
  mutate(predicted_prob_ctree_over = predicted_ctree_over[, 2])

# ROC Curve and AUC
roc_ctree <- roc(hsd_test2, x = predicted_prob_ctree, class = change_type, 
                 pos_class = 1, neg_class = 0, direction = ">=")

roc_ctree_over<- roc(hsd_test2, x = predicted_prob_ctree_over, class = change_type, 
                     pos_class = 1, neg_class = 0, direction = ">=")
 
 
plot(roc_ctree) + 
  geom_line(data = roc_ctree, color = "red") +
  geom_abline(slope = 1) +
  labs(title = "ROC Curve for Classification Tree Home Price Forecast Model")

plot(roc_ctree_over) + 
  geom_line(data = roc_ctree_over, color = "red") +
  geom_abline(slope = 1) +
  labs(title = "ROC Curve for Oversample Classification Tree Home Price Forecast Model")


auc(roc_ctree)
auc(roc_ctree_over)

###### RANDOM FOREST MODEL ##########

#I Kind of broke it trying to fix the the log regression and 
#changing the change_type threshold. Toggle line 134/135 to get to work

price_randomForest <-  randomForest(as.factor(change_type) ~ . -price_change - Year , data = hsd_train2, ntree = 1000, importance=TRUE)
print(price_randomForest)
plot(price_randomForest)
importance(price_randomForest)
varImpPlot(price_randomForest)

pred1=predict(price_randomForest,type = "prob")
perf = prediction(pred1[,2], hsd_train2$change_type)

auc = performance(perf, measure="auc")@y.values[[1]]
auc

pred3 = performance(perf, "tpr","fpr")

plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")
## Refining the Random forest Model based on importance Variables

price_randomForest_refine <-  randomForest(as.factor(change_type) ~ . -BitCoin -Unemployment-Mort_30_Year - Crime-Population-GDP_Billions -price_change - Year , data = hsd_train2, ntree = 1000, importance=TRUE)
print(price_randomForest_refine)
plot(price_randomForest_refine)
importance(price_randomForest_refine)
varImpPlot(price_randomForest_refine)

pred2=predict(price_randomForest_refine,type = "prob")
perf2 = prediction(pred2[,2], hsd_train2$change_type)

auc2 = performance(perf2, measure="auc")@y.values[[1]]
auc2

pred4 = performance(perf2, "tpr","fpr")

plot(pred4,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

