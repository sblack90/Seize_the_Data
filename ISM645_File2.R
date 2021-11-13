

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
  mutate(target_price=(`Median_House_Price_1`+`Median_House_Price_2`+`Median_House_Price_3`+ `Median_House_Price_4`+`Median_House_Price_5`)/5, Mort_15_Year=as.numeric(Mort_15_Year)) %>%
  select(CPI,BitCoin,Unemployment,GDP_Billions,Population,Mort_15_Year,Mort_30_Year,Crime,Nasdaq,Dow,target_price)

str(hsd_2)

#Split data

#Train on older data (2010-2020)
hsd_train <- hsd_2 %>% 
  filter(Year<=2020)

str(hsd_train)

#Test newer data (2021)
hsd_test <- hsd_2 %>% 
  filter(Year>2020)

str(hsd_test)

######Need to mutate variables to add lag?

#Build Linear Regression Model
price_prediction1 <- lm(target_price ~ . - target_price, data = hsd_train)

summary(price_prediction1)
                       
