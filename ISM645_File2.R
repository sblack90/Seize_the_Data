

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
  mutate(target_price=(`Median.House.Price.(1.Bedroom)`+`Median.House.Price.(2.Bedrooms)`+`Median.House.Price.(3.Bedrooms)`+ `Median.House.Price.(4.Bedrooms)`+`Median.House.Price.(5+.Bedrooms)`)/5)

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
price_prediction1 <- lm(target_price ~ Unemployment 15.Year + 30.Year + Crime.by.Month.(Don) + Nasdaq + Dow, 
                        data = hsd_train)
