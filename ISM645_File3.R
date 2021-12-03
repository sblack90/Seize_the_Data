##Combine Output

##From File1
####### CREATE A FAKE HOUSE TO ESTIMATE SALES PRICE USING THE RANDOM FOREST MODEL WHICH IS THE BEST RMSE

HOUSE <- data.frame(zip = 27410, bathrooms = 2.5, bedrooms = 3, area = 2000, yearBuilt = 1991) 

HOUSE_PREDICTIONS <- SALES_RANDOM_FOREST %>%
  predict(HOUSE)

HOUSE <- HOUSE %>%
  mutate(price = HOUSE_PREDICTIONS)

HOUSE


##From File2
##price_prediction7 is the linear regression on change in price

macro_var <- data.frame(Year=2021, GDP_Billions=23187.042, Population=537174, Mort_30_Year=3.487)

forecast_prediction <- log_price7 %>% 
  predict(macro_var)

macro_var <- macro_var %>% 
  mutate(change_type = forecast_prediction)

macro_var
