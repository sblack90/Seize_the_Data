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
##test using log reg. Note that it was weird if I didn't put all var in (even though prediction is only on the first line)
#So I was lazy and set them to zero rather than research the ones that aren't independent vars

macro_var <- data.frame(GDP_Billions=23187.042, Population=546308, Mort_30_Year=3.487,
                         Year=2021, Month="December", CPI=0, BitCoin=log(53113.40), Unemployment=0, Crime=2420, Nasdaq=0, Dow=0,
                        price_change=0)

forecast_prediction <- log_price6 %>% 
  predict(macro_var)

macro_var <- macro_var %>% 
  mutate(change_type = forecast_prediction)

macro_var
