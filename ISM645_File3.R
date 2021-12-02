##Combine Output

##From File1
####### CREATE A FAKE HOUSE TO ESTIMATE SALES PRICE

HOUSE <- data.frame(zip = 27410, bathrooms = 2.5, bedrooms = 3, area = 2000, yearBuilt = 1991) 

HOUSE_PREDICTIONS <- SALES_REGRESSION %>%
  predict(HOUSE)

HOUSE <- HOUSE %>%
  mutate(price = HOUSE_PREDICTIONS)

HOUSE


##From File2
##price_prediction7 is the linear regression on change in price



