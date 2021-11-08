

library(ggrepel)
library(dplyr) 
library(ggplot2)
library(tidyr)
library(openxlsx)
library(reshape)

# Importing the raw data from csv file
house_series_data <- read.csv("PA Data Merge.csv")

###### EXPLORING THE DATA ##########

# Exploring the variables of the raw data
str(house_series_data)

# Cleaning the data and selecting the relevant variables
hsd_2 <- house_series_data %>% 
  select()

str(hsd_2)  
