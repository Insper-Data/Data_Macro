
# JANUARY 2021  

# Script to generate tables used in our paper

# Authors: Augusto Netto, Gabriela Garcia, Maria Clara Drzeviechi and Victor H. Alexandrino


#--------------------------------------------------------------------------------------------


# Libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(transformr)
library(stargazer)

# Calling our dataset
dataset_total_jan_2021 <- read.csv("https://raw.githubusercontent.com/Insper-Data/Data_Macro/master/Paper/dataset_total_jan_2021.csv")


#--------------------------------------------------------------------------------------------


# Table 1 - countries divided into development level:
countries_table_EM <- dataset_total_jan_2021 %>% 
  filter(develop == "EM") %>% 
  group_by(country) %>%
  distinct(country)

countries_table_AM <- dataset_total_jan_2021 %>% 
  filter(develop == "AM") %>% 
  group_by(country) %>%
  distinct(country)

# Table 2 - sum stats:
sum_stats_variables <- dataset_total_jan_2021 %>%
  select(-c(1, 2, 34:36, 41:47, 50:64)) # ommiting variables that do not appear in table 2

sum_stats <- stargazer(sum_stats_variables, title = "Summarized Statistics", omit.summary.stat = c("p25", "p75"))

