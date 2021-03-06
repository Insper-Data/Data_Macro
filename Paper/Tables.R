
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
dataset_total_jan_2021 <- read.csv("https://raw.githubusercontent.com/Insper-Data/Data_Macro/master/Paper/Datasets/dataset_total_jan_2021.csv")


#--------------------------------------------------------------------------------------------


# Table 1 - countries divided into development level:
countries_table_EM <- dataset_total_jan_2021 %>% 
  filter(develop == "EM") %>% 
  group_by(country) %>%
  distinct(country) %>% 
  rename("Emerging Markets" = country)

countries_table_AM <- dataset_total_jan_2021 %>% 
  filter(develop == "AM") %>% 
  group_by(country) %>%
  distinct(country) %>% 
  rename("Advanced Markets" = country)

countries_table <- countries_table_AM %>% 
  left_join(countries_table_EM, by = row_number('Emerging Makets'))


# Table 2 - sum stats:
sum_stats_variables <- dataset_total_jan_2021 %>%
  select(c(debt_to_GDP,
           foreign_participation_percent_GDP,
           foreign_ex_officials_participation_percent_GDP,
           GDP_percapita_cur_USD, inflation_average,
           lending_borrowing_percent_GDP,
           nominal_rate, control_corruption_rank, political_stability_rank,
           rule_of_law_rank,
           current_account_percent_GDP,
           vix_EUA,
           dxy,
           fx_volatility)) %>%  # selecting variables to Table 2
  mutate(GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000)

sum_stats <- stargazer(sum_stats_variables, title = "Summarized Statistics", omit.summary.stat = c("p25", "p75"),
                       type = "latex",
                       covariate.labels = c("Debt-to-GDP (%)", 
                                            "Foreign Debt Participation (% of GDP)",
                                             "Foreign Part. Except Officials (% of GDP)",
                                             "GDP per capita (thousand USD)", 
                                             "Inflation (%)",
                                             "(+) Lending (-) Borrowing (% of GDP)",
                                             "Nominal Interest Rate (%)",
                                             "Control of Corruption",
                                             "Political Stability",
                                             "Rule of Law",
                                             "Current Account (% of GDP)",
                                             "US VIX",
                                             "DXY Index",
                                             "FX Volatility"))

