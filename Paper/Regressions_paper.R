
# JANUARY 2021  

# Script to generate regressions used in our paper

# Authors: Augusto Netto, Gabriela Garcia, Maria Clara Drzeviechi and Victor H. Alexandrino


#--------------------------------------------------------------------------------------------


# Libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(transformr)
library(stargazer)
library(plm)
library(lmtest)

# Calling our dataset
dataset_total_jan_2021 <- read.csv("https://raw.githubusercontent.com/Insper-Data/Data_Macro/master/Paper/Datasets/dataset_total_jan_2021.csv")


#--------------------------------------------------------------------------------------------


# Setting panel dataset
panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))

# Panel dataset for AM and lag added
panel_dataset_AM <- panel_dataset %>% 
  filter(develop == "AM",
         country != "United States") %>% 
  group_by(country) %>% 
  mutate(lag_debt_to_GDP = dplyr::lag(debt_to_GDP,1), 
         lag_ln_GDP_percapita_cur_USD = dplyr::lag(ln_GDP_percapita_cur_USD,1),
         lag_nominal_rate = dplyr::lag(nominal_rate,1),
         lag_inflation_end = dplyr::lag(inflation_end,1),
         lag_current_account_percent_GDP = dplyr::lag(current_account_percent_GDP,1),
         lag_fx_volatility = dplyr::lag(fx_volatility,1),
         lag_US_nominal_rate = dplyr::lag(US_nominal_rate,1)) 


# Panel dataset for EM and lag added
panel_dataset_EM <- panel_dataset %>% 
  filter(develop == "EM") %>% 
  group_by(country) %>% 
  mutate(lag_debt_to_GDP = dplyr::lag(debt_to_GDP,1), 
         lag_ln_GDP_percapita_cur_USD = dplyr::lag(ln_GDP_percapita_cur_USD,1),
         lag_nominal_rate = dplyr::lag(nominal_rate,1),
         lag_inflation_end = dplyr::lag(inflation_end,1),
         lag_current_account_percent_GDP = dplyr::lag(current_account_percent_GDP,1),
         lag_fx_volatility = dplyr::lag(fx_volatility,1),
         lag_US_nominal_rate = dplyr::lag(US_nominal_rate,1)) 


#--------------------------------------------------------------------------------------------
      # ADVANCED MARKETS
#--------------------------------------------------------------------------------------------

# Formula for dynamic model:
formula_AM <- foreign_ex_officials_participation_percent_GDP ~ lag_debt_to_GDP + lag_ln_GDP_percapita_cur_USD + lag_nominal_rate + lag_inflation_end + lag_current_account_percent_GDP + lag_fx_volatility + lag_US_nominal_rate + vix_EUA 


# Dynamic Panel regressions:
reg1.AM <- plm(formula_AM, data = panel_dataset_AM, model = "within", effect = "individual")
reg2.AM <- plm(formula_AM, data = panel_dataset_AM, model = "within", effect = "time")
reg3.AM <- plm(formula_AM, data = panel_dataset_AM, model = "within", effect = "twoways")
reg4.AM <- plm(formula_AM, data = panel_dataset_AM, model = "pooling")


# Clusterized errors:
reg1.AMc <- coeftest(reg1.AM, vcovHC.plm(reg1.AM, type="sss", cluster = "group", method = "white2"))[,2]
reg2.AMc <- coeftest(reg2.AM, vcovHC.plm(reg2.AM, type="sss", cluster="group", method = "white2"))[,2]
reg3.AMc <- coeftest(reg3.AM, vcovHC.plm(reg3.AM, type="sss", cluster="group", method = "white2"))[,2]
reg4.AMc <- coeftest(reg4.AM, vcovHC.plm(reg4.AM, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.AM, 
          reg2.AM, 
          reg3.AM, 
          reg4.AM,
          title = "Advanced Markets", type = "latex", style = "qje", 
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.AMc, reg2.AMc, reg3.AMc, reg4.AMc),
          omit.stat = "f",
          covariate.labels = c("Debt-to-GDP (t-1)", "ln(GDP per capita) (t-1)", "Nominal Interest (t-1)",
                               "Inflation (t-1)", "Current Account Balance (t-1)",
                               "FX Volatility (t-1)", "US Nominal Rate (t-1)", "VIX EUA"),
          dep.var.labels = c("Foreign Participation in Sovereign Debt (% GDP)"))

#--------------------------------------------------------------------------------------------
      # EMERGING MARKETS
#--------------------------------------------------------------------------------------------

# Formula:
formula_EM <- formula_EM_dynamic <- foreign_ex_officials_participation_percent_GDP ~ lag_debt_to_GDP + lag_ln_GDP_percapita_cur_USD + lag_nominal_rate + lag_inflation_end + lag_current_account_percent_GDP + lag_fx_volatility + lag_US_nominal_rate + vix_EUA 

# Panel regressions:
reg1.EM <- plm(formula_EM, data = panel_dataset_EM, model = "within", effect = "individual")
reg2.EM <- plm(formula_EM, data = panel_dataset_EM, model = "within", effect = "time")
reg3.EM <- plm(formula_EM, data = panel_dataset_EM, model = "within", effect = "twoways")
reg4.EM <- plm(formula_EM, data = panel_dataset_EM, model = "pooling")


# Clusterized errors:
reg1.EMc <- coeftest(reg1.EM, vcovHC.plm(reg1.EM, type="sss", cluster = "group", method = "white2"))[,2]
reg2.EMc <- coeftest(reg2.EM, vcovHC.plm(reg2.EM, type="sss", cluster="group", method = "white2"))[,2]
reg3.EMc <- coeftest(reg3.EM, vcovHC.plm(reg3.EM, type="sss", cluster="group", method = "white2"))[,2]
reg4.EMc <- coeftest(reg4.EM, vcovHC.plm(reg4.EM, type="sss", cluster="group", method = "white2"))[,2]

# Output for LaTeX:
stargazer(reg1.EM, 
          reg2.EM, 
          reg3.EM, 
          reg4.EM,
          title = "Emerging Markets", type = "latex", style = "qje",
          add.lines = list(c("Country FE", "Yes", "No", "Yes", "No"), c("Year FE", "No", "Yes", "Yes", "No")),
          se = list(reg1.EMc, reg2.EMc, reg3.EMc, reg4.EMc),
          omit.stat = "f",
          covariate.labels = c("Debt-to-GDP (t-1)", "ln(GDP per capita) (t-1)", "Nominal Interest (t-1)",
                               "Inflation (t-1)", "Current Account Balance (t-1)",
                               "FX Volatility (t-1)", "US Nominal Rate (t-1)", "VIX EUA"),
          dep.var.labels = c("Foreign Participation in Sovereign Debt (% GDP)"))

