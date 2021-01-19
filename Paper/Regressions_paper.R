
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
dataset_total_jan_2021 <- read.csv("https://raw.githubusercontent.com/Insper-Data/Data_Macro/master/Paper/dataset_total_jan_2021.csv")


#--------------------------------------------------------------------------------------------


# Setting panel dataset
panel_dataset <- pdata.frame(dataset_total_jan_2021, index = c("country", "year"))

# Panel dataset for AM
panel_dataset_AM <- panel_dataset %>% 
  filter(develop == "AM",
         country != "United States")

# Panel dataset for EM
panel_dataset_EM <- panel_dataset %>% 
  filter(develop == "EM")


#--------------------------------------------------------------------------------------------
      # ADVANCED MARKETS
#--------------------------------------------------------------------------------------------

# Formula:
formula_AM <- foreign_ex_officials_participation_percent_GDP ~ debt_to_GDP + ln_GDP_percapita_cur_USD + nominal_rate + inflation_end +  current_account_percent_GDP + fx_volatility + vix_EUA + US_nominal_rate

# Panel regressions:
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
          title = "Advanced Markets", type = "latex", 
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          se = list(reg1.AMc, reg2.AMc, reg3.AMc, reg4.AMc),
          omit.stat = "f",
           covariate.labels = c("Debt-to-GDP", "ln(GDP per capita)", "Nominal Interest", "Inflation", "Current Account Balance",
                                "FX Volatility", "VIX EUA", "US Nominal Rate"),
          dep.var.labels = c("Foreign participation on public debt divided by GDP"))


#--------------------------------------------------------------------------------------------
      # EMERGING MARKETS
#--------------------------------------------------------------------------------------------

# Formula:
formula_EM <- foreign_ex_officials_participation_percent_GDP ~ debt_to_GDP + ln_GDP_percapita_cur_USD + nominal_rate + inflation_end +  current_account_percent_GDP + fx_volatility + vix_EUA + US_nominal_rate

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
          title = "Emerging Markets", type = "latex", 
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          se = list(reg1.EMc, reg2.EMc, reg3.EMc, reg4.EMc),
          omit.stat = "f",
          covariate.labels = c("Debt-to-GDP", "ln(GDP per capita)", "Nominal Interest",
                               "Inflation", "Current Account Balance",
                               "FX Volatility", "VIX EUA", "US Nominal Rate"),
          dep.var.labels = c("Foreign participation on public debt divided by GDP"))

