#libraries
rm(list=ls())
library(tidyverse)
library(readr)
library(readxl)
library(spData)library(ggrepel)
library(cowplot)
library(gapminder)
library(plm)
library(stargazer)
library(lmtest)

#dados
dataset_total <- read.csv("dataset_total.csv")

#arrumando detalhes
panel_dataset <- plm.data(dataset_total, index=c("country", "year"))

panel_dataset <- panel_dataset %>% 
  mutate(for_part = (foreign_debt_/total_debt_))

panel_dataset <- panel_dataset %>% 
  mutate(for_ex_BC = ((nonbank_foreign_debt_ + bank_foreign_debt_)/total_debt_))

panel_dataset <- panel_dataset %>% 
  mutate(for_nonbank_prop = (nonbank_foreign_debt_/total_debt_))

panel_dataset <- panel_dataset %>% 
  mutate(develop = as.character(develop))

#Econometrics
#-Type 1 regression: total foreign debt as response variable for all countries in the database
#-Type 2 regression: total foreign debt excluding official debt as response variable for all countries in the database
#-Type 3 regression: total foreign debt as response variable for advanced markets
#-Type 4 regression: total foreign debt excluding official debt as response variable for advanced markets
#-Type 5 regression: total foreign debt as response variable for emerging markets
#-Type 6 regression: total foreign debt excluding official debt as response variable for emerging markets

##Type 1 regression
###Regression 1.1
f1.1 <-  for_part ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes + lending_GDP + account_GPD + unemployment + inflation_mean + develop


reg1.1.1 <- plm(f1.1, data = panel_dataset, model="within", effect="individual")

reg1.1.2 <- plm(f1.1, data = panel_dataset, model = "within", effect = "time")

reg1.1.3 <- plm(f1.1, data = panel_dataset, model = "within", effect = "twoways")

reg1.1.4 <- plm(f1.1, data = panel_dataset, model = "pooling")

summary( reg1.1.1)
summary( reg1.1.2)
summary( reg1.1.3)
summary( reg1.1.4)




coeftest(reg1.1.1, vcov=vcovHC(reg1.1.1, type="sss", cluster="group"))
coeftest(reg1.1.2, vcov=vcovHC(reg1.1.2, type="sss", cluster="group"))
coeftest(reg1.1.3, vcov=vcovHC(reg1.1.3, type="sss", cluster="group"))
coeftest(reg1.1.4, vcov=vcovHC(reg1.1.4, type="sss", cluster="group"))

stargazer(reg1.1.1)
stargazer(reg1.1.2)
stargazer(reg1.1.3)
stargazer(reg1.1.4)

##Regression 1.2
f1.2 <-  for_part ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes + lending_GDP +  unemployment + inflation_mean + develop

reg1.2.1 <- plm(f1.2, data = panel_dataset, model="within", effect="individual")

reg1.2.2 <- plm(f1.2, data = panel_dataset, model = "within", effect = "time")

reg1.2.3 <- plm(f1.2, data = panel_dataset, model = "within", effect = "twoways")

reg1.2.4 <- plm(f1.2, data = panel_dataset, model = "pooling")

summary( reg1.2.1)
summary( reg1.2.2)
summary( reg1.2.3)
summary( reg1.2.4)




coeftest(reg1.2.1, vcov=vcovHC(reg1.2.1, type="sss", cluster="group"))
coeftest(reg1.2.2, vcov=vcovHC(reg1.2.2, type="sss", cluster="group"))
coeftest(reg1.2.3, vcov=vcovHC(reg1.2.3, type="sss", cluster="group"))
coeftest(reg1.2.4, vcov=vcovHC(reg1.2.4, type="sss", cluster="group"))

stargazer(reg1.2.1)
stargazer(reg1.2.2)
stargazer(reg1.2.3)
stargazer(reg1.2.4)

###Regression 1.3
f1.3 <-  for_part ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUR + taxes + lending_GDP + account_GPD + unemployment + inflation_mean + develop


reg1.3.1 <- plm(f1.3, data = panel_dataset, model="within", effect="individual")

reg1.3.2 <- plm(f1.3, data = panel_dataset, model = "within", effect = "time")

reg1.3.3 <- plm(f1.3, data = panel_dataset, model = "within", effect = "twoways")

reg1.3.4 <- plm(f1.3, data = panel_dataset, model = "pooling")

summary( reg1.3.1)
summary( reg1.3.2)
summary( reg1.3.3)
summary( reg1.3.4)




coeftest(reg1.3.1, vcov=vcovHC(reg1.3.1, type="sss", cluster="group"))
coeftest(reg1.3.2, vcov=vcovHC(reg1.3.2, type="sss", cluster="group"))
coeftest(reg1.3.3, vcov=vcovHC(reg1.3.3, type="sss", cluster="group"))
coeftest(reg1.3.4, vcov=vcovHC(reg1.3.4, type="sss", cluster="group"))


stargazer(reg1.3.1)
stargazer(reg1.3.2)
stargazer(reg1.3.3)
stargazer(reg1.3.4)

###Regression 1.4
f1.4 <-  for_part ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes  + account_GPD + unemployment + inflation_mean + develop


reg1.4.1 <- plm(f1.4, data = panel_dataset, model="within", effect="individual")

reg1.4.2 <- plm(f1.4, data = panel_dataset, model = "within", effect = "time")

reg1.4.3 <- plm(f1.4, data = panel_dataset, model = "within", effect = "twoways")

reg1.4.4 <- plm(f1.4, data = panel_dataset, model = "pooling")

summary( reg1.4.1)
summary( reg1.4.2)
summary( reg1.4.3)
summary( reg1.4.4)




coeftest(reg1.4.1, vcov=vcovHC(reg1.4.1, type="sss", cluster="group"))
coeftest(reg1.4.2, vcov=vcovHC(reg1.4.2, type="sss", cluster="group"))
coeftest(reg1.4.3, vcov=vcovHC(reg1.4.3, type="sss", cluster="group"))
coeftest(reg1.4.4, vcov=vcovHC(reg1.4.4, type="sss", cluster="group"))


stargazer(reg1.4.1)
stargazer(reg1.4.2)
stargazer(reg1.4.3)
stargazer(reg1.4.4)

##Regression 1.5

f1.5 <-  for_part ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes + lending_GDP + account_GPD  + develop


reg1.5.1 <- plm(f1.5, data = panel_dataset, model="within", effect="individual")

reg1.5.2 <- plm(f1.5, data = panel_dataset, model = "within", effect = "time")

reg1.5.3 <- plm(f1.5, data = panel_dataset, model = "within", effect = "twoways")

reg1.5.4 <- plm(f1.5, data = panel_dataset, model = "pooling")

summary( reg1.5.1)
summary( reg1.5.2)
summary( reg1.5.3)
summary( reg1.5.4)



coeftest(reg1.5.1, vcov=vcovHC(reg1.5.1, type="sss", cluster="group"))
coeftest(reg1.5.2, vcov=vcovHC(reg1.5.2, type="sss", cluster="group"))
coeftest(reg1.5.3, vcov=vcovHC(reg1.5.3, type="sss", cluster="group"))
coeftest(reg1.5.4, vcov=vcovHC(reg1.5.4, type="sss", cluster="group"))

stargazer(reg1.5.1)
stargazer(reg1.5.2)
stargazer(reg1.5.3)
stargazer(reg1.5.4)

##Regression 1.6

f1.6 <-  for_part ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate  + taxes + lending_GDP + account_GPD + unemployment + inflation_mean + develop


reg1.6.1 <- plm(f1.6, data = panel_dataset, model="within", effect="individual")

reg1.6.2 <- plm(f1.6, data = panel_dataset, model = "within", effect = "time")

reg1.6.3 <- plm(f1.6, data = panel_dataset, model = "within", effect = "twoways")

reg1.6.4 <- plm(f1.6, data = panel_dataset, model = "pooling")

summary( reg1.6.1)
summary( reg1.6.2)
summary( reg1.6.3)
summary( reg1.6.4)




coeftest(reg1.6.1, vcov=vcovHC(reg1.6.1, type="sss", cluster="group"))
coeftest(reg1.6.2, vcov=vcovHC(reg1.6.2, type="sss", cluster="group"))
coeftest(reg1.6.3, vcov=vcovHC(reg1.6.3, type="sss", cluster="group"))
coeftest(reg1.6.4, vcov=vcovHC(reg1.6.4, type="sss", cluster="group"))


stargazer(reg1.6.1)
stargazer(reg1.6.2)
stargazer(reg1.6.3)
stargazer(reg1.6.4)

##Type 2 regression
###Regression 2.1
f2.1 <-  for_ex_BC ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes + lending_GDP + account_GPD + unemployment + inflation_mean + develop


reg2.1.1 <- plm(f2.1, data = panel_dataset, model="within", effect="individual")

reg2.1.2 <- plm(f2.1, data = panel_dataset, model = "within", effect = "time")

reg2.1.3 <- plm(f2.1, data = panel_dataset, model = "within", effect = "twoways")

reg2.1.4 <- plm(f2.1, data = panel_dataset, model = "pooling")

summary( reg2.1.1)
summary( reg2.1.2)
summary( reg2.1.3)
summary( reg2.1.4)




coeftest(reg2.1.1, vcov=vcovHC(reg2.1.1, type="sss", cluster="group"))
coeftest(reg2.1.2, vcov=vcovHC(reg2.1.2, type="sss", cluster="group"))
coeftest(reg2.1.3, vcov=vcovHC(reg2.1.3, type="sss", cluster="group"))
coeftest(reg2.1.4, vcov=vcovHC(reg2.1.4, type="sss", cluster="group"))

stargazer(reg2.1.1)
stargazer(reg2.1.2)
stargazer(reg2.1.3)
stargazer(reg2.1.4)

##Regression 2.2
f2.2 <-  for_ex_BC ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes + lending_GDP +  unemployment + inflation_mean + develop

reg2.2.1 <- plm(f2.2, data = panel_dataset, model="within", effect="individual")

reg2.2.2 <- plm(f2.2, data = panel_dataset, model = "within", effect = "time")

reg2.2.3 <- plm(f2.2, data = panel_dataset, model = "within", effect = "twoways")

reg2.2.4 <- plm(f2.2, data = panel_dataset, model = "pooling")

summary( reg2.2.1)
summary( reg2.2.2)
summary( reg2.2.3)
summary( reg2.2.4)




coeftest(reg2.2.1, vcov=vcovHC(reg2.2.1, type="sss", cluster="group"))
coeftest(reg2.2.2, vcov=vcovHC(reg2.2.2, type="sss", cluster="group"))
coeftest(reg2.2.3, vcov=vcovHC(reg2.2.3, type="sss", cluster="group"))
coeftest(reg2.2.4, vcov=vcovHC(reg2.2.4, type="sss", cluster="group"))


stargazer(reg2.2.1)
stargazer(reg2.2.2)
stargazer(reg2.2.3)
stargazer(reg2.2.4)

###Regression 2.3
f2.3 <-  for_ex_BC ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUR + taxes + lending_GDP + account_GPD + unemployment + inflation_mean + develop


reg2.3.1 <- plm(f2.3, data = panel_dataset, model="within", effect="individual")

reg2.3.2 <- plm(f2.3, data = panel_dataset, model = "within", effect = "time")

reg2.3.3 <- plm(f2.3, data = panel_dataset, model = "within", effect = "twoways")

reg2.3.4 <- plm(f2.3, data = panel_dataset, model = "pooling")

summary( reg2.3.1)
summary( reg2.3.2)
summary( reg2.3.3)
summary( reg2.3.4)




coeftest(reg2.3.1, vcov=vcovHC(reg2.3.1, type="sss", cluster="group"))
coeftest(reg2.3.2, vcov=vcovHC(reg2.3.2, type="sss", cluster="group"))
coeftest(reg2.3.3, vcov=vcovHC(reg2.3.3, type="sss", cluster="group"))
coeftest(reg2.3.4, vcov=vcovHC(reg2.3.4, type="sss", cluster="group"))

stargazer(reg2.3.1)
stargazer(reg2.3.2)
stargazer(reg2.3.3)
stargazer(reg2.3.4)

###Regression 2.4
f2.4 <-  for_ex_BC ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes  + account_GPD + unemployment + inflation_mean + develop


reg2.4.1 <- plm(f2.4, data = panel_dataset, model="within", effect="individual")

reg2.4.2 <- plm(f2.4, data = panel_dataset, model = "within", effect = "time")

reg2.4.3 <- plm(f2.4, data = panel_dataset, model = "within", effect = "twoways")

reg2.4.4 <- plm(f2.4, data = panel_dataset, model = "pooling")

summary( reg2.4.1)
summary( reg2.4.2)
summary( reg2.4.3)
summary( reg2.4.4)




coeftest(reg2.4.1, vcov=vcovHC(reg2.4.1, type="sss", cluster="group"))
coeftest(reg2.4.2, vcov=vcovHC(reg2.4.2, type="sss", cluster="group"))
coeftest(reg2.4.3, vcov=vcovHC(reg2.4.3, type="sss", cluster="group"))
coeftest(reg2.4.4, vcov=vcovHC(reg2.4.4, type="sss", cluster="group"))

stargazer(reg2.4.1)
stargazer(reg2.4.2)
stargazer(reg2.4.3)
stargazer(reg2.4.4)

##Regression 2.5

f2.5 <-  for_ex_BC ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate + vix_EUA + taxes + lending_GDP + account_GPD  + develop


reg2.5.1 <- plm(f2.5, data = panel_dataset, model="within", effect="individual")

reg2.5.2 <- plm(f2.5, data = panel_dataset, model = "within", effect = "time")

reg2.5.3 <- plm(f2.5, data = panel_dataset, model = "within", effect = "twoways")

reg2.5.4 <- plm(f2.5, data = panel_dataset, model = "pooling")

summary( reg2.5.1)
summary( reg2.5.2)
summary( reg2.5.3)
summary( reg2.5.4)



coeftest(reg2.5.1, vcov=vcovHC(reg2.5.1, type="sss", cluster="group"))
coeftest(reg2.5.2, vcov=vcovHC(reg2.5.2, type="sss", cluster="group"))
coeftest(reg2.5.3, vcov=vcovHC(reg2.5.3, type="sss", cluster="group"))
coeftest(reg2.5.4, vcov=vcovHC(reg2.5.4, type="sss", cluster="group"))

stargazer(reg2.5.1)
stargazer(reg2.5.2)
stargazer(reg2.5.3)
stargazer(reg2.5.4)

##Regression 2.6

f2.6 <-  for_ex_BC ~  debt_to_GDP_ + fx_ + ln_GDP_per_cap_cte + nominal_rate  + taxes + lending_GDP + account_GPD + unemployment + inflation_mean + develop


reg2.6.1 <- plm(f2.6, data = panel_dataset, model="within", effect="individual")

reg2.6.2 <- plm(f2.6, data = panel_dataset, model = "within", effect = "time")

reg2.6.3 <- plm(f2.6, data = panel_dataset, model = "within", effect = "twoways")

reg2.6.4 <- plm(f2.6, data = panel_dataset, model = "pooling")

summary( reg2.6.1)
summary( reg2.6.2)
summary( reg2.6.3)
summary( reg2.6.4)




coeftest(reg2.6.1, vcov=vcovHC(reg2.6.1, type="sss", cluster="group"))
coeftest(reg2.6.2, vcov=vcovHC(reg2.6.2, type="sss", cluster="group"))
coeftest(reg2.6.3, vcov=vcovHC(reg2.6.3, type="sss", cluster="group"))
coeftest(reg2.6.4, vcov=vcovHC(reg2.6.4, type="sss", cluster="group"))

stargazer(reg2.5.1)
stargazer(reg2.5.2)
stargazer(reg2.5.3)
stargazer(reg2.5.4)
