#libraries
rm(list=ls())
library(tidyverse)
library(readr)
library(readxl)
library(cowplot)
library(gapminder)
library(plm)
library(stargazer)
library(lmtest)

#dados
dataset_total <- read.csv("dataset_total.csv")


#arrumando detalhes
panel_dataset <- pdata.frame(dataset_total, index =c("country", "year"))

panel_dataset <- panel_dataset %>% 
  mutate(for_part = (foreign_debt/total_debt), for_ex_BC = ((nonbank_foreign_debt + bank_foreign_debt)/total_debt), for_nonbank_prop = (nonbank_foreign_debt/total_debt), develop = as.character(develop))


#panel dataset_AM
panel_dataset_AM <- panel_dataset %>% 
  filter(develop == "AM")

#panel dataset_EM
panel_dataset_EM <- panel_dataset %>% 
  filter(develop == "EM") 



#Econometrics
#-Type 1 regression: total foreign debt as response variable for all countries in the database
#-Type 2 regression: total foreign debt excluding official debt as response variable for all countries in the database
#-Type 3 regression: total foreign debt as response variable for advanced markets
#-Type 4 regression: total foreign debt excluding official debt as response variable for advanced markets
#-Type 5 regression: total foreign debt as response variable for emerging markets
#-Type 6 regression: total foreign debt excluding official debt as response variable for emerging markets

##Type 1 regression
###Regression 1.1 (tudo)


f1.1 <-  for_part ~  debt_to_GDP + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA + account_balance + unemployment + inflation_mean + develop + post_08 + fx_volatility


reg1.1.1 <- plm(f1.1, data = panel_dataset, model="within", effect = "individual")

reg1.1.2 <- plm(f1.1, data = panel_dataset, model = "within", effect = "time")

reg1.1.3 <- plm(f1.1, data = panel_dataset, model = "within", effect = "twoways")

reg1.1.4 <- plm(f1.1, data = panel_dataset, model = "pooling")



reg1.1.1c <- coeftest(reg1.1.1, vcov=vcovHC(reg1.1.1, type="sss", cluster="group", method="white2"))
reg1.1.2c <- coeftest(reg1.1.2, vcov=vcovHC(reg1.1.2, type="sss", cluster="group", method="white2"))
reg1.1.3c <- coeftest(reg1.1.3, vcov=vcovHC(reg1.1.3, type="sss", cluster="group", method="white2"))
reg1.1.4c <- coeftest(reg1.1.4, vcov=vcovHC(reg1.1.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.1.1c, reg1.1.2c, reg1.1.3c, reg1.1.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

##Type 1 regression
###Regression 1.2 
f1.2 <-  for_part ~  debt_to_GDP + develop 

reg1.2.1 <- plm(f1.2, data = panel_dataset, model="within", effect="individual")

reg1.2.2 <- plm(f1.2, data = panel_dataset, model = "within", effect = "time")

reg1.2.3 <- plm(f1.2, data = panel_dataset, model = "within", effect = "twoways")

reg1.2.4 <- plm(f1.2, data = panel_dataset, model = "pooling")



reg1.2.1c <- coeftest(reg1.2.1, vcov=vcovHC(reg1.2.1, type="sss", cluster="group", method="white2"))
reg1.2.2c <- coeftest(reg1.2.2, vcov=vcovHC(reg1.2.2, type="sss", cluster="group", method="white2"))
reg1.2.3c <- coeftest(reg1.2.3, vcov=vcovHC(reg1.2.3, type="sss", cluster="group", method="white2"))
reg1.2.4c <- coeftest(reg1.2.4, vcov=vcovHC(reg1.2.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.2.1c, reg1.2.2c, reg1.2.3c, reg1.2.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 1 regression
###Regression 1.3 
f1.3 <-  for_part ~  debt_to_GDP + debt_to_GDP*develop + vix_EUA +  account_balance + develop


reg1.3.1 <- plm(f1.3, data = panel_dataset, model="within", effect="individual")

reg1.3.2 <- plm(f1.3, data = panel_dataset, model = "within", effect = "time")

reg1.3.3 <- plm(f1.3, data = panel_dataset, model = "within", effect = "twoways")

reg1.3.4 <- plm(f1.3, data = panel_dataset, model = "pooling")



reg1.3.1c <- coeftest(reg1.3.1, vcov=vcovHC(reg1.3.1, type="sss", cluster="group", method="white2"))
reg1.3.2c <- coeftest(reg1.3.2, vcov=vcovHC(reg1.3.2, type="sss", cluster="group", method="white2"))
reg1.3.3c <- coeftest(reg1.3.3, vcov=vcovHC(reg1.3.3, type="sss", cluster="group", method="white2"))
reg1.3.4c <- coeftest(reg1.3.4, vcov=vcovHC(reg1.3.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.3.1c, reg1.3.2c, reg1.3.3c, reg1.3.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 1 regression
###Regression 1.4 ()
f1.4 <-  for_part ~  debt_to_GDP  + ln_GDP_per_cap_cte + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA  + nominal_rate +  inflation_mean + develop + post_08


reg1.4.1 <- plm(f1.4, data = panel_dataset, model="within", effect="individual")

reg1.4.2 <- plm(f1.4, data = panel_dataset, model = "within", effect = "time")

reg1.4.3 <- plm(f1.4, data = panel_dataset, model = "within", effect = "twoways")

reg1.4.4 <- plm(f1.4, data = panel_dataset, model = "pooling")



reg1.4.1c <- coeftest(reg1.4.1, vcov=vcovHC(reg1.4.1, type="sss", cluster="group", method="white2"))
reg1.4.2c <- coeftest(reg1.4.2, vcov=vcovHC(reg1.4.2, type="sss", cluster="group", method="white2"))
reg1.4.3c <- coeftest(reg1.4.3, vcov=vcovHC(reg1.4.3, type="sss", cluster="group", method="white2"))
reg1.4.4c <- coeftest(reg1.4.4, vcov=vcovHC(reg1.4.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.4.1c, reg1.4.2c, reg1.4.3c, reg1.4.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 1 regression
###Regression 1.5 ()
f1.5 <-  for_part ~  debt_to_GDP + fx + ln_GDP_per_cap_cte + debt_to_GDP*develop  + vix_EUA  + nominal_rate + lending_borroeing_rate + account_balance + inflation_mean + develop 


reg1.5.1 <- plm(f1.5, data = panel_dataset, model="within", effect="individual")

reg1.5.2 <- plm(f1.5, data = panel_dataset, model = "within", effect = "time")

reg1.5.3 <- plm(f1.5, data = panel_dataset, model = "within", effect = "twoways")

reg1.5.4 <- plm(f1.5, data = panel_dataset, model = "pooling")



reg1.5.1c <- coeftest(reg1.5.1, vcov=vcovHC(reg1.5.1, type="sss", cluster="group", method="white2"))
reg1.5.2c <- coeftest(reg1.5.2, vcov=vcovHC(reg1.5.2, type="sss", cluster="group", method="white2"))
reg1.5.3c <- coeftest(reg1.5.3, vcov=vcovHC(reg1.5.3, type="sss", cluster="group", method="white2"))
reg1.5.4c <- coeftest(reg1.5.4, vcov=vcovHC(reg1.5.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.5.1c, reg1.5.2c, reg1.5.3c, reg1.5.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))
##################################################################################################################################################################################################################


##Type 2 regression
###Regression 2.1 (todos)
f2.1 <-  for_ex_BC ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cte + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA + taxes + lending_borroeing_rate + account_balance + unemployment + inflation_mean + develop + post_08


reg2.1.1 <- plm(f2.1, data = panel_dataset, model="within", effect="individual")

reg2.1.2 <- plm(f2.1, data = panel_dataset, model = "within", effect = "time")

reg2.1.3 <- plm(f2.1, data = panel_dataset, model = "within", effect = "twoways")

reg2.1.4 <- plm(f2.1, data = panel_dataset, model = "pooling")



reg2.1.1c <- coeftest(reg2.1.1, vcov=vcovHC(reg2.1.1, type="sss", cluster="group", method="white2"))
reg2.1.2c <- coeftest(reg2.1.2, vcov=vcovHC(reg2.1.2, type="sss", cluster="group", method="white2"))
reg2.1.3c <- coeftest(reg2.1.3, vcov=vcovHC(reg2.1.3, type="sss", cluster="group", method="white2"))
reg2.1.4c <- coeftest(reg2.1.4, vcov=vcovHC(reg2.1.4, type="sss", cluster="group", method="white2"))

stargazer(reg2.1.1c, reg2.1.2c, reg2.1.3c, reg2.1.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

##Type 2 regression
###Regression 2.2 ()
f2.2 <-  for_part ~  debt_to_GDP + develop 

reg2.2.1 <- plm(f2.2, data = panel_dataset, model="within", effect="individual")

reg2.2.2 <- plm(f2.2, data = panel_dataset, model = "within", effect = "time")

reg2.2.3 <- plm(f2.2, data = panel_dataset, model = "within", effect = "twoways")

reg2.2.4 <- plm(f2.2, data = panel_dataset, model = "pooling")



reg2.2.1c <- coeftest(reg2.2.1, vcov=vcovHC(reg2.2.1, type="sss", cluster="group", method="white2"))
reg2.2.2c <- coeftest(reg2.2.2, vcov=vcovHC(reg2.2.2, type="sss", cluster="group", method="white2"))
reg2.2.3c <- coeftest(reg2.2.3, vcov=vcovHC(reg2.2.3, type="sss", cluster="group", method="white2"))
reg2.2.4c <- coeftest(reg2.2.4, vcov=vcovHC(reg2.2.4, type="sss", cluster="group", method="white2"))

stargazer(reg2.2.1c, reg2.2.2c, reg2.2.3c, reg2.2.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 2 regression
###Regression 2.3 (todos os regressores tirando o "post_08" e com uma interação)
f2.3 <-  for_part ~  debt_to_GDP + debt_to_GDP*develop + vix_EUA +  account_balance + develop


reg2.3.1 <- plm(f2.3, data = panel_dataset, model="within", effect="individual")

reg2.3.2 <- plm(f2.3, data = panel_dataset, model = "within", effect = "time")

reg2.3.3 <- plm(f2.3, data = panel_dataset, model = "within", effect = "twoways")

reg2.3.4 <- plm(f2.3, data = panel_dataset, model = "pooling")



reg2.3.1c <- coeftest(reg2.3.1, vcov=vcovHC(reg2.3.1, type="sss", cluster="group", method="white2"))
reg2.3.2c <- coeftest(reg2.3.2, vcov=vcovHC(reg2.3.2, type="sss", cluster="group", method="white2"))
reg2.3.3c <- coeftest(reg2.3.3, vcov=vcovHC(reg2.3.3, type="sss", cluster="group", method="white2"))
reg2.3.4c <- coeftest(reg2.3.4, vcov=vcovHC(reg2.3.4, type="sss", cluster="group", method="white2"))

stargazer(reg2.3.1c, reg2.3.2c, reg2.3.3c, reg2.3.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 2 regression
###Regression 2.4 (todos os regressores e duas interações e com a "nominal rate" (mas dropando algumas observações))
f2.4 <-  for_part ~  debt_to_GDP  + ln_GDP_per_cap_cte + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA  + nominal_rate +  inflation_mean + develop + post_08


reg2.4.1 <- plm(f2.4, data = panel_dataset, model="within", effect="individual")

reg2.4.2 <- plm(f2.4, data = panel_dataset, model = "within", effect = "time")

reg2.4.3 <- plm(f2.4, data = panel_dataset, model = "within", effect = "twoways")

reg2.4.4 <- plm(f2.4, data = panel_dataset, model = "pooling")



reg2.4.1c <- coeftest(reg2.4.1, vcov=vcovHC(reg2.4.1, type="sss", cluster="group", method="white2"))
reg2.4.2c <- coeftest(reg2.4.2, vcov=vcovHC(reg2.4.2, type="sss", cluster="group", method="white2"))
reg2.4.3c <- coeftest(reg2.4.3, vcov=vcovHC(reg2.4.3, type="sss", cluster="group", method="white2"))
reg2.4.4c <- coeftest(reg2.4.4, vcov=vcovHC(reg2.4.4, type="sss", cluster="group", method="white2"))

stargazer(reg2.4.1c, reg2.4.2c, reg2.4.3c, reg2.4.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 2 regression
###Regression 2.5 (todos os regressores tirando o "post-08"; duas interações; com a "nominal rate" (mas dropando algumas observações))
f2.5 <- for_part ~  debt_to_GDP + fx + ln_GDP_per_cap_cte + debt_to_GDP*develop  + vix_EUA  + nominal_rate + lending_borroeing_rate + account_balance + inflation_mean + develop 



reg2.5.1 <- plm(f2.5, data = panel_dataset, model="within", effect="individual")

reg2.5.2 <- plm(f2.5, data = panel_dataset, model = "within", effect = "time")

reg2.5.3 <- plm(f2.5, data = panel_dataset, model = "within", effect = "twoways")

reg2.5.4 <- plm(f2.5, data = panel_dataset, model = "pooling")



reg2.5.1c <- coeftest(reg2.5.1, vcov=vcovHC(reg2.5.1, type="sss", cluster="group", method="white2"))
reg2.5.2c <- coeftest(reg2.5.2, vcov=vcovHC(reg2.5.2, type="sss", cluster="group", method="white2"))
reg2.5.3c <- coeftest(reg2.5.3, vcov=vcovHC(reg2.5.3, type="sss", cluster="group", method="white2"))
reg2.5.4c <- coeftest(reg2.5.4, vcov=vcovHC(reg2.5.4, type="sss", cluster="group", method="white2"))

stargazer(reg2.5.1c, reg2.5.2c, reg2.5.3c, reg2.5.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

########################################################################################################################################################################################################################3
##Type 3 regression
#Regression 3.1

f3.1 <- for_part ~  debt_to_GDP + fx + ln_GDP_per_cap_cte + inflation_mean +  nominal_rate + vix_EUA + taxes + account_GPD + lending_borroeing_rate + unemployment


reg3.1.1 <- plm(f3.1, data = panel_dataset_AM, model = "within", effect = "individual")

reg3.1.2 <- plm(f3.1, data = panel_dataset_AM, model = "within", effect = "time")

reg3.1.3 <- plm(f3.1, data = panel_dataset_AM, model = "within", effect = "twoways")

reg3.1.4 <- plm(f3.1, data = panel_dataset_AM, model = "pooling")

# Clustering:
reg3.1.1c <- coeftest(reg3.1.1, vcovHC.plm(reg3.1.1, type="sss", cluster = "group", method = "white2"))

reg3.1.2c <- coeftest(reg3.1.2, vcovHC.plm(reg3.1.2, type="sss", cluster="group", method = "white2"))

reg3.1.3c <- coeftest(reg3.1.3, vcovHC.plm(reg3.1.3, type="sss", cluster="group", method = "white2"))

reg3.1.4c <- coeftest(reg3.1.4, vcovHC.plm(reg3.1.4, type="sss", cluster="group", method = "white2"))

stargazer(reg3.1.1c, reg3.1.2c, reg3.1.3c, reg3.1.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

##Type 3 regression
#Regression 3.2
f3.2 <- for_part ~  debt_to_GDP +  nominal_rate

### RegressÃµes com resposta AM divida estrangeira completa:
reg3.2.1 <- plm(f3.2, data = panel_dataset_AM, model = "within", effect = "individual")

reg3.2.2 <- plm(f3.2, data = panel_dataset_AM, model = "within", effect = "time")

reg3.2.3 <- plm(f3.2, data = panel_dataset_AM, model = "within", effect = "twoways")

reg3.2.4 <- plm(f3.2, data = panel_dataset_AM, model = "pooling")

# Clustering:
reg3.2.1c <- coeftest(reg3.2.1, vcovHC.plm(reg3.2.1, type="sss", cluster = "group", method = "white2"))

reg3.2.2c <- coeftest(reg3.2.2, vcovHC.plm(reg3.2.2, type="sss", cluster="group", method = "white2"))

reg3.2.3c <- coeftest(reg3.2.3, vcovHC.plm(reg3.2.3, type="sss", cluster="group", method = "white2"))

reg3.2.4c <- coeftest(reg3.2.4, vcovHC.plm(reg3.2.4, type="sss", cluster="group", method = "white2"))

stargazer(reg3.2.1c, reg3.2.2c, reg3.2.3c, reg3.2.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))



##Type 3 regression
#Regression 3.3
f3.3 <- for_part ~  debt_to_GDP +  nominal_rate + inflation_mean

### RegressÃµes com resposta AM divida estrangeira completa:

reg3.3.1 <- plm(f3.3, data = panel_dataset_AM, model = "within", effect = "individual")

reg3.3.2 <- plm(f3.3, data = panel_dataset_AM, model = "within", effect = "time")

reg3.3.3 <- plm(f3.3, data = panel_dataset_AM, model = "within", effect = "twoways")

reg3.3.4 <- plm(f3.3, data = panel_dataset_AM, model = "pooling")

# Clustering:

reg3.3.1c <- coeftest(reg3.3.1, vcovHC.plm(reg3.3.1, type="sss", cluster = "group", method = "white2"))

reg3.3.2c <- coeftest(reg3.3.2, vcovHC.plm(reg3.3.2, type="sss", cluster="group", method = "white2"))

reg3.3.3c <- coeftest(reg3.3.3, vcovHC.plm(reg3.3.3, type="sss", cluster="group", method = "white2"))

reg3.3.4c <- coeftest(reg3.3.4, vcovHC.plm(reg3.3.4, type="sss", cluster="group", method = "white2"))

stargazer(reg3.3.1c, reg3.3.2c, reg3.3.3c, reg3.3.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


##Type 3 regression
#Regression 3.4
f3.4 <- for_part ~  debt_to_GDP +  nominal_rate + inflation_mean + lending_borroeing_rate + account_GPD

### RegressÃµes com resposta AM divida estrangeira completa:

reg3.4.1 <- plm(f3.4, data = panel_dataset_AM, model = "within", effect = "individual")

reg3.4.2 <- plm(f3.4, data = panel_dataset_AM, model = "within", effect = "time")

reg3.4.3 <- plm(f3.4, data = panel_dataset_AM, model = "within", effect = "twoways")

reg3.4.4 <- plm(f3.4, data = panel_dataset_AM, model = "pooling")

# Clustering:

reg3.4.1c <- coeftest(reg3.4.1, vcovHC.plm(reg3.4.1, type="sss", cluster = "group", method = "white2"))

reg3.4.2c <- coeftest(reg3.4.2, vcovHC.plm(reg3.4.2, type="sss", cluster="group", method = "white2"))

reg3.4.3c <- coeftest(reg3.4.3, vcovHC.plm(reg3.4.3, type="sss", cluster="group", method = "white2"))

reg3.4.4c <- coeftest(reg3.4.4, vcovHC.plm(reg3.4.4, type="sss", cluster="group", method = "white2"))

stargazer(reg3.4.1c, reg3.4.2c, reg3.4.3c, reg3.4.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))




##Type 4 regression
#Regression 4.1
f4.1 <- for_ex_BC ~  debt_to_GDP + fx + ln_GDP_per_cap_cte + inflation_mean +  nominal_rate + vix_EUA + taxes + account_GPD + lending_borroeing_rate + unemployment

### RegressÃµes com resposta AM divida estrangeira completa:

reg4.1.1 <- plm(f4.1, data = panel_dataset_AM, model = "within", effect = "individual")

reg4.1.2 <- plm(f4.1, data = panel_dataset_AM, model = "within", effect = "time")

reg4.1.3 <- plm(f4.1, data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.1.4 <- plm(f4.1, data = panel_dataset_AM, model = "pooling")

# Clustering:

reg4.1.1c <- coeftest(reg4.1.1, vcovHC.plm(reg4.1.1, type="sss", cluster = "group", method = "white2"))

reg4.1.2c <- coeftest(reg4.1.2, vcovHC.plm(reg4.1.2, type="sss", cluster="group", method = "white2"))

reg4.1.3c <- coeftest(reg4.1.3, vcovHC.plm(reg4.1.3, type="sss", cluster="group", method = "white2"))

reg4.1.4c <- coeftest(reg4.1.4, vcovHC.plm(reg4.1.4, type="sss", cluster="group", method = "white2"))

stargazer(reg4.1.1c, reg4.1.2c, reg4.1.3c, reg4.1.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


############################################################################################################################


##Type 4 regression
#Regression 4.2
f4.2 <- for_ex_BC ~  debt_to_GDP + nominal_rate

### RegressÃµes com resposta AM divida estrangeira completa:

reg4.2.1 <- plm(f4.2, data = panel_dataset_AM, model = "within", effect = "individual")

reg4.2.2 <- plm(f4.2, data = panel_dataset_AM, model = "within", effect = "time")

reg4.2.3 <- plm(f4.2, data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.2.4 <- plm(f4.2, data = panel_dataset_AM, model = "pooling")

# Clustering:

reg4.2.1c <- coeftest(reg4.2.1, vcovHC.plm(reg4.2.1, type="sss", cluster = "group", method = "white2"))

reg4.2.2c <- coeftest(reg4.2.2, vcovHC.plm(reg4.2.2, type="sss", cluster="group", method = "white2"))

reg4.2.3c <- coeftest(reg4.2.3, vcovHC.plm(reg4.2.3, type="sss", cluster="group", method = "white2"))

reg4.2.4c <- coeftest(reg4.2.4, vcovHC.plm(reg4.2.4, type="sss", cluster="group", method = "white2"))

stargazer(reg4.2.1c, reg4.2.2c, reg4.2.3c, reg4.2.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


############################################################################################################################

##Type 4 regression
#Regression 4.3
f4.3 <- for_ex_BC ~  debt_to_GDP +  nominal_rate + inflation_mean

### RegressÃµes com resposta AM divida estrangeira completa:

reg4.3.1 <- plm(f4.3, data = panel_dataset_AM, model = "within", effect = "individual")

reg4.3.2 <- plm(f4.3, data = panel_dataset_AM, model = "within", effect = "time")

reg4.3.3 <- plm(f4.3, data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.3.4 <- plm(f4.3, data = panel_dataset_AM, model = "pooling")

# Clustering:

reg4.3.1c <- coeftest(reg4.3.1, vcovHC.plm(reg4.3.1, type="sss", cluster = "group", method = "white2"))

reg4.3.2c <- coeftest(reg4.3.2, vcovHC.plm(reg4.3.2, type="sss", cluster="group", method = "white2"))

reg4.3.3c <- coeftest(reg4.3.3, vcovHC.plm(reg4.3.3, type="sss", cluster="group", method = "white2"))

reg4.3.4c <- coeftest(reg4.3.4, vcovHC.plm(reg4.3.4, type="sss", cluster="group", method = "white2"))

stargazer(reg4.3.1c, reg4.3.2c, reg4.3.3c, reg4.3.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


############################################################################################################################

##Type 4 regression
#Regression 4.4
f4.4 <- for_ex_BC ~  debt_to_GDP +  nominal_rate + inflation_mean + lending_borroeing_rate + account_GPD

### RegressÃµes com resposta AM divida estrangeira completa:

reg4.4.1 <- plm(f4.4, data = panel_dataset_AM, model = "within", effect = "individual")

reg4.4.2 <- plm(f4.4, data = panel_dataset_AM, model = "within", effect = "time")

reg4.4.3 <- plm(f4.4, data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.4.4 <- plm(f4.4, data = panel_dataset_AM, model = "pooling")

# Clustering:

reg4.4.1c <- coeftest(reg4.4.1, vcovHC.plm(reg4.4.1, type="sss", cluster = "group", method = "white2"))

reg4.4.2c <- coeftest(reg4.4.2, vcovHC.plm(reg4.4.2, type="sss", cluster="group", method = "white2"))

reg4.4.3c <- coeftest(reg4.4.3, vcovHC.plm(reg4.4.3, type="sss", cluster="group", method = "white2"))

reg4.4.4c <- coeftest(reg4.4.4, vcovHC.plm(reg4.4.4, type="sss", cluster="group", method = "white2"))

stargazer(reg4.4.1c, reg4.4.2c, reg4.4.3c, reg4.4.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


############################################################################################################################


##Type 4 regression
#Regression 4.5

# RegressÃµes:
reg4.5.1 <- plm(for_ex_BC ~  debt_to_GDP*post_08 + fx + inflation_mean +  nominal_rate + vix_EUA + taxes + account_GPD + lending_borroeing_rate + unemployment,
                data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.5.2 <- plm(for_ex_BC ~  debt_to_GDP*post_15 + fx + inflation_mean +  nominal_rate + vix_EUA + taxes + account_GPD + lending_borroeing_rate + unemployment,
                data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.5.3 <- plm(for_ex_BC ~  debt_to_GDP*post_16 + fx + inflation_mean +  nominal_rate + vix_EUA + taxes + account_GPD + lending_borroeing_rate + unemployment,
                data = panel_dataset_AM, model = "within", effect = "twoways")

reg4.5.4 <- plm(for_ex_BC ~  debt_to_GDP*post_17 + fx + inflation_mean +  nominal_rate + vix_EUA + taxes + account_GPD + lending_borroeing_rate + unemployment,
                data = panel_dataset_AM, model = "within", effect = "twoways")


# Clustering:

reg4.5.1c <- coeftest(reg4.5.1, vcovHC.plm(reg4.5.1, type="sss", cluster = "group", method = "white2"))

reg4.5.2c <- coeftest(reg4.5.2, vcovHC.plm(reg4.5.2, type="sss", cluster = "group", method = "white2"))

reg4.5.3c <- coeftest(reg4.5.3, vcovHC.plm(reg4.5.3, type="sss", cluster = "group", method = "white2"))

reg4.5.4c <- coeftest(reg4.5.4, vcovHC.plm(reg4.5.4, type="sss", cluster = "group", method = "white2"))

stargazer(reg4.5.1c, reg4.5.2c, reg4.5.3c, reg4.5.4c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(D2008)","(D2015)", "(D2016)", "(D2017)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "YES", "YES", "YES")),
          dep.var.labels = c("Foreign participation on public debt"))        # Vemos que pÃ³s 2008 o debt to GDP se torna significante.


####################################################################################################################################################################################################################

##Type 5 regression
#Regression 5.1
#REGRESSAO 1:
#Tudo


#REGRESSAO 7:

f7 <- for_part ~  debt_to_GDP 

#Country Fixed effect
regEM7.1 <- plm(f7, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
regEM7.2 <- plm(f7, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
regEM7.3 <- plm(f7, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
regEM7.4 <- plm(f7, data = panel_dataset_EM, model = "pooling")

#Randon effect
regEM7.5 <- plm(f7, data = panel_dataset_EM, model = "random", random.method = "walhus")


# Testes
# Pooled vs Fixed
# P-valor inferior a 0,05, o modelo de Efeitos Fixos Ã© melhor do que o modelo Pooled
pFtest(regEM7.1,regEM7.4) #Country Fixed
pFtest(regEM7.2,regEM7.4) #Pooled
pFtest(regEM7.3,regEM7.4) #Country-Year Fixed

# Pooled vs Aleatorio
# Breusch e Pagan
# P-valor inferior a 0,05 o modelo de Efeitos AleatÃ³rios Ã© superior ao modelo Pooled.
plmtest(regEM7.4, type="bp") #Randon Effect

# Fixed vs Randon
# Hausmann
# P-valor superior a 0,05 o modelo de Efeitos AleatÃ³rios
phtest(regEM7.1,regEM7.5) #Efeitos AleatÃ³rios
phtest(regEM7.2,regEM7.5) #Year Fixed
phtest(regEM7.3,regEM7.5) #Efeitos AleatÃ³rios

# Teste de dependencia transversal
# A hipÃ³tese nula Ã© de que os resÃ?duos atravÃ©s dos indivÃ?duos nÃ£o estÃ£o correlacionados
pcdtest(regEM7.1, test="cd") 

# Normalidade  dos residuos
# H0: normalidade nos resÃ?duos da regressÃ£o.
shapiro.test(regEM7.1$residuals) 

# Homocedasticidade dos residuos
# Breusch e Pagan
# H0: nao hÃ¡ homocedasticidade nos residuos 
bptest(regEM7.1) #problema de heterocedasticidade

# CorrelaÃ§Ã£o serial
# Breusch-Godfrey/Wooldridge
# H0: nao existe correlacao serial 
pbgtest(regEM7.1) #Existe problema de correlacao nos dados


# Teste para efeitos individuais ou de tempo
# A hipÃ³tse nula Ã© a nÃ£o correlaÃ§Ã£o entre os erros do mesmo grupo
pwtest(regEM7.4, effect = "individual") # CorrelaÃ§Ã£o entre erros
pwtest(regEM7.4, effect = "time") # Nao hÃ¡ correlaÃ§Ã£o entre erros

# Teste de dependencia transversal
# A hipÃ³tese nula Ã© de que os resÃ?duos atravÃ©s dos indivÃ?duos nÃ£o estÃ£o correlacionados
pcdtest(regEM7.1, test="cd") 


#Cluster 

regEM7.1c <- coeftest(regEM7.1, vcovHC.plm(regEM7.1, type="sss", cluster = "group", method = "white2"))

regEM7.2c <- coeftest(regEM7.2, vcovHC.plm(regEM7.2, type="sss", cluster="group", method = "white2"))

regEM7.3c <- coeftest(regEM7.3, vcovHC.plm(regEM7.3, type="sss", cluster="group", method = "white2"))

regEM7.4c <- coeftest(regEM7.4, vcovHC.plm(regEM7.4, type="sss", cluster="group", method = "white2"))

regEM7.5c <- coeftest(regEM7.5, vcovHC.plm(regEM7.5, type="sss", cluster="group", method = "white2"))



stargazer(regEM7.1c, regEM7.2c, regEM7.3c, regEM7.4c, regEM7.5c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

############################################################################################################################################################################################################################################################################################################

#REGRESSAO 8:

f8 <- for_part ~ debt_to_GDP + inflation_mean 


regEM8.1 <- plm(f8, data = panel_dataset_EM, effect = "individual", model = "within")

regEM8.2 <- plm(f8, data = panel_dataset_EM, model = "within", effect = "time")

regEM8.3 <- plm(f8, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM8.4 <- plm(f8, data = panel_dataset_EM, model = "pooling")

regEM8.5 <- plm(f8, data = panel_dataset_EM, model = "random", random.method = "walhus")



# Testes
# Pooled vs Fixed
# P-valor inferior a 0,05, o modelo de Efeitos Fixos Ã© melhor do que o modelo Pooled
pFtest(regEM8.1,regEM8.4) #Country Fixed
pFtest(regEM8.2,regEM8.4) #Year Fixed
pFtest(regEM8.3,regEM8.4) #Country-Year Fixed

# Pooled vs Aleatorio
# Breusch e Pagan
# AceitaÃ§Ã£o de H0 implica que o modelo pooled Ã© preferÃ?vel
plmtest(regEM8.4, type="bp") #Random Effect

# Fixed vs Randon
# Hausmann
# Se rejeitar a hipÃ³tese nula, o modelo de Efeitos Fixos Ã© o mais adequado
phtest(regEM8.1,regEM8.5) #Country Fixed
phtest(regEM8.2,regEM8.5) #Year Fixed
phtest(regEM8.3,regEM8.5) #Country-Year Fixed

# Teste de dependencia transversal
# A hipÃ³tese nula Ã© de que os resÃ?duos atravÃ©s dos indivÃ?duos nÃ£o estÃ£o correlacionados
pcdtest(regEM8.1, test="cd") 

# Normalidade  dos residuos
# H0: normalidade nos resÃ?duos da regressÃ£o.
shapiro.test(regEM8.1$residuals) 

# Homocedasticidade dos residuos
# Breusch e Pagan
# H0: nao hÃ¡ homocedasticidade nos residuos 
bptest(regEM8.1) #problema de heterocedasticidade

# CorrelaÃ§Ã£o serial
# Breusch-Godfrey/Wooldridge
# H0: nao existe correlacao serial 
pbgtest(regEM8.1) #Existe problema de correlacao nos dados


# Teste para efeitos individuais ou de tempo
# A hipÃ³tse nula Ã© a nÃ£o correlaÃ§Ã£o entre os erros do mesmo grupo
pwtest(regEM8.4, effect = "individual") # Nao hÃ¡ correlaÃ§Ã£o entre erros
pwtest(regEM8.4, effect = "time") # Nao hÃ¡ correlaÃ§Ã£o entre erros



# Cluster

regEM8.1c <- coeftest(regEM8.1, vcovHC.plm(regEM8.1, type="sss", cluster ="group", method = "white2"))

regEM8.2c <- coeftest(regEM8.2, vcovHC.plm(regEM8.2, type="sss", cluster="group", method = "white2"))

regEM8.3c <- coeftest(regEM8.3, vcovHC.plm(regEM8.3, type="sss", cluster="group", method = "white2"))

regEM8.4c <- coeftest(regEM8.4, vcovHC.plm(regEM8.4, type="sss", cluster="group", method = "white2"))

regEM8.5c <- coeftest(regEM8.5, vcovHC.plm(regEM8.5, type="sss", cluster="group", method = "white2"))

stargazer(regEM8.1c, regEM8.2c, regEM8.3c, regEM8.4c, regEM8.5c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))





############################################################################################################################################################################################################################################################################################################

#REGRESSAO 9:


f9 <- for_part ~  debt_to_GDP +  account_GPD + lending_GDP + vix_EUA


regEM9.1 <- plm(f9, data = panel_dataset_EM, effect = "individual", model = "within")

regEM9.2 <- plm(f9, data = panel_dataset_EM, model = "within", effect = "time")

regEM9.3 <- plm(f9, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM9.4 <- plm(f9, data = panel_dataset_EM, model = "pooling")

regEM9.5 <- plm(f9, data = panel_dataset_EM, model = "random", random.method = "walhus")


# Cluster

regEM9.1c <- coeftest(regEM9.1, vcovHC.plm(regEM9.1, type="sss", cluster ="group", method = "white2"))

regEM9.2c <- coeftest(regEM9.2, vcovHC.plm(regEM9.2, type="sss", cluster="group", method = "white2"))

regEM9.3c <- coeftest(regEM9.3, vcovHC.plm(regEM9.3, type="sss", cluster="group", method = "white2"))

regEM9.4c <- coeftest(regEM9.4, vcovHC.plm(regEM9.4, type="sss", cluster="group", method = "white2"))

regEM9.5c <- coeftest(regEM9.5, vcovHC.plm(regEM9.5, type="sss", cluster="group", method = "white2"))


stargazer(regEM9.1c, regEM9.2c, regEM9.3c, regEM9.4c, regEM9.5c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))



############################################################################################################################################################################################################################################################################################################

#REGRESSAO 10:

f10 <- for_part ~  debt_to_GDP + account_GPD + lending_GDP + inflation_mean + vix_EUA 

regEM10.1 <- plm(f10, data = panel_dataset_EM, effect = "individual", model = "within")

regEM10.2 <- plm(f10, data = panel_dataset_EM, model = "within", effect = "time")

regEM10.3 <- plm(f10, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM10.4 <- plm(f10, data = panel_dataset_EM, model = "pooling")

regEM10.5 <- plm(f10, data = panel_dataset_EM, model = "random", random.method = "walhus")


# Cluster

regEM10.1c <- coeftest(regEM10.1, vcovHC.plm(regEM10.1, type="sss", cluster ="group", method = "white2"))

regEM10.2c <- coeftest(regEM10.2, vcovHC.plm(regEM10.2, type="sss", cluster="group", method = "white2"))

regEM10.3c <- coeftest(regEM10.3, vcovHC.plm(regEM10.3, type="sss", cluster="group", method = "white2"))

regEM10.4c <- coeftest(regEM10.4, vcovHC.plm(regEM10.4, type="sss", cluster="group", method = "white2"))

regEM10.5c <- coeftest(regEM10.5, vcovHC.plm(regEM10.5, type="sss", cluster="group", method = "white2"))

stargazer(regEM10.1c, regEM10.2c, regEM10.3c, regEM10.4c, regEM10.5c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))




############################################################################################################################################################################################################################################################################################################

#REGRESSAO 11:

f11 <- for_part ~  debt_to_GDP + account_GPD + lending_GDP + inflation_mean + vix_EUR 

regEM11.1 <- plm(f11, data = panel_dataset_EM, effect = "individual", model = "within")

regEM11.2 <- plm(f11, data = panel_dataset_EM, model = "within", effect = "time")

regEM11.3 <- plm(f11, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM11.4 <- plm(f11, data = panel_dataset_EM, model = "pooling")

regEM11.5 <- plm(f11, data = panel_dataset_EM, model = "random", random.method = "walhus")



# Cluster

regEM11.1c <- coeftest(regEM11.1, vcovHC.plm(regEM11.1, type="sss", cluster ="group", method = "white2"))

regEM11.2c <- coeftest(regEM11.2, vcovHC.plm(regEM11.2, type="sss", cluster="group", method = "white2"))

regEM11.3c <- coeftest(regEM11.3, vcovHC.plm(regEM11.3, type="sss", cluster="group", method = "white2"))

regEM11.4c <- coeftest(regEM11.4, vcovHC.plm(regEM11.4, type="sss", cluster="group", method = "white2"))

regEM11.5c <- coeftest(regEM11.5, vcovHC.plm(regEM11.5, type="sss", cluster="group", method = "white2"))

stargazer(regEM11.1c, regEM11.2c, regEM11.3c, regEM11.4c, regEM11.5c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))



####################################################################################################################################################################################################################

##Type 6 regression
#Regression 5.1

#REGRESSAO 1:

f1 <- for_ex_BC ~  debt_to_GDP

#Country Fixed effect
regEM1.1 <- plm(f1, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
regEM1.2 <- plm(f1, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
regEM1.3 <- plm(f1, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
regEM1.4 <- plm(f1, data = panel_dataset_EM, model = "pooling")

#Randon effect
regEM1.5 <- plm(f1, data = panel_dataset_EM, model = "random", random.method = "walhus")


# Testes
# Pooled vs Fixed
pFtest(regEM1.1,regEM1.4) #Country Fixed
pFtest(regEM1.2,regEM1.4) #Year Fixed
pFtest(regEM1.3,regEM1.4) #Country-Year Fixed

# Pooled vs Aleatorio
# Breusch e Pagan
plmtest(regEM1.4, type="bp") #Randon Effect

# Fixed vs Randon
# Hausmann
phtest(regEM1.1,regEM1.5) #Country Fixed
phtest(regEM1.2,regEM1.5) #Year Fixed
phtest(regEM1.3,regEM1.5) #Country-Year Fixed

# Teste de dependencia transversal
# A hipÃ³tese nula Ã© de que os resÃ?duos atravÃ©s dos indivÃ?duos nÃ£o estÃ£o correlacionados
pcdtest(regEM1.1, test="cd") 

# Normalidade  dos residuos
# H0: normalidade nos resÃ?duos da regressÃ£o.
shapiro.test(regEM1.1$residuals) 

# Homocedasticidade dos residuos
# Breusch e Pagan
# H0: nao hÃ¡ homocedasticidade nos residuos 
bptest(regEM1.1) #problema de heterocedasticidade

# CorrelaÃ§Ã£o serial
# Breusch-Godfrey/Wooldridge
# H0: nao existe correlacao serial 
pbgtest(regEM1.1) #Existe problema de correlacao nos dados


# Teste para efeitos individuais ou de tempo
# A hipÃ³tse nula Ã© a nÃ£o correlaÃ§Ã£o entre os erros do mesmo grupo
pwtest(regEM1.4, effect = "individual") # CorrelaÃ§Ã£o entre erros
pwtest(regEM1.4, effect = "time") # Nao hÃ¡ correlaÃ§Ã£o entre erros

# Teste de dependencia transversal
# A hipÃ³tese nula Ã© de que os resÃ?duos atravÃ©s dos indivÃ?duos nÃ£o estÃ£o correlacionados
pcdtest(regEM1.1, test="cd") 


#Cluster 

regEM1.1c <- coeftest(regEM1.1, vcovHC.plm(regEM1.1, type="sss", cluster = "group", method = "white2"))

regEM1.2c <- coeftest(regEM1.2, vcovHC.plm(regEM1.2, type="sss", cluster="group", method = "white2"))

regEM1.3c <- coeftest(regEM1.3, vcovHC.plm(regEM1.3, type="sss", cluster="group", method = "white2"))

regEM1.4c <- coeftest(regEM1.4, vcovHC.plm(regEM1.4, type="sss", cluster="group", method = "white2"))

regEM1.5c <- coeftest(regEM1.5, vcovHC.plm(regEM1.5, type="sss", cluster="group", method = "white2"))



stargazer(regEM1.1c, regEM1.2c, regEM1.3c, regEM1.4c, regEM1.5c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

############################################################################################################################################################################################################################################################################################################

#REGRESSAO 2:

f2 <- for_ex_BC ~ ln_GDP_per_cap_curr


regEM2.1 <- plm(f2, data = panel_dataset_EM, effect = "individual", model = "within")

regEM2.2 <- plm(f2, data = panel_dataset_EM, model = "within", effect = "time")

regEM2.3 <- plm(f2, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM2.4 <- plm(f2, data = panel_dataset_EM, model = "pooling")

regEM2.5 <- plm(f2, data = panel_dataset_EM, model = "random", random.method = "walhus")


# Testes
# Pooled vs Fixed
# P-valor inferior a 0,05, o modelo de Efeitos Fixos Ã© melhor do que o modelo Pooled
pFtest(regEM2.1,regEM2.4) #Country Fixed
pFtest(regEM2.2,regEM2.4) #Year Fixed
pFtest(regEM2.3,regEM2.4) #Country-Year Fixed

# Pooled vs Aleatorio
# Breusch e Pagan
# AceitaÃ§Ã£o de H0 implica que o modelo pooled Ã© preferÃ?vel
plmtest(regEM2.4, type="bp") #Random Effect

# Fixed vs Randon
# Hausmann
# Se rejeitar a hipÃ³tese nula, o modelo de Efeitos Fixos Ã© o mais adequado
phtest(regEM2.1,regEM2.5) #Country Fixed
phtest(regEM2.2,regEM2.5) #Year Fixed
phtest(regEM2.3,regEM2.5) #Country-Year Fixed

# Teste de dependencia transversal
# A hipÃ³tese nula Ã© de que os resÃ?duos atravÃ©s dos indivÃ?duos nÃ£o estÃ£o correlacionados
pcdtest(regEM2.1, test="cd") 

# Normalidade  dos residuos
# H0: normalidade nos resÃ?duos da regressÃ£o.
shapiro.test(regEM2.1$residuals) 

# Homocedasticidade dos residuos
# Breusch e Pagan
# H0: nao hÃ¡ homocedasticidade nos residuos 
bptest(regEM2.1) #problema de heterocedasticidade

# CorrelaÃ§Ã£o serial
# Breusch-Godfrey/Wooldridge
# H0: nao existe correlacao serial 
pbgtest(regEM2.1) #Existe problema de correlacao nos dados


# Teste para efeitos individuais ou de tempo
# A hipÃ³tse nula Ã© a nÃ£o correlaÃ§Ã£o entre os erros do mesmo grupo
pwtest(regEM2.4, effect = "individual") # CorrelaÃ§Ã£o entre erros
pwtest(regEM2.4, effect = "time") # Nao hÃ¡ correlaÃ§Ã£o entre erros



# Cluster

regEM2.1c <- coeftest(regEM2.1, vcovHC.plm(regEM2.1, type="sss", cluster ="group", method = "white2"))

regEM2.2c <- coeftest(regEM2.2, vcovHC.plm(regEM2.2, type="sss", cluster="group", method = "white2"))

regEM2.3c <- coeftest(regEM2.3, vcovHC.plm(regEM2.3, type="sss", cluster="group", method = "white2"))

regEM2.4c <- coeftest(regEM2.4, vcovHC.plm(regEM2.4, type="sss", cluster="group", method = "white2"))

regEM2.5c <- coeftest(regEM2.5, vcovHC.plm(regEM2.5, type="sss", cluster="group", method = "white2"))

stargazer(regEM2.1c, regEM2.2c, regEM2.3c, regEM2.4c, regEM2.5c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))





############################################################################################################################################################################################################################################################################################################

#REGRESSAO 3:

f3 <- for_ex_BC ~  debt_to_GDP + account_GPD + lending_GDP + vix_EUA 


regEM3.1 <- plm(f3, data = panel_dataset_EM, effect = "individual", model = "within")

regEM3.2 <- plm(f3, data = panel_dataset_EM, model = "within", effect = "time")

regEM3.3 <- plm(f3, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM3.4 <- plm(f3, data = panel_dataset_EM, model = "pooling")

regEM3.5 <- plm(f3, data = panel_dataset_EM, model = "random", random.method = "walhus")


# Testes
# Pooled vs Fixed
pFtest(regEM3.1,regEM3.4) #Country Fixed
pFtest(regEM3.2,regEM3.4) #Year Fixed
pFtest(regEM3.3,regEM3.4) #Country-Year Fixed

# Pooled vs Aleatorio
# Breusch e Pagan
plmtest(regEM3.4, type="bp") #Randon Effect

# Fixed vs Randon
# Hausmann
phtest(regEM3.1,regEM3.5) #Country Fixed
phtest(regEM3.2,regEM3.5) #Year Fixed
phtest(regEM3.3,regEM3.5) #Country-Year Fixed

# Teste de dependencia transversal
# A hipótese nula é de que os resíduos através dos indivíduos não estão correlacionados
pcdtest(regEM3.1, test="cd") 

# Normalidade  dos residuos
# H0: normalidade nos resíduos da regressão.
shapiro.test(regEM3.1$residuals) 

# Homocedasticidade dos residuos
# Breusch e Pagan
# H0: nao há homocedasticidade nos residuos 
bptest(regEM3.1) #problema de heterocedasticidade

# Correlação serial
# Breusch-Godfrey/Wooldridge
# H0: nao existe correlacao serial 
pbgtest(regEM3.1) #Existe problema de correlacao nos dados


# Teste para efeitos individuais ou de tempo
# A hipótse nula é a não correlação entre os erros do mesmo grupo
pwtest(regEM3.4, effect = "individual") # Correlação entre erros
pwtest(regEM3.4, effect = "time") # Nao há correlação entre erros

# Teste de dependencia transversal
# A hipótese nula é de que os resíduos através dos indivíduos não estão correlacionados
pcdtest(regEM3.1, test="cd") 


# Cluster

regEM3.1c <- coeftest(regEM3.1, vcovHC.plm(regEM3.1, type="sss", cluster ="group", method = "white2"))

regEM3.2c <- coeftest(regEM3.2, vcovHC.plm(regEM3.2, type="sss", cluster="group", method = "white2"))

regEM3.3c <- coeftest(regEM3.3, vcovHC.plm(regEM3.3, type="sss", cluster="group", method = "white2"))

regEM3.4c <- coeftest(regEM3.4, vcovHC.plm(regEM3.4, type="sss", cluster="group", method = "white2"))

regEM3.5c <- coeftest(regEM3.5, vcovHC.plm(regEM3.5, type="sss", cluster="group", method = "white2"))


stargazer(regEM3.1c, regEM3.2c, regEM3.3c, regEM3.4c, regEM3.5c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))



############################################################################################################################################################################################################################################################################################################

#REGRESSAO 4:

f4 <- for_ex_BC ~  debt_to_GDP + account_GPD + lending_GDP + vix_EUR

regEM4.1 <- plm(f4, data = panel_dataset_EM, effect = "individual", model = "within")

regEM4.2 <- plm(f4, data = panel_dataset_EM, model = "within", effect = "time")

regEM4.3 <- plm(f4, data = panel_dataset_EM, model = "within", effect = "twoways")

regEM4.4 <- plm(f4, data = panel_dataset_EM, model = "pooling")

regEM4.5 <- plm(f4, data = panel_dataset_EM, model = "random", random.method = "walhus")



# Cluster

regEM4.1c <- coeftest(regEM4.1, vcovHC.plm(regEM4.1, type="sss", cluster ="group", method = "white2"))

regEM4.2c <- coeftest(regEM4.2, vcovHC.plm(regEM4.2, type="sss", cluster="group", method = "white2"))

regEM4.3c <- coeftest(regEM4.3, vcovHC.plm(regEM4.3, type="sss", cluster="group", method = "white2"))

regEM4.4c <- coeftest(regEM4.4, vcovHC.plm(regEM4.4, type="sss", cluster="group", method = "white2"))

regEM4.5c <- coeftest(regEM4.5, vcovHC.plm(regEM4.5, type="sss", cluster="group", method = "white2"))

stargazer(regEM4.1c, regEM4.2c, regEM4.3c, regEM4.4c, regEM4.5c,
          title = "Debt held by foreign investors excluding official institutions (Clusterized errors)", type = "latex", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "NO", "YES", "YES", "NO", "NO"), c("Year FE", "YES", "NO", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))




####################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################





