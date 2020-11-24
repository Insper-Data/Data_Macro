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
  filter(develop == "AM",
         country != "United States")  # Tirei os EUA!!!!

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
f1.4 <-  for_part ~  debt_to_GDP  + ln_GDP_per_cap_cur + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA  + nominal_rate +  inflation_mean + develop + post_08


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
f1.5 <-  for_part ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + debt_to_GDP*develop  + vix_EUA  + nominal_rate + lending_borroeing_rate  + develop 


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


###Regression 1.6 ()
f1.6 <-  for_part ~   debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + debt_to_GDP*develop  + vix_EUA  + nominal_rate + account_balance   +develop 


reg1.6.1 <- plm(f1.6, data = panel_dataset, model="within", effect="individual")

reg1.6.2 <- plm(f1.6, data = panel_dataset, model = "within", effect = "time")

reg1.6.3 <- plm(f1.6, data = panel_dataset, model = "within", effect = "twoways")

reg1.6.4 <- plm(f1.6, data = panel_dataset, model = "pooling")



reg1.6.1c <- coeftest(reg1.6.1, vcov=vcovHC(reg1.6.1, type="sss", cluster="group", method="white2"))
reg1.6.2c <- coeftest(reg1.6.2, vcov=vcovHC(reg1.6.2, type="sss", cluster="group", method="white2"))
reg1.6.3c <- coeftest(reg1.6.3, vcov=vcovHC(reg1.6.3, type="sss", cluster="group", method="white2"))
reg1.6.4c <- coeftest(reg1.6.4, vcov=vcovHC(reg1.6.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.6.1c, reg1.6.2c, reg1.6.3c, reg1.6.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))

###Regression 1.7 ()
f1.7 <-  for_part ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + debt_to_GDP*develop    + nominal_rate +  account_balance +  develop + ln_GDP_per_cap_cur*develop


reg1.7.1 <- plm(f1.7, data = panel_dataset, model="within", effect="individual")

reg1.7.2 <- plm(f1.7, data = panel_dataset, model = "within", effect = "time")

reg1.7.3 <- plm(f1.7, data = panel_dataset, model = "within", effect = "twoways")

reg1.7.4 <- plm(f1.7, data = panel_dataset, model = "pooling")



reg1.7.1c <- coeftest(reg1.7.1, vcov=vcovHC(reg1.7.1, type="sss", cluster="group", method="white2"))
reg1.7.2c <- coeftest(reg1.7.2, vcov=vcovHC(reg1.7.2, type="sss", cluster="group", method="white2"))
reg1.7.3c <- coeftest(reg1.7.3, vcov=vcovHC(reg1.7.3, type="sss", cluster="group", method="white2"))
reg1.7.4c <- coeftest(reg1.7.4, vcov=vcovHC(reg1.7.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.7.1c, reg1.7.2c, reg1.7.3c, reg1.7.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))


###Regression 1.8 ()
f1.8 <-  for_part ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + debt_to_GDP*develop + vix_EUA    + nominal_rate +  lending_borroeing_rate +  develop + ln_GDP_per_cap_cur*develop


reg1.8.1 <- plm(f1.8, data = panel_dataset, model="within", effect="individual")

reg1.8.2 <- plm(f1.8, data = panel_dataset, model = "within", effect = "time")

reg1.8.3 <- plm(f1.8, data = panel_dataset, model = "within", effect = "twoways")

reg1.8.4 <- plm(f1.8, data = panel_dataset, model = "pooling")



reg1.8.1c <- coeftest(reg1.8.1, vcov=vcovHC(reg1.7.1, type="sss", cluster="group", method="white2"))
reg1.8.2c <- coeftest(reg1.8.2, vcov=vcovHC(reg1.7.2, type="sss", cluster="group", method="white2"))
reg1.8.3c <- coeftest(reg1.8.3, vcov=vcovHC(reg1.7.3, type="sss", cluster="group", method="white2"))
reg1.8.4c <- coeftest(reg1.8.4, vcov=vcovHC(reg1.7.4, type="sss", cluster="group", method="white2"))

stargazer(reg1.8.1c, reg1.8.2c, reg1.8.3c, reg1.8.4c,
          title = "Debt held by foreign investors (Clusterized errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))
##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################



##Type 2 regression
###Regression 2.1 (todos)
f2.1 <-  for_ex_BC ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA + taxes + lending_borroeing_rate + account_balance + unemployment + inflation_mean + develop + post_08


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
f2.2 <-  for_ex_BC ~  debt_to_GDP + develop 

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
f2.3 <-  for_ex_BC ~  debt_to_GDP + debt_to_GDP*develop + vix_EUA +  account_balance + develop


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

f2.4 <-  for_ex_BC ~  debt_to_GDP  + ln_GDP_per_cap_cur + debt_to_GDP*develop + debt_to_GDP*post_08  + vix_EUA  + nominal_rate  + develop + post_08


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


##Regression 2.5

###Regression 2.5 (todos os regressores e duas interações e com a "nominal rate" (mas dropando algumas observa��es))

f2.5 <-  for_ex_BC ~  debt_to_GDP  + ln_GDP_per_cap_cur + debt_to_GDP*develop + vix_EUA  + nominal_rate +   develop + 


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


##Type 2 regression
###Regression 2.5 (todos os regressores tirando o "post-08"; duas interações; com a "nominal rate" (mas dropando algumas observações))

f2.5 <- for_ex_BC ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + debt_to_GDP*develop  + vix_EUA  + nominal_rate + lending_borroeing_rate + account_balance + inflation_mean + develop 



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



##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################



##Type 3 regression
#Regression 3.1

f3.1 <- for_part ~  debt_to_GDP + fx_volatility + ln_GDP_per_cap_cur + inflation_mean +  nominal_rate + vix_EUA + taxes + account_balance + lending_borroeing_rate + unemployment


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
f3.4 <- for_part ~  debt_to_GDP +  nominal_rate + inflation_mean + lending_borroeing_rate + account_balance

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


##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################




##Type 4 regression
#Regression 4.1

f4.1 <- for_ex_BC ~  debt_to_GDP + fx + ln_GDP_per_cap_cur + inflation_mean +  nominal_rate + vix_EUA + taxes + account_balance + lending_borroeing_rate + unemployment

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


##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################


##Type 5 regression
#Regression 5.1



f5.1 <- for_part ~  debt_to_GDP*post_08

#Country Fixed effect
reg5.1.1 <- plm(f5.1, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
reg5.1.2 <- plm(f5.1, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
reg5.1.3 <- plm(f5.1, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
reg5.1.4 <- plm(f5.1, data = panel_dataset_EM, model = "pooling")

#Randon effect
reg5.1.5 <- plm(f5.1, data = panel_dataset_EM, model = "random", random.method = "walhus")



#Cluster 

reg5.1.1c <- coeftest(reg5.1.1, vcovHC.plm(reg5.1.1, type="sss", cluster = "group", method = "white2"))

reg5.1.2c <- coeftest(reg5.1.2, vcovHC.plm(reg5.1.2, type="sss", cluster="group", method = "white2"))

reg5.1.3c <- coeftest(reg5.1.3, vcovHC.plm(reg5.1.3, type="sss", cluster="group", method = "white2"))

reg5.1.4c <- coeftest(reg5.1.4, vcovHC.plm(reg5.1.4, type="sss", cluster="group", method = "white2"))

reg5.1.5c <- coeftest(reg5.1.5, vcovHC.plm(reg5.1.5, type="sss", cluster="group", method = "white2"))



stargazer(reg5.1.1c, reg5.1.2c, reg5.1.3c, reg5.1.4c, reg5.1.5c,
          title = "Total Debt Held by Foreign Investor (Clustered errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO", "NO"), c("Year FE", "NO", "YES", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))



#REGRESSAO 2:


f5.2 <- for_part ~ debt_to_GDP + nominal_rate + inflation_end + fx_volatility + vix_EUA


#Country Fixed effect
reg5.2.1 <- plm(f5.2, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
reg5.2.2 <- plm(f5.2, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
reg5.2.3 <- plm(f5.2, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
reg5.2.4 <- plm(f5.2, data = panel_dataset_EM, model = "pooling")

#Randon effect
reg5.2.5 <- plm(f5.2, data = panel_dataset_EM, model = "random", random.method = "walhus")



#Cluster 

reg5.2.1c <- coeftest(reg5.2.1, vcovHC.plm(reg5.2.1, type="sss", cluster = "group", method = "white2"))

reg5.2.2c <- coeftest(reg5.2.2, vcovHC.plm(reg5.2.2, type="sss", cluster="group", method = "white2"))

reg5.2.3c <- coeftest(reg5.2.3, vcovHC.plm(reg5.2.3, type="sss", cluster="group", method = "white2"))

reg5.2.4c <- coeftest(reg5.2.4, vcovHC.plm(reg5.2.4, type="sss", cluster="group", method = "white2"))

reg5.2.5c <- coeftest(reg5.2.5, vcovHC.plm(reg5.2.5, type="sss", cluster="group", method = "white2"))



stargazer(reg5.2.1c, reg5.2.2c, reg5.2.3c, reg5.2.4c, reg5.2.5c,
          title = "Total Debt Held by Foreign Investor (Clustered errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO", "NO"), c("Year FE", "NO", "YES", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign Participation on Public Debt"))






#REGRESSAO 3:


f5.3 <- for_part ~ debt_to_GDP + nominal_rate + inflation_end + account_balance + fx_volatility + vix_EUA


#Country Fixed effect
reg5.3.1 <- plm(f5.3, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
reg5.3.2 <- plm(f5.3, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
reg5.3.3 <- plm(f5.3, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
reg5.3.4 <- plm(f5.3, data = panel_dataset_EM, model = "pooling")

#Randon effect
reg5.3.5 <- plm(f5.3, data = panel_dataset_EM, model = "random", random.method = "walhus")



#Cluster 

reg5.3.1c <- coeftest(reg5.3.1, vcovHC.plm(reg5.3.1, type="sss", cluster = "group", method = "white2"))

reg5.3.2c <- coeftest(reg5.3.2, vcovHC.plm(reg5.3.2, type="sss", cluster="group", method = "white2"))

reg5.3.3c <- coeftest(reg5.3.3, vcovHC.plm(reg5.3.3, type="sss", cluster="group", method = "white2"))

reg5.3.4c <- coeftest(reg5.3.4, vcovHC.plm(reg5.3.4, type="sss", cluster="group", method = "white2"))

reg5.3.5c <- coeftest(reg5.3.5, vcovHC.plm(reg5.3.5, type="sss", cluster="group", method = "white2"))



stargazer(reg5.3.1c, reg5.3.2c, reg5.3.3c, reg5.3.4c, reg5.3.5c,
          title = "Total Debt Held by Foreign Investor (Clustered errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO", "NO"), c("Year FE", "NO", "YES", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign Participation on Public Debt"))




#REGRESSAO 4:

f5.4 <- for_part ~ debt_to_GDP*post_08 + nominal_rate + inflation_end + account_balance + fx_volatility + vix_EUA

#Country Fixed effect
reg5.4.1 <- plm(f5.4, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
reg5.4.2 <- plm(f5.4, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
reg5.4.3 <- plm(f5.4, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
reg5.4.4 <- plm(f5.4, data = panel_dataset_EM, model = "pooling")

#Randon effect
reg5.4.5 <- plm(f5.4, data = panel_dataset_EM, model = "random", random.method = "walhus")



#Cluster 

reg5.4.1c <- coeftest(reg5.4.1, vcovHC.plm(reg5.4.1, type="sss", cluster = "group", method = "white2"))

reg5.4.2c <- coeftest(reg5.4.2, vcovHC.plm(reg5.4.2, type="sss", cluster="group", method = "white2"))

reg5.4.3c <- coeftest(reg5.4.3, vcovHC.plm(reg5.4.3, type="sss", cluster="group", method = "white2"))

reg5.4.4c <- coeftest(reg5.4.4, vcovHC.plm(reg5.4.4, type="sss", cluster="group", method = "white2"))

reg5.4.5c <- coeftest(reg5.4.5, vcovHC.plm(reg5.4.5, type="sss", cluster="group", method = "white2"))



stargazer(reg5.4.1c, reg5.4.2c, reg5.4.3c, reg5.4.4c, reg5.4.5c,
          title = "Total Debt Held by Foreign Investor (Clustered errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO", "NO"), c("Year FE", "NO", "YES", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign Participation on Public Debt"))



##############################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################################




##Type 6 regression
#Regression 6.1



f6.1 <- for_ex_BC ~  debt_to_GDP + account_balance + vix_EUA 

#Country Fixed effect
reg6.1.1 <- plm(f6.1, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
reg6.1.2 <- plm(f6.1, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
reg6.1.3 <- plm(f6.1, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
reg6.1.4 <- plm(f6.1, data = panel_dataset_EM, model = "pooling")

#Randon effect
reg6.1.5 <- plm(f6.1, data = panel_dataset_EM, model = "random", random.method = "walhus")



#Cluster 

reg6.1.1c <- coeftest(reg6.1.1, vcovHC.plm(reg6.1.1, type="sss", cluster = "group", method = "white2"))

reg6.1.2c <- coeftest(reg6.1.2, vcovHC.plm(reg6.1.2, type="sss", cluster="group", method = "white2"))

reg6.1.3c <- coeftest(reg6.1.3, vcovHC.plm(reg6.1.3, type="sss", cluster="group", method = "white2"))

reg6.1.4c <- coeftest(reg6.1.4, vcovHC.plm(reg6.1.4, type="sss", cluster="group", method = "white2"))

reg6.1.5c <- coeftest(reg6.1.5, vcovHC.plm(reg6.1.5, type="sss", cluster="group", method = "white2"))



stargazer(reg6.1.1c, reg6.1.2c, reg6.1.3c, reg6.1.4c, reg6.1.5c,
          title = "Total Debt Held by Foreign Investor (Clustered errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO", "NO"), c("Year FE", "NO", "YES", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign participation on public debt"))



#REGRESSAO 2:


f6.2 <- for_ex_BC ~ debt_to_GDP + nominal_rate + inflation_end + account_balance  + vix_EUA


#Country Fixed effect
reg6.2.1 <- plm(f6.2, data = panel_dataset_EM, effect = "individual", model = "within")

#Year Fixed effect
reg6.2.2 <- plm(f6.2, data = panel_dataset_EM,effect = "time", model = "within")

#Country-Year Fixed effect
reg6.2.3 <- plm(f6.2, data = panel_dataset_EM, effect = "twoways", model = "within")

#Pooling
reg6.2.4 <- plm(f6.2, data = panel_dataset_EM, model = "pooling")

#Randon effect
reg6.2.5 <- plm(f6.2, data = panel_dataset_EM, model = "random", random.method = "walhus")



#Cluster 

reg6.2.1c <- coeftest(reg6.2.1, vcovHC.plm(reg6.2.1, type="sss", cluster = "group", method = "white2"))

reg6.2.2c <- coeftest(reg6.2.2, vcovHC.plm(reg6.2.2, type="sss", cluster="group", method = "white2"))

reg6.2.3c <- coeftest(reg6.2.3, vcovHC.plm(reg6.2.3, type="sss", cluster="group", method = "white2"))

reg6.2.4c <- coeftest(reg6.2.4, vcovHC.plm(reg6.2.4, type="sss", cluster="group", method = "white2"))

reg6.2.5c <- coeftest(reg6.2.5, vcovHC.plm(reg6.2.5, type="sss", cluster="group", method = "white2"))



stargazer(reg6.2.1c, reg6.2.2c, reg6.2.3c, reg6.2.4c, reg6.2.5c,
          title = "Total Debt Held by Foreign Investor (Clustered errors)", type = "text", 
          column.labels = c("(Within)","(Within)", "(Within)", "(Pooled)", "(Random)"),
          model.numbers = F,
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO", "NO"), c("Year FE", "NO", "YES", "YES", "NO", "NO")),
          dep.var.labels = c("Foreign Participation on Public Debt"))




