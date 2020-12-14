# Pacotes:

library(tidyverse)
library(plm)
library(stargazer)
library(lmtest)


# Setando o painel:

panel_dataset <- pdata.frame(dataset_total, index = c("country", "year"))

panel_dataset <- panel_dataset %>% 
  mutate(for_part = (foreign_debt/total_debt), for_ex_BC = ((nonbank_foreign_debt + bank_foreign_debt)/total_debt), for_nonbank_prop = (nonbank_foreign_debt/total_debt), develop = as.character(develop))


#panel dataset_AM
panel_dataset_AM <- panel_dataset %>% 
  filter(develop == "AM",
         country != "United States")  # Tirei os EUA!!!!

#panel dataset_EM
panel_dataset_EM <- panel_dataset %>% 
  filter(develop == "EM") 


########################################################################################################################################################################################################################

# Slide com todos os países:


## Primeira versão:

ft1 <- for_ex_BC ~ debt_to_GDP*develop + ln_GDP_per_cap_cte + nominal_rate + fx_volatility + lending_borroeing_rate + vix_EUA

 


reg3.1.1 <- plm(ft1, data = panel_dataset, model = "within", effect = "individual")

reg3.1.2 <- plm(ft1, data = panel_dataset, model = "within", effect = "time")

reg3.1.3 <- plm(ft1, data = panel_dataset, model = "within", effect = "twoways")

reg3.1.4 <- plm(ft1, data = panel_dataset, model = "pooling")



# Clustering errors:
reg3.1.1c <- coeftest(reg3.1.1, vcovHC.plm(reg3.1.1, type="sss", cluster = "group", method = "white2"))[,2]
reg3.1.2c <- coeftest(reg3.1.2, vcovHC.plm(reg3.1.2, type="sss", cluster="group", method = "white2"))[,2]
reg3.1.3c <- coeftest(reg3.1.3, vcovHC.plm(reg3.1.3, type="sss", cluster="group", method = "white2"))[,2]
reg3.1.4c <- coeftest(reg3.1.4, vcovHC.plm(reg3.1.4, type="sss", cluster="group", method = "white2"))[,2]

# Output:
stargazer(reg3.1.1, 
          reg3.1.2, 
          reg3.1.3, 
          reg3.1.4,
          title = "(Clusterized errors)", type = "text", 
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          se = list(reg3.1.1c, reg3.1.2c, reg3.1.3c, reg3.1.4c),
          omit.stat = "f",
          dep.var.labels = c("Foreign participation on public debt"))



########################################################################################################################################################################################################################

# Slide com avançados:

## Primeira versão:

ft1 <- for_ex_BC ~ debt_to_GDP*post_08 + ln_GDP_per_cap_cte + nominal_rate  + lending_borroeing_rate + vix_EUA



# Regressoes em painel

reg3.1.1 <- plm(ft1, data = panel_dataset_AM, model = "within", effect = "individual")

reg3.1.2 <- plm(ft1, data = panel_dataset_AM, model = "within", effect = "time")

reg3.1.3 <- plm(ft1, data = panel_dataset_AM, model = "within", effect = "twoways")

reg3.1.4 <- plm(ft1, data = panel_dataset_AM, model = "pooling")



# Clustering errors:
reg3.1.1c <- coeftest(reg3.1.1, vcovHC.plm(reg3.1.1, type="sss", cluster = "group", method = "white2"))[,2]
reg3.1.2c <- coeftest(reg3.1.2, vcovHC.plm(reg3.1.2, type="sss", cluster="group", method = "white2"))[,2]
reg3.1.3c <- coeftest(reg3.1.3, vcovHC.plm(reg3.1.3, type="sss", cluster="group", method = "white2"))[,2]
reg3.1.4c <- coeftest(reg3.1.4, vcovHC.plm(reg3.1.4, type="sss", cluster="group", method = "white2"))[,2]

# Output:
stargazer(reg3.1.1, 
          reg3.1.2, 
          reg3.1.3, 
          reg3.1.4,
          title = "Advanced Markets", type = "text", 
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          se = list(reg3.1.1c, reg3.1.2c, reg3.1.3c, reg3.1.4c),
          omit.stat = "f",
          dep.var.labels = c("Foreign participation on public debt"))


########################################################################################################################################################################################################################

# Slide com emergentes:




ft1 <- for_ex_BC ~ debt_to_GDP + post_08 + ln_GDP_per_cap_cte + nominal_rate + fx_volatility + account_balance + vix_EUA



# Regressoes em painel

reg3.1.1 <- plm(ft1, data = panel_dataset_EM, model = "within", effect = "individual")

reg3.1.2 <- plm(ft1, data = panel_dataset_EM, model = "within", effect = "time")

reg3.1.3 <- plm(ft1, data = panel_dataset_EM, model = "within", effect = "twoways")

reg3.1.4 <- plm(ft1, data = panel_dataset_EM, model = "pooling")



# Clustering errors:
reg3.1.1c <- coeftest(reg3.1.1, vcovHC.plm(reg3.1.1, type="sss", cluster = "group", method = "white2"))[,2]
reg3.1.2c <- coeftest(reg3.1.2, vcovHC.plm(reg3.1.2, type="sss", cluster="group", method = "white2"))[,2]
reg3.1.3c <- coeftest(reg3.1.3, vcovHC.plm(reg3.1.3, type="sss", cluster="group", method = "white2"))[,2]
reg3.1.4c <- coeftest(reg3.1.4, vcovHC.plm(reg3.1.4, type="sss", cluster="group", method = "white2"))[,2]

# Output:
stargazer(reg3.1.1, 
          reg3.1.2, 
          reg3.1.3, 
          reg3.1.4,
          title = "Emerging Markets - Threshold comparison", type = "text", 
          add.lines = list(c("Country FE", "YES", "NO", "YES", "NO"), c("Year FE", "NO", "YES", "YES", "NO")),
          se = list(reg3.1.1c, reg3.1.2c, reg3.1.3c, reg3.1.4c),
          omit.stat = "f",
          dep.var.labels = c("Foreign participation on public debt"))

