
# Ajuste de base do Tsuda et. al. 


#pacotes
library(tidyverse)
library(ggplot2)
library(scales)
library(readr)
library(readxl)
library(transformr)


#entrando no diretorio das bases (alterar)
getwd()
setwd()

#puxando a base de debt 

########advanced#############

#total debt

total_debt_advanced<- read_xlsx ("total_debt_advanced.xlsx")

total_debt_advanced<-total_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "total_debt")

#foreign debt

foreign_debt_advanced<- read_xlsx ("foreign_debt_advanced.xlsx")

foreign_debt_advanced<-foreign_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "foreign_debt")

#official foreign debt

official_foreign_debt_advanced<- read_xlsx ("official_foreign_debt_advanced.xlsx")

official_foreign_debt_advanced<-official_foreign_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "official_foreign_debt")

#bank foreign debt

bank_foreign_debt_advanced<- read_xlsx ("bank_foreign_debt_advanced.xlsx")

bank_foreign_debt_advanced<-bank_foreign_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "bank_foreign_debt")


#nonbank foreign debt

nonbank_foreign_debt_advanced<- read_xlsx ("nonbank_foreign_debt_advanced.xlsx")

nonbank_foreign_debt_advanced<-nonbank_foreign_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "nonbank_foreign_debt")

#domestic debt

domestic_debt_advanced<- read_xlsx ("domestic_debt_advanced.xlsx")

domestic_debt_advanced<-domestic_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "domestic_debt")

#official domestic debt

official_domestic_debt_advanced<- read_xlsx ("domestic_debt_advanced.xlsx")

official_domestic_debt_advanced<-official_domestic_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "official_domestic_debt")

#bank domestic debt

bank_domestic_debt_advanced<- read_xlsx ("bank_domestic_debt_advanced.xlsx")

bank_domestic_debt_advanced<-bank_domestic_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "bank_domestic_debt")

#nonbank domestic debt

nonbank_domestic_debt_advanced<- read_xlsx ("nonbank_domestic_debt_advanced.xlsx")

nonbank_domestic_debt_advanced<-nonbank_domestic_debt_advanced %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "nonbank_domestic_debt")

#fx

fx_advanced<- read_xlsx ("fx_advanced.xlsx")

fx_advanced <- fx_advanced %>% 
  rename(country = "USD/National currency") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "fx")

#debt to GDP

debt_to_GDP_advanced<- read_xlsx ("debt_to_GDP_advanced.xlsx")

debt_to_GDP_advanced<-debt_to_GDP_advanced %>% 
  rename(country = "Total Debt/GDP") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "debt_to_GDP")


########emerging#############

#total debt

total_debt_emerging<- read_xlsx ("total_debt_emerging.xlsx")

total_debt_emerging<-total_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "total_debt")

#foreign debt

foreign_debt_emerging<- read_xlsx ("foreign_debt_emerging.xlsx")

foreign_debt_emerging<-foreign_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "foreign_debt")

#official foreign debt

official_foreign_debt_emerging<- read_xlsx ("official_foreign_debt_emerging.xlsx")

official_foreign_debt_emerging<-official_foreign_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "official_foreign_debt")

#bank foreign debt

bank_foreign_debt_emerging<- read_xlsx ("bank_foreign_debt_emerging.xlsx")

bank_foreign_debt_emerging<-bank_foreign_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "bank_foreign_debt")


#nonbank foreign debt

nonbank_foreign_debt_emerging<- read_xlsx ("nonbank_foreign_debt_emerging.xlsx")

nonbank_foreign_debt_emerging<-nonbank_foreign_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "nonbank_foreign_debt")

#domestic debt

domestic_debt_emerging<- read_xlsx ("domestic_debt_emerging.xlsx")

domestic_debt_emerging<-domestic_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "domestic_debt")

#official domestic debt

official_domestic_debt_emerging<- read_xlsx ("domestic_debt_emerging.xlsx")

official_domestic_debt_emerging<-official_domestic_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "official_domestic_debt")


#bank domestic debt

bank_domestic_debt_emerging<- read_xlsx ("bank_domestic_debt_emerging.xlsx")

bank_domestic_debt_emerging<-bank_domestic_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "bank_domestic_debt")

#nonbank domestic debt

nonbank_domestic_debt_emerging<- read_xlsx ("nonbank_domestic_debt_emerging.xlsx")

nonbank_domestic_debt_emerging<-nonbank_domestic_debt_emerging %>% 
  rename(country = "Billion LC") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "nonbank_domestic_debt")

#fx

fx_emerging<- read_xlsx ("fx_emerging.xlsx")

fx_emerging<-fx_emerging %>% 
  rename(country = "USD/National currency") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "fx")

#debt to GDP

debt_to_GDP_emerging<- read_xlsx ("debt_to_GDP_emerging.xlsx")

debt_to_GDP_emerging<-debt_to_GDP_emerging %>% 
  rename(country = "Total Debt/GDP") %>%
  pivot_longer(c(`2004Q1` : `2019Q4`), names_to = "yearQ", values_to = "debt_to_GDP")


##juntando as bases para países advanced
advanced <- total_debt_advanced %>% 
  left_join(debt_to_GDP_advanced, by = c("country", "yearQ")) %>% 
  left_join(fx_advanced, by = c("country", "yearQ")) %>% 
  left_join(nonbank_domestic_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(bank_domestic_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(official_domestic_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(domestic_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(nonbank_foreign_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(bank_foreign_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(official_foreign_debt_advanced, by = c("country", "yearQ")) %>% 
  left_join(foreign_debt_advanced, by = c("country", "yearQ")) 


##juntando as bases para paises emerging  

emerging <- total_debt_emerging %>% 
  left_join(debt_to_GDP_emerging, by = c("country", "yearQ")) %>% 
  left_join(fx_emerging, by = c("country", "yearQ")) %>% 
  left_join(nonbank_domestic_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(bank_domestic_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(official_domestic_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(domestic_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(nonbank_foreign_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(bank_foreign_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(official_foreign_debt_emerging, by = c("country", "yearQ")) %>% 
  left_join(foreign_debt_emerging, by = c("country", "yearQ")) 

#juntando as duas

debt_df <-  rbind(emerging, advanced)

#salvando a base em csv

write_csv(debt_df, "debt_df.csv")

