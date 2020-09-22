
# Script para juntar as bases que vamos utilizar

#libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(transformr)

#chegando no wd do computador obs.: modificar para o seu endereço
setwd("C:/Users/gabri/Documents/Insper_Data/Macro/projeto_econometria/bases.csv")


##puxando as bases
debt_prop <- read.csv("tsuda_tidy.csv")  # ja esta tidy  

weo_am <- readxl::read_xlsx("WEO_Data_Paises_AM.xlsx")  # Observar que esta em .xlsx

weo_em <- readxl::read_xlsx("WEO_Data_Paises_EM.xlsx")  # Observar que esta em .xlsx

taxes <- readxl::read_xlsx("taxes.xlsx")  # Observar que esta em .xlsx

gov_index <- readxl::read_xlsx("gov_index.xlsx")  # Observar que esta em .xlsx

real_interest_rate <- readxl::read_xlsx("real_interest_rates.xlsx")  # Observar que esta em .xlsx



 


##arrumando weo_am
weo_am <- weo_am %>%
  pivot_longer('1980':'2021', names_to = "Year", values_to = "value")

weo_am <- weo_am %>% 
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`, country = Country) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = ""))
 
weo_am <- weo_am %>% 
   mutate(value = as.numeric(value), Year = as.integer(Year)) %>% 
   filter(!is.na(country))

weo_am <- weo_am %>%
  select(`WEO Country Code`, country, Year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)


##arrumando weo_em
weo_em <- weo_em %>%
  pivot_longer('1980':'2021', names_to = "Year", values_to = "value")

weo_em <- weo_em %>% 
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`, country = Country) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = ""))

weo_em <- weo_em %>% 
  mutate(value = as.numeric(value), Year = as.integer(Year)) %>% 
  filter(!is.na(country))

weo_em <- weo_em %>%
  select(`WEO Country Code`, country, Year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)


# Bind em weo_am e weo_am:
weo <-  rbind(weo_am, weo_em)


# Agora precisamos fazer o join entre weo e debt_prop:

# Primeiro arrumar as colunas da debt_prop para fazer o join da forma correta:
debt_prop <- debt_prop %>% 
  separate(yearQ, into = c("Year", "Quarter"), sep = "Q", remove = FALSE) %>% 
  mutate(Year = as.integer(Year), Quarter = as.integer(Quarter))


# Agora o join pela base debt_prop:
dataset_total <- debt_prop %>% 
  left_join(weo, by = c("country", "Year"))
# Obs. paises que nÃ£o estavam na weo: Latvia, Lithuania, Norway

#Mudando os nomes das variáveis que vieram da WEO

dataset_total <- dataset_total %>% 
  rename (woe_country_code='WEO Country Code',
          GDP_cte='Gross domestic product, constant prices',
          GDP_cur='Gross domestic product, current prices',
          GDP_per_cap_cte='Gross domestic product per capita, constant prices',
          inflation_mean='Inflation, average consumer prices',
          inflation_end='Inflation, end of period consumer prices',
          unemployment='Unemployment rate',
          lending_borroeing_rate='General government net lending/borrowing',
          account_balance='Current account balance')


#base de impostos
dataset_total <- dataset_total %>% 
  left_join(taxes, by = c("country", "Year"))

#base de indicadores diversos no World Bank

dataset_total <- dataset_total %>% 
  left_join(gov_index, by = c("country", "Year"))


#base de taxa de juros real

dataset_total <- dataset_total %>% 
  left_join(real_interest_rate, by = c("country", "Year"))


#colocando NA nas observações

dataset_total<- dataset_total %>% 
  na_if("..")

# Escrevendo um arquivo csv para dataset_total:
#write_csv(dataset_total, "dataset_total.csv")











 











