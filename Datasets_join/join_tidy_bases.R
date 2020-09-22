
# Script para juntar as bases que vamos utilizar


#libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(transformr)
library(imfr)

#chegando no wd do computador obs.: modificar para o seu endereço
setwd("C:/Users/gabri/Documents/Insper_Data/Macro/projeto_econometria/bases.csv")


##puxando as bases
debt_prop_Q <- read.csv("tsuda_tidy.csv")  # ja esta tidy  

weo_am <- readxl::read_xlsx("WEO_Data_Paises_AM.xlsx")  # Observar que esta em .xlsx

weo_em <- readxl::read_xlsx("WEO_Data_Paises_EM.xlsx")  # Observar que esta em .xlsx

taxes <- readxl::read_xlsx("taxes.xlsx")  # Observar que esta em .xlsx

gov_index <- readxl::read_xlsx("gov_index.xlsx")  # Observar que esta em .xlsx

vix <- readxl::read_xlsx("vix.xlsx")  # Observar que esta em .xlsx


#'anualizando' os dados da base do tsuda

debt_prop <- debt_prop_Q %>% 
  separate(yearQ, into = c("year", "quarter"), sep = "Q", remove = FALSE) %>% 
  group_by(country, year) %>% 
  mutate(total_debt_= mean(total_debt), 
         debt_to_GDP_=mean(debt_to_GDP),
         fx_=mean(fx),
         nonbank_domestic_debt_=mean(nonbank_domestic_debt),
         bank_domestic_debt_=mean(bank_domestic_debt),
         official_domestic_debt_=mean(official_domestic_debt),
         domestic_debt_=mean(domestic_debt),
         nonbank_foreign_debt_=mean(nonbank_foreign_debt),
         bank_foreign_debt_=mean(bank_foreign_debt),
         official_foreign_debt_=mean(official_foreign_debt),
         foreign_debt_=mean(foreign_debt))

 
# arrumando a df debt_prop
debt_prop <- debt_prop %>% 
  mutate(year = as.integer(year), quarter = as.integer(quarter)) %>% 
  select(1,2,3,4,16,17,18,19, 20,21,22,23,24,25, 26)

debt_prop <- debt_prop %>% 
  filter(quarter == 4)

##arrumando weo_am
weo_am <- weo_am %>%
  pivot_longer('1980':'2021', names_to = "year", values_to = "value")

weo_am <- weo_am %>% 
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`, country = Country) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = ""))
 
weo_am <- weo_am %>% 
   mutate(value = as.numeric(value), year = as.integer(year)) %>% 
   filter(!is.na(country))

weo_am <- weo_am %>%
  select(`WEO Country Code`, country, year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)


##arrumando weo_em
weo_em <- weo_em %>%
  pivot_longer('1980':'2021', names_to = "year", values_to = "value")

weo_em <- weo_em %>% 
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`, country = Country) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = ""))

weo_em <- weo_em %>% 
  mutate(value = as.numeric(value), year = as.integer(year)) %>% 
  filter(!is.na(country))

weo_em <- weo_em %>%
  select(`WEO Country Code`, country, year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)


# Bind em weo_am e weo_am:
weo <-  rbind(weo_am, weo_em)


# Agora precisamos fazer o join entre weo e debt_prop:



# Agora o join pela base debt_prop:
dataset_total <- debt_prop %>% 
  left_join(weo, by = c("country", "year"))
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

taxes <- taxes %>% 
  rename(year=1, taxes=3)

dataset_total <- dataset_total %>% 
  left_join(taxes, by = c("country", "year"))

#base de indicadores diversos no World Bank

gov_index <- gov_index %>% 
  select(1,3,7,13, 19, 25, 31, 37)

gov_index <- gov_index %>% 
  rename(year=1, country=2, control_corruption_rank=3, effectiveness_rank=4, politial_stavility_rank=5, regulatory_quality=6, rule_of_law_rank=7, voice_rank=8)

dataset_total <- dataset_total %>% 
  left_join(gov_index, by = c("country", "year"))


#VIX

vix<-vix %>% 
  separate(year, into = c("year", "m", "d"), sep = "-", remove = TRUE)
  
vix <- vix %>% 
  select(1,4,5)

vix<- vix %>% 
  mutate(year = as.numeric(year))
  

dataset_total <- dataset_total %>% 
  left_join(vix, by = c("year"))




#colocando NA nas observações

dataset_total<- dataset_total %>% 
  na_if("..")

# Escrevendo um arquivo csv para dataset_total:
#write_csv(dataset_total, "dataset_total.csv")











 











