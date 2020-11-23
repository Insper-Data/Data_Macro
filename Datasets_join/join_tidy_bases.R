
# Script para juntar as bases que vamos utilizar
rm(list=ls())

#libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(transformr)


# Chegando no wd do computador obs.: modificar para o seu endere?o
# maria
setwd("/Users/mariaclara/Documents/InsperData/DataMacro")
# gabi
#setwd("C:/Users/gabri/Documents/Insper_Data/Macro/projeto_econometria/bases.csv")


## Puxando as bases
debt_prop_Q <- read.csv("tsuda_tidy.csv")  # ja esta tidy  

weo_am <- readxl::read_xlsx("WEO_Data_Paises_AM.xlsx")  # Observar que esta em .xlsx

weo_em <- readxl::read_xlsx("WEO_Data_Paises_EM.xlsx")  # Observar que esta em .xlsx

taxes <- readxl::read_xlsx("taxes.xlsx")  # Observar que esta em .xlsx

gov_index <- readxl::read_xlsx("gov_index.xlsx")  # Observar que esta em .xlsx

vix <- readxl::read_xlsx("vix.xlsx")  # Observar que esta em .xlsx

GDP_per_cap <- readxl::read_xlsx("GDP_per_cap_WB.xlsx")  # Observar que esta em .xlsx

continents <- read.csv("continents.csv")

volatilidade_cambio <- read.csv("volatilidade_cambio.csv")


#arrumando base da volatilidade do cambio

volatilidade_cambio <- volatilidade_cambio %>% 
  mutate(country=as.character(country)) 

volatilidade_cambio <- volatilidade_cambio %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Czechia', replacement = "Czech Republic")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Netherlands \\(The\\)', replacement = "Netherlands")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'United Kingdom Of Great Britain And Northern Ireland \\(The\\)', replacement = "United Kingdom")) %>%
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Korea \\(The Republic Of\\)', replacement = "Korea")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Philippines \\(The\\)', replacement = "Philippines")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Russian Federation \\(The\\)', replacement = "Russia"))


  
  
#'anualizando' os dados da base do tsuda

debt_prop <- debt_prop_Q %>% 
  separate(yearQ, into = c("year", "quarter"), sep = "Q", remove = FALSE) %>% 
  group_by(country, year) %>% 
  mutate(total_debt= mean(total_debt), 
         debt_to_GDP=mean(debt_to_GDP),
         fx=mean(fx),
         nonbank_domestic_debt=mean(nonbank_domestic_debt),
         bank_domestic_debt=mean(bank_domestic_debt),
         official_domestic_debt=mean(official_domestic_debt),
         domestic_debt=mean(domestic_debt),
         nonbank_foreign_debt=mean(nonbank_foreign_debt),
         bank_foreign_debt=mean(bank_foreign_debt),
         official_foreign_debt=mean(official_foreign_debt),
         foreign_debt=mean(foreign_debt))

 
# Arrumando a df debt_prop
debt_prop <- debt_prop %>% 
  mutate(year = as.integer(year), quarter = as.integer(quarter))

debt_prop <- debt_prop %>% 
  filter(quarter == 4)

## Arrumando weo_am
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

weo_am <- weo_am %>% 
  mutate(develop="AM")


## Arrumando weo_em
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

weo_em <- weo_em %>% 
  mutate(develop = "EM")


# Bind em weo_am e weo_am:
weo <-  rbind(weo_am, weo_em)


# Agora precisamos fazer o join entre weo e debt_prop:

# Agora o join pela base debt_prop:
dataset_total <- debt_prop %>% 
  left_join(weo, by = c("country", "year"))
# Obs. paises que não estavam na weo: Latvia, Lithuania, Norway

# Mudando os nomes das variaveis que vieram da WEO

dataset_total <- dataset_total %>% 
  rename (woe_country_code = 'WEO Country Code',
          GDP_cte = 'Gross domestic product, constant prices',
          GDP_cur = 'Gross domestic product, current prices',
          GDP_per_cap_cte = 'Gross domestic product per capita, constant prices',
          inflation_mean = 'Inflation, average consumer prices',
          inflation_end = 'Inflation, end of period consumer prices',
          unemployment = 'Unemployment rate',
          lending_borroeing_rate = 'General government net lending/borrowing',
          account_balance = 'Current account balance')


# Adicionando log nas variaveis de GDP

dataset_total <- dataset_total %>% 
  mutate(ln_GDP_cte = ifelse(GDP_cte > 0, log(GDP_cte), NA),
         ln_GDP_cur = ifelse(GDP_cur > 0, log(GDP_cur), NA),
         ln_GDP_per_cap_cte = ifelse(GDP_per_cap_cte > 0, log(GDP_per_cap_cte), NA))


# Base de impostos

taxes <- taxes %>% 
  rename(year=1, taxes=3)

dataset_total <- dataset_total %>% 
  left_join(taxes, by = c("country", "year"))

# VIX

vix<-vix %>% 
  separate(year, into = c("year", "m", "d"), sep = "-", remove = TRUE)
  
vix <- vix %>% 
  select(1,4,5)

vix<- vix %>% 
  mutate(year = as.numeric(year))
  

dataset_total <- dataset_total %>% 
  left_join(vix, by = c("year"))



# Puxando a base de taxa de juros nominais

interest_rates <- readxl::read_xlsx("Interest_Rate_Nom.xlsx")

interest_rates_tidy <-  interest_rates %>% 
  pivot_longer(("2000":"2020M08"), 
               names_to = "year",
               values_to = "nominal_rate") %>% 
  rename(country = 1)


interest_rates_tidy <- interest_rates_tidy %>% 
  filter(str_length(year) <= 4) %>% 
  mutate(year = as.numeric(year))


interest_rates_tidy <- interest_rates_tidy %>% 
  na_if("...") %>% 
  na_if("-") %>% 
  mutate(nominal_rate = as.numeric(nominal_rate)) %>% 
  mutate(country = str_replace_all(string = country, pattern = "China, P.R.: Mainland", replacement = "China")) %>% 
  mutate(country = str_replace_all(string = country, pattern = "Egypt, Arab Rep. of", replacement = "Egypt")) %>% 
  mutate(country = str_replace_all(string = country, pattern = "Poland, Rep. of", replacement = "Poland")) %>% 
  mutate(country = str_replace_all(string = country, pattern = "Russian Federation", replacement = "Russia")) %>%
  mutate(country = str_replace_all(string = country, pattern = "Czech Rep.", replacement = "Czech Republic")) %>%
  mutate(country = str_replace_all(string = country, pattern = "Korea, Rep. of", replacement = "Korea"))

dataset_total <- dataset_total %>% 
  left_join(interest_rates_tidy, by = c("year", "country"))


# Adicionando os continentes

continents <- continents %>% 
  rename(continent=1) %>% 
  rename(country=2)

dataset_total <- dataset_total %>% 
  left_join(continents, by="country")
  
dataset_total <-dataset_total %>% 
  mutate(nominal_rate=as.numeric(nominal_rate))


# GDP per cap
GDP_per_cap <- GDP_per_cap %>% 
  rename(year = 1 , country = 3 , GDP_per_cap_cur_USD = 5)

GDP_per_cap <- GDP_per_cap %>% 
  mutate(GDP_per_cap_cur_USD = as.numeric(GDP_per_cap_cur_USD)) %>% 
  mutate(ln_GDP_per_cap_cur = log(GDP_per_cap_cur_USD))

GDP_per_cap <- GDP_per_cap %>% 
  select(1,3,5,6)

dataset_total <- dataset_total %>% 
  left_join(GDP_per_cap, by=c("country", "year"))



# Base de indicadores diversos no World Bank

gov_index <- gov_index %>% 
  select(1,3,7,13, 19, 25, 31, 37)

gov_index <- gov_index %>% 
  rename(year=1, country=2, control_corruption_rank=3, gov_effectiveness_rank=4, political_stability_rank=5, regulatory_quality_rank=6, rule_of_law_rank=7, voice_rank=8)

dataset_total_index <- dataset_total %>% 
  left_join(gov_index, by = c("country", "year"))

dataset_total_index <- dataset_total %>% 
  mutate(taxes = as.numeric(taxes)) %>% 
  mutate(control_corruption_rank = as.numeric(control_corruption_rank)) %>% 
  mutate(gov_effectiveness_rank = as.numeric(gov_effectiveness_rank)) %>% 
  mutate(political_stability_rank = as.numeric(political_stability_rank)) %>% 
  mutate(regulatory_quality_rank = as.numeric(regulatory_quality_rank)) %>% 
  mutate(rule_of_law_rank = as.numeric(rule_of_law_rank)) %>% 
  mutate(voice_rank=as.numeric(voice_rank))


 
# Colocando NA nas observações

dataset_total<- dataset_total %>% 
  na_if("..")

dataset_total_index<- dataset_total_index %>% 
  na_if("..")


# Arrumando a inflacao e GDP per capita
dataset_total <- dataset_total %>% 
  mutate(inflation_mean =(inflation_mean/1000), inflation_end=(inflation_end/1000),
         account_balance = (account_balance/1000), lending_borroeing_rate = (lending_borroeing_rate/1000), 
         unemployment = (unemployment/1000))

#Acrescentando dummies

dataset_total <- dataset_total %>% 
  mutate(yearnum = year)

dataset_total <- dataset_total %>% 
  mutate(post_08 = ifelse(yearnum >= 2008, "YES", "NO"),
         post_09 = ifelse(yearnum >= 2009, "YES", "NO"),
         post_15 = ifelse(yearnum >= 2015, "YES", "NO"),
         post_16 = ifelse(yearnum >= 2016, "YES", "NO"),
         post_17 = ifelse(yearnum >= 2017, "YES", "NO")) %>% 
  mutate(post_08 = as.factor(post_08),
         post_09 = as.factor(post_09),
         post_15 = as.factor(post_15),
         post_16 = as.factor(post_16),
         post_17 = as.factor(post_17))


#adicionando a volatilidade do cambio

dataset_total <- dataset_total %>% 
  left_join(volatilidade_cambio, by=c("country", "year"))

dataset_total <- dataset_total %>% 
  group_by() %>% 
  mutate(fx_volatility = ifelse(country == "United States", 0, fx_volatility),
         ticker = ifelse(country == "United States", "USD", ticker))

dataset_total <- dataset_total %>%
  filter(country != "Latvia") %>%  
           filter(country != "Lithuania") %>% 
                    filter(country != "Norway")

dataset_total <- dataset_total %>% 
  distinct()

dataset_total_index <- dataset_total_index %>% 
  distinct()

# Escrevendo um arquivo csv para dataset_total:
write_csv(dataset_total, "dataset_total.csv", append = F)

# Escrevendo um arquivo csv para dataset_total_index:
write_csv(dataset_total_index, "dataset_total_index.csv", append = F)
 #fim





