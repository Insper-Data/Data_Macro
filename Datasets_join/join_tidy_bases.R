
# Script para juntar as bases que vamos utilizar

#libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)

#chegando no wd do computador (modificar)
setwd("C:/Users/gutao/OneDrive - Insper - Institudo de Ensino e Pesquisa/Documents/Insper/Insper Data/Projeto Macro 2020.1/Data/Bases.csv")


##puxando as bases
debt_prop <- read.csv("tsuda_tidy.csv")  # ja esta tidy  

weo_am <- readxl::read_xlsx("WEO_Data_Paises_AM.xlsx")  # Observar que esta em .xlsx

weo_em <- readxl::read_xlsx("WEO_Data_Paises_EM.xlsx")  # Observar que esta em .xlsx


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
# Obs. paises que não estavam na weo: Latvia, Lithuania, Norway


# Escrevendo um arquivo csv para dataset_total:
#write_csv(dataset_total, "dataset_total.csv")


# Agora precisamos arrumar os nomes das colunas que vieram da WEO, estao muito grandes








 











