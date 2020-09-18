
# Script para juntar as bases que vamos utilizar

#libraries
library(tidyverse)
library(transformr)
library(stringi)

#chegando no wd do computador (modificar)
setwd("C:/Users/gutao/OneDrive - Insper - Institudo de Ensino e Pesquisa/Documents/Insper/Insper Data/Projeto Macro 2020.1/Data/Bases.csv")


##puxando as bases
debt_prop <- read_csv("tsuda_tidy.csv")  # ja esta tidy  

weo_am <- readxl::read_xlsx("WEO_Data_Paises_AM.xlsx")  # Observar que esta em .xlsx

weo_em <- readxl::read_xlsx("WEO_Data_Paises_EM.xlsx")  # Observar que esta em .xlsx


##arrumando weo_am
weo_am <- weo_am %>%
  pivot_longer('1980':'2021', names_to = "Year", values_to = "value")

weo_am <- weo_am %>% 
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = ""))
 
weo_am <- weo_am %>% 
   mutate(value = as.numeric(value), Year = as.integer(Year)) %>% 
   filter(!is.na(Country))

weo_am <- weo_am %>%
  select(`WEO Country Code`, Country, Year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)


##arrumando weo_em
weo_em <- weo_em %>%
  pivot_longer('1980':'2021', names_to = "Year", values_to = "value")

weo_em <- weo_em %>% 
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = ""))

weo_em <- weo_em %>% 
  mutate(value = as.numeric(value), Year = as.integer(Year)) %>% 
  filter(!is.na(Country))

weo_em <- weo_em %>%
  select(`WEO Country Code`, Country, Year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)


# Bind em weo_am e weo_am:
weo <-  rbind(weo_am, weo_em)


# Agora precisamos fazer o join entre weo e debt_prop...
















 











