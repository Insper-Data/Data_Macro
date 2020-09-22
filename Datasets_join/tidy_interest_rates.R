

#pacotes
library(tidyverse)
library(ggplot2)
library(scales)
library(readr)
library(readxl)
library(transformr)


#entrando no diretorio das bases (alterar)
getwd()
setwd("/Users/mariaclara/Documents/InsperData/DataMacro")

#puxando a base de taxa de juros nominais

interest_rates <- read_xlsx ("Interest_Rate_Nom.xlsx")

interest_rates_tidy <-  interest_rates %>% 
  pivot_longer(("2000":"2020M08"), 
               names_to = "Year",
               values_to = "nominal_rate")


interest_rates_tidy <- interest_rates_tidy %>% 
  filter(str_length(Year)<=4)

interest_rates_tidy <- interest_rates_tidy %>% 
  na_if("...")






