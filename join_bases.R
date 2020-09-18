# Script para juntar as bases que vamos utilizar

#chegando no wd do computador (modificar)

getwd()
setwd("C:/Users/gabri/Documents/Insper_Data/Macro/projeto_econometria/bases.csv")

#libraries
library(tidyverse)
library(transformr)

##puxando as bases
debt_prop <- read_csv("tsuda_tidy.csv")

weo_am <- read_delim("WEO_Data_Paises_AM.csv", delim = ";")

weo_em <- read_delim("WEO_Data_Paises_EM.csv", delim = ";")

#arrumando weo_am













