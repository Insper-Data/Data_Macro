rm(list=ls())

#puxando as libraries
library(tidyverse)
library(BatchGetSymbols)


#volatilidade do câmbio

my.tickers <- c('BRL=X', 'ARS=X', 'BGN=X', 'CLP=X', 'CNY=X', 'COP=X', 
                'EGP=X', 'HUF=X', 'INR=X', 'LVL=X', 'LTL=X', 'MYL=X',
                'MXN=X', 'PEN=X', 'PHP=X', 'PLN=X', 'RON=X', 'RUB=X',
                'ZAR=X', 'THB=X', 'TRY=X', 'UAH=X', 'UYU=X', 'AUD=X',
                'CAD=X', 'EUR=X', 'CZK=X', 'DKK=X', 'FIM=X', 'JPY=X',
                'KRW=X', 'NZD=X', 'NOK=X', 'SEK=X', 'GBP=X',
                'USD=X', 'IDR=X', 'CHF=X', 'MYR=X', 'PHP=X', 'KWR=X')

dados <- BatchGetSymbols( tickers = my.tickers, first.date = "2004-01-01", last.date = "2019-12-31", freq.data = "daily", how.to.aggregate = "last", 
                         do.complete.data = TRUE)


df <- print(dados$df.tickers)

df <- df %>% 
  select (1,2,6)

df <- df %>% 
  separate(ref.date, into = c("year", "month", "day"), sep="-") 

df <- df %>% 
  select(1,2,5) %>% 
  filter(price.close != "NA")

df <- df %>% 
  group_by(ticker, year) %>% 
  dplyr::summarise(fx_volatility=sd(price.close))



df <- df %>% 
  separate(ticker, into = c("ticker", "X"), sep="=")

df <- df %>% 
  select(1,3,4)

#juntando com os paises
 
countries <- read.csv("https://raw.githubusercontent.com/datasets/currency-codes/master/data/codes-all.csv")

countries <- countries %>% 
  select(1,3)

countries <- countries %>% 
  rename(country=1, ticker=2)

countries <- countries %>% 
  mutate(country=tolower(country)) %>% 
  mutate(country=str_to_title(country))

df <- df %>% 
  left_join(countries, by=("ticker"))


#criando a base csv
setwd("C:/Users/gabri/Documents/Insper_Data/Macro/projeto_econometria/bases.csv")


write_csv(df, "volatilidade_cambio.csv")

