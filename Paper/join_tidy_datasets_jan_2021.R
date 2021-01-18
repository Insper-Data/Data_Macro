
# JANUARY 2021 DATASET VERSION 

# Script to join all datasets used in our paper

# Authors: Augusto Netto, Gabriela Garcia, Maria Clara Drzeviechi and Victor H. Alexandrino

#--------------------------------------------------------------------------------------------

# Cleaning environment:
rm(list=ls())

# Libraries
library(tidyverse)
library(transformr)
library(stringi)
library(skimr)
library(transformr)

# Calling datasets
debt_prop_Q <- read.csv("tsuda_tidy.csv")  # already tidy  
weo_oct_2020 <- readxl::read_xlsx("WEOOct2020all.xlsx")
taxes <- readxl::read_xlsx("taxes.xlsx")
gov_index <- readxl::read_xlsx("gov_index.xlsx")
vix <- readxl::read_xlsx("vix.xlsx")
continents <- read.csv("continents.csv")
fx_volatility <- read.csv("volatilidade_cambio.csv")
real_interest_rates <- readxl::read_xlsx("real_interest_rates.xlsx")
International_Liquidity <- readxl::read_xlsx("International_Liquidity.xlsx")
interest_rates <- readxl::read_xlsx("Interest_Rate_Nom.xlsx")


#--------------------------------------------------------------------------------------------
      # TIDYING & JOINING USED DATASETS
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------

# Annualizing Arslanalp & Tsuda dataset

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

#--------------------------------------------------------------------------------------------

# Tidying debt_prop dataset (it will be base dataset for joins)
debt_prop <- debt_prop %>% 
  mutate(year = as.integer(year), quarter = as.integer(quarter))

debt_prop <- debt_prop %>% 
  filter(quarter == 4)

#--------------------------------------------------------------------------------------------

# Tidying WEO Oct 2020
weo_oct_2020 <- weo_oct_2020 %>%
  select(-c('2022':'Estimates Start After')) %>% 
  pivot_longer('2002':'2021', names_to = "year", values_to = "value")

weo_oct_2020 <- weo_oct_2020 %>%
  na_if('--') %>% 
  na_if('n/a') %>%
  rename(sub_description = `Subject Descriptor`, sub_notes = `Subject Notes`, country = Country) %>% 
  mutate(value = str_replace_all(string = value, pattern = ",", replacement = "")) 
  
weo_oct_2020 <- weo_oct_2020 %>%
  filter(sub_notes == "Expressed in billions of national currency units; the base year is country-specific. Expenditure-based GDP is total final expenditures at purchasers' prices (including the f.o.b. value of exports of goods and services), less the f.o.b. value of imports of goods and services. [SNA 1993]" | 
         sub_notes == "Expressed in billions of national currency units. Expenditure-based GDP is total final expenditures at purchasers' prices (including the f.o.b. value of exports of goods and services), less the f.o.b. value of imports of goods and services. [SNA 1993]" |
         sub_notes == "Values are based upon GDP in national currency converted to U.S. dollars using market exchange rates (yearly average). Exchange rate projections are provided by country economists for the group of other emerging market and developing countries. Exchanges rates for advanced economies are established in the WEO assumptions for each WEO exercise. Expenditure-based GDP is total final expenditures at purchasers' prices (including the f.o.b. value of exports of goods and services), less the f.o.b. value of imports of goods and services. [SNA 1993]" | 
         sub_notes == "GDP is expressed in constant national currency per person. Data are derived by dividing constant price GDP by total population." |
         sub_notes == "GDP is expressed in current national currency per person. Data are derived by dividing current price GDP by total population." |
         sub_notes == "GDP is expressed in current U.S. dollars per person. Data are derived by first converting GDP in national currency to U.S. dollars and then dividing it by total population." |
         sub_notes == "Expressed as a ratio of total investment in current local currency and GDP in current local currency. Investment or gross capital formation is measured by the total value of the gross fixed capital formation and changes in inventories and acquisitions less disposals of valuables for a unit or sector. [SNA 1993]" |
         sub_notes == "Expressed as a ratio of gross national savings in current local currency and GDP in current local currency. Gross national saving is gross disposable income less final consumption expenditure after taking account of an adjustment for pension funds. [SNA 1993] For many countries, the estimates of national saving are built up from national accounts data on gross domestic investment and from balance of payments-based data on net foreign investment." |
         sub_notes == "Expressed in averages for the year, not end-of-period data. A consumer price index (CPI) measures changes in the prices of goods and services that households consume. Such changes affect the real purchasing power of consumers' incomes and their welfare. As the prices of different goods and services do not all change at the same rate, a price index can only reflect their average movement. A price index is typically assigned a value of unity, or 100, in some reference period and the values of the index for other periods of time are intended to indicate the average proportionate, or percentage, change in prices from this price reference period. Price indices can also be used to measure differences in price levels between different cities, regions or countries at the same point in time. [CPI Manual 2004, Introduction] For euro countries, consumer prices are calculated based on harmonized prices. For more information see http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-BE-04-001/EN/KS-BE-04-001-EN.PDF.]" |
         sub_notes == "Expressed in end of the period, not annual average data. A consumer price index (CPI) measures changes in the prices of goods and services that households consume. Such changes affect the real purchasing power of consumers' incomes and their welfare. As the prices of different goods and services do not all change at the same rate, a price index can only reflect their average movement. A price index is typically assigned a value of unity, or 100, in some reference period and the values of the index for other periods of time are intended to indicate the average proportionate, or percentage, change in prices from this price reference period. Price indices can also be used to measure differences in price levels between different cities, regions or countries at the same point in time. [CPI Manual 2004, Introduction] For euro countries, consumer prices are calculated based on harmonized prices. For more information see http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-BE-04-001/EN/KS-BE-04-001-EN.PDF." |
         sub_notes == "Unemployment rate can be defined by either the national definition, the ILO harmonized definition, or the OECD harmonized definition. The OECD harmonized unemployment rate gives the number of unemployed persons as a percentage of the labor force (the total number of people employed plus unemployed). [OECD Main Economic Indicators, OECD, monthly] As defined by the International Labour Organization, unemployed workers are those who are currently not working but are willing and able to work for pay, currently available to work, and have actively searched for work. [ILO, http://www.ilo.org/public/english/bureau/stat/res/index.htm]" |
         sub_notes == "Revenue consists of taxes, social contributions, grants receivable, and other revenue. Revenue increases government's net worth, which is the difference between its assets and liabilities (GFSM 2001, paragraph 4.20). Note: Transactions that merely change the composition of the balance sheet do not change the net worth position, for example, proceeds from sales of nonfinancial and financial assets or incurrence of liabilities." |
         sub_notes == "Total expenditure consists of total expense and the net acquisition of nonfinancial assets. Note: Apart from being on an accrual basis, total expenditure differs from the GFSM 1986 definition of total expenditure in the sense that it also takes the disposals of nonfinancial assets into account." |
         sub_notes == "Net lending (+)/ borrowing (-) is calculated as revenue minus total expenditure. This is a core GFS balance that measures the extent to which general government is either putting financial resources at the disposal of other sectors in the economy and nonresidents (net lending), or utilizing the financial resources generated by other sectors and nonresidents (net borrowing). This balance may be viewed as an indicator of the financial impact of general government activity on the rest of the economy and nonresidents (GFSM 2001, paragraph 4.17). Note: Net lending (+)/borrowing (-) is also equal to net acquisition of financial assets minus net incurrence of liabilities." |
         sub_notes == "Current account is all transactions other than those in financial and capital items. The major classifications are goods and services, income and current transfers. The focus of the BOP is on transactions (between an economy and the rest of the world) in goods, services, and income.")

weo_oct_2020 <- weo_oct_2020 %>%
  mutate(sub_description = ifelse(sub_notes == "Expressed in billions of national currency units; the base year is country-specific. Expenditure-based GDP is total final expenditures at purchasers' prices (including the f.o.b. value of exports of goods and services), less the f.o.b. value of imports of goods and services. [SNA 1993]",
                                  "GDP_cte_billions", sub_description),
         sub_description = ifelse(sub_notes == "Expressed in billions of national currency units. Expenditure-based GDP is total final expenditures at purchasers' prices (including the f.o.b. value of exports of goods and services), less the f.o.b. value of imports of goods and services. [SNA 1993]",
                                  "GDP_cur_billions", sub_description),
         sub_description = ifelse(sub_notes == "Values are based upon GDP in national currency converted to U.S. dollars using market exchange rates (yearly average). Exchange rate projections are provided by country economists for the group of other emerging market and developing countries. Exchanges rates for advanced economies are established in the WEO assumptions for each WEO exercise. Expenditure-based GDP is total final expenditures at purchasers' prices (including the f.o.b. value of exports of goods and services), less the f.o.b. value of imports of goods and services. [SNA 1993]",
                                  "GDP_cur_USD_billions", sub_description),
         sub_description = ifelse(sub_notes == "GDP is expressed in constant national currency per person. Data are derived by dividing constant price GDP by total population.",
                                  "GDP_percapita_cte", sub_description),
         sub_description = ifelse(sub_notes == "GDP is expressed in current national currency per person. Data are derived by dividing current price GDP by total population.",
                                  "GDP_percapita_cur", sub_description),
         sub_description = ifelse(sub_notes == "GDP is expressed in current U.S. dollars per person. Data are derived by first converting GDP in national currency to U.S. dollars and then dividing it by total population.",
                                  "GDP_percapita_cur_USD", sub_description),
         sub_description = ifelse(sub_notes == "Expressed as a ratio of total investment in current local currency and GDP in current local currency. Investment or gross capital formation is measured by the total value of the gross fixed capital formation and changes in inventories and acquisitions less disposals of valuables for a unit or sector. [SNA 1993]",
                                  "total_investment_percent_GDP", sub_description),
         sub_description = ifelse(sub_notes == "Expressed as a ratio of gross national savings in current local currency and GDP in current local currency. Gross national saving is gross disposable income less final consumption expenditure after taking account of an adjustment for pension funds. [SNA 1993] For many countries, the estimates of national saving are built up from national accounts data on gross domestic investment and from balance of payments-based data on net foreign investment.",
                                  "gross_national_savings_percent_GDP", sub_description),
         sub_description = ifelse(sub_notes == "Expressed in averages for the year, not end-of-period data. A consumer price index (CPI) measures changes in the prices of goods and services that households consume. Such changes affect the real purchasing power of consumers' incomes and their welfare. As the prices of different goods and services do not all change at the same rate, a price index can only reflect their average movement. A price index is typically assigned a value of unity, or 100, in some reference period and the values of the index for other periods of time are intended to indicate the average proportionate, or percentage, change in prices from this price reference period. Price indices can also be used to measure differences in price levels between different cities, regions or countries at the same point in time. [CPI Manual 2004, Introduction] For euro countries, consumer prices are calculated based on harmonized prices. For more information see http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-BE-04-001/EN/KS-BE-04-001-EN.PDF.]",
                                  "inflation_average_index", sub_description),
         sub_description = ifelse(sub_notes == "Expressed in end of the period, not annual average data. A consumer price index (CPI) measures changes in the prices of goods and services that households consume. Such changes affect the real purchasing power of consumers' incomes and their welfare. As the prices of different goods and services do not all change at the same rate, a price index can only reflect their average movement. A price index is typically assigned a value of unity, or 100, in some reference period and the values of the index for other periods of time are intended to indicate the average proportionate, or percentage, change in prices from this price reference period. Price indices can also be used to measure differences in price levels between different cities, regions or countries at the same point in time. [CPI Manual 2004, Introduction] For euro countries, consumer prices are calculated based on harmonized prices. For more information see http://epp.eurostat.ec.europa.eu/cache/ITY_OFFPUB/KS-BE-04-001/EN/KS-BE-04-001-EN.PDF.",
                                  "inflation_end_index", sub_description),
         sub_description = ifelse(sub_notes == "Unemployment rate can be defined by either the national definition, the ILO harmonized definition, or the OECD harmonized definition. The OECD harmonized unemployment rate gives the number of unemployed persons as a percentage of the labor force (the total number of people employed plus unemployed). [OECD Main Economic Indicators, OECD, monthly] As defined by the International Labour Organization, unemployed workers are those who are currently not working but are willing and able to work for pay, currently available to work, and have actively searched for work. [ILO, http://www.ilo.org/public/english/bureau/stat/res/index.htm]",
                                  "unemployment_rate", sub_description),
         sub_description = ifelse(sub_notes == "Revenue consists of taxes, social contributions, grants receivable, and other revenue. Revenue increases government's net worth, which is the difference between its assets and liabilities (GFSM 2001, paragraph 4.20). Note: Transactions that merely change the composition of the balance sheet do not change the net worth position, for example, proceeds from sales of nonfinancial and financial assets or incurrence of liabilities." & Units == "National currency",
                                  "general_gov_revenue_billions", sub_description),
         sub_description = ifelse(sub_notes == "Revenue consists of taxes, social contributions, grants receivable, and other revenue. Revenue increases government's net worth, which is the difference between its assets and liabilities (GFSM 2001, paragraph 4.20). Note: Transactions that merely change the composition of the balance sheet do not change the net worth position, for example, proceeds from sales of nonfinancial and financial assets or incurrence of liabilities." & Units == "Percent of GDP",
                                  "general_gov_revenue_percent_GDP", sub_description),
         sub_description = ifelse(sub_notes == "Total expenditure consists of total expense and the net acquisition of nonfinancial assets. Note: Apart from being on an accrual basis, total expenditure differs from the GFSM 1986 definition of total expenditure in the sense that it also takes the disposals of nonfinancial assets into account." & Units == "National currency",
                                  "general_gov_expenditures_billions", sub_description),
         sub_description = ifelse(sub_notes == "Total expenditure consists of total expense and the net acquisition of nonfinancial assets. Note: Apart from being on an accrual basis, total expenditure differs from the GFSM 1986 definition of total expenditure in the sense that it also takes the disposals of nonfinancial assets into account." & Units == "Percent of GDP",
                                  "general_gov_expenditures_percent_GDP", sub_description),
         sub_description = ifelse(sub_notes == "Net lending (+)/ borrowing (-) is calculated as revenue minus total expenditure. This is a core GFS balance that measures the extent to which general government is either putting financial resources at the disposal of other sectors in the economy and nonresidents (net lending), or utilizing the financial resources generated by other sectors and nonresidents (net borrowing). This balance may be viewed as an indicator of the financial impact of general government activity on the rest of the economy and nonresidents (GFSM 2001, paragraph 4.17). Note: Net lending (+)/borrowing (-) is also equal to net acquisition of financial assets minus net incurrence of liabilities." & Units == "National currency",
                                  "lending_borrowing_billions", sub_description),
         sub_description = ifelse(sub_notes == "Net lending (+)/ borrowing (-) is calculated as revenue minus total expenditure. This is a core GFS balance that measures the extent to which general government is either putting financial resources at the disposal of other sectors in the economy and nonresidents (net lending), or utilizing the financial resources generated by other sectors and nonresidents (net borrowing). This balance may be viewed as an indicator of the financial impact of general government activity on the rest of the economy and nonresidents (GFSM 2001, paragraph 4.17). Note: Net lending (+)/borrowing (-) is also equal to net acquisition of financial assets minus net incurrence of liabilities." & Units == "Percent of GDP",
                                  "lending_borrowing_percent_GDP", sub_description),
         sub_description = ifelse(sub_notes == "Current account is all transactions other than those in financial and capital items. The major classifications are goods and services, income and current transfers. The focus of the BOP is on transactions (between an economy and the rest of the world) in goods, services, and income." & Units == "U.S. dollars",
                                  "current_account_billions", sub_description),
         sub_description = ifelse(sub_notes == "Current account is all transactions other than those in financial and capital items. The major classifications are goods and services, income and current transfers. The focus of the BOP is on transactions (between an economy and the rest of the world) in goods, services, and income." & Units == "Percent of GDP",
                                  "current_account_percent_GDP", sub_description))

weo_oct_2020 <- weo_oct_2020 %>% 
  mutate(value = as.numeric(value), year = as.integer(year)) %>% 
  filter(!is.na(country))  

weo_oct_2020 <- weo_oct_2020 %>%
  select(`WEO Country Code`, country, year, sub_description, value) %>% 
  pivot_wider(names_from = sub_description, values_from = value)

weo_oct_2020 <- weo_oct_2020 %>%
  mutate(GDP_cte_billions = GDP_cte_billions/1000,
         GDP_cur_billions = GDP_cur_billions/1000,
         GDP_cur_USD_billions = GDP_cur_USD_billions/1000,
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000,
         total_investment_percent_GDP = total_investment_percent_GDP/1000,
         gross_national_savings_percent_GDP = gross_national_savings_percent_GDP/1000,
         inflation_average_index = inflation_average_index/1000,
         inflation_end_index = inflation_end_index/1000,
         unemployment_rate = unemployment_rate/1000,
         general_gov_revenue_billions = general_gov_revenue_billions/1000,
         general_gov_revenue_percent_GDP = general_gov_revenue_percent_GDP/1000,
         general_gov_expenditures_billions = general_gov_expenditures_billions/1000,
         general_gov_expenditures_percent_GDP = general_gov_expenditures_percent_GDP/1000,
         lending_borrowing_billions = lending_borrowing_billions/1000,
         lending_borrowing_percent_GDP = lending_borrowing_percent_GDP/1000,
         current_account_billions = current_account_billions/1000,
         current_account_percent_GDP = current_account_percent_GDP/1000)

weo_oct_2020 <- weo_oct_2020 %>%
  mutate(ln_GDP_cte_billions = log(GDP_cte_billions),
         ln_GDP_cur_billions = log(GDP_cur_billions),
         ln_GDP_percapita_cur_USD = log(GDP_percapita_cur_USD))

# Joining Arslanalp & Tsuda with WEO Oct 2020
dataset_total <- debt_prop %>% 
  left_join(weo_oct_2020, by = c("country", "year"))

#--------------------------------------------------------------------------------------------

# Tidying taxes dataset
taxes <- taxes %>% 
  rename(year=1, taxes=3)

# Joining taxes with dataset_total
dataset_total <- dataset_total %>% 
  left_join(taxes, by = c("country", "year"))

#--------------------------------------------------------------------------------------------

# Tidying VIX dataset
vix <- vix %>% 
  separate(year, into = c("year", "m", "d"), sep = "-", remove = TRUE)

vix <- vix %>% 
  select(1,4,5)

vix <- vix %>% 
  mutate(year = as.numeric(year))

# Joining VIX with dataset_total
dataset_total <- dataset_total %>% 
  left_join(vix, by = c("year"))

#--------------------------------------------------------------------------------------------

# Tidying nominal interest rates dataset
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

# Joining nominal interest rates with dataset_total
dataset_total <- dataset_total %>% 
  left_join(interest_rates_tidy, by = c("year", "country"))

#--------------------------------------------------------------------------------------------

# Tidying continents dataset
continents <- continents %>% 
  rename(continent=1) %>% 
  rename(country=2)

# Joining continents with dataset_total
dataset_total <- dataset_total %>% 
  left_join(continents, by="country")

#--------------------------------------------------------------------------------------------

# Tidying governance indicators (WB) dataset
gov_index <- gov_index %>% 
  select(1, 3, 7, 13, 19, 25, 31, 37)

gov_index <- gov_index %>% 
  rename(year=1, country=2, control_corruption_rank=3, gov_effectiveness_rank=4, political_stability_rank=5, regulatory_quality_rank=6, rule_of_law_rank=7, voice_rank=8)

# Joining continents with dataset_total
dataset_total <- dataset_total %>% 
  left_join(gov_index, by = c("country", "year"))

# For some reason we cannot explain, variables joined as characters. We changed it below
dataset_total <- dataset_total %>% 
  mutate(control_corruption_rank = as.numeric(control_corruption_rank)) %>% 
  mutate(gov_effectiveness_rank = as.numeric(gov_effectiveness_rank)) %>% 
  mutate(political_stability_rank = as.numeric(political_stability_rank)) %>% 
  mutate(regulatory_quality_rank = as.numeric(regulatory_quality_rank)) %>% 
  mutate(rule_of_law_rank = as.numeric(rule_of_law_rank)) %>% 
  mutate(voice_rank=as.numeric(voice_rank))

#--------------------------------------------------------------------------------------------

# Tidying FX volatility
fx_volatility <- fx_volatility %>% 
  mutate(country=as.character(country)) 

fx_volatility <- fx_volatility %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Czechia', replacement = "Czech Republic")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Netherlands \\(The\\)', replacement = "Netherlands")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'United Kingdom Of Great Britain And Northern Ireland \\(The\\)', replacement = "United Kingdom")) %>%
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Korea \\(The Republic Of\\)', replacement = "Korea")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Philippines \\(The\\)', replacement = "Philippines")) %>% 
  dplyr::mutate(country = str_replace_all(string = country, pattern = 'Russian Federation \\(The\\)', replacement = "Russia"))

# Joining FX volatility with dataset_total

dataset_total <- dataset_total %>% 
  left_join(fx_volatility, by=c("country", "year"))

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

#--------------------------------------------------------------------------------------------

# Tidying real interest rates

real_interest_rates <- real_interest_rates %>% 
  rename(year = Year) %>% 
  mutate(country = str_replace_all(string = country, pattern = "Egypt, Arab Rep.", replacement = "Egypt")) %>%
  mutate(country = str_replace_all(string = country, pattern = "Russian Federation", replacement = "Russia")) %>%
  mutate(country = str_replace_all(string = country, pattern = "Korea, Rep.", replacement = "Korea"))

# Joining real interest rates with dataset_total
dataset_total <- dataset_total %>% 
  left_join(real_interest_rates, by = c("year", "country")) %>% 
  select(country, year, real_interest_rate, everything()) %>% 
  na_if("..")

#--------------------------------------------------------------------------------------------

# Tidying international reserves
International_Liquidity <- International_Liquidity %>% 
  select(-Scale) %>% 
  na_if("...") %>% 
  pivot_longer(-Country, names_to = "year", values_to = "int_liq") %>% 
  filter(str_length(year) <= 4) %>% 
  rename(country = Country)

International_Liquidity <- International_Liquidity %>%
  mutate(year = as.numeric(year),
         int_liq = as.numeric(int_liq))

# Joining international reserves with dataset_total
dataset_total <- dataset_total %>% 
  left_join(International_Liquidity, by = c("year", "country"))


#--------------------------------------------------------------------------------------------
# CREATING NEW VARIABLES  
#--------------------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------

# Adding time dummies
dataset_total <- dataset_total %>% 
  mutate(post_08 = ifelse(year >= 2008, "YES", "NO"),
         post_09 = ifelse(year >= 2009, "YES", "NO"),
         post_12 = ifelse(year >= 2012, "YES", "NO"),
         post_13 = ifelse(year >= 2013, "YES", "NO"),
         post_15 = ifelse(year >= 2015, "YES", "NO"),
         post_16 = ifelse(year >= 2016, "YES", "NO"),
         post_17 = ifelse(year >= 2017, "YES", "NO")) %>% 
  mutate(post_08 = as.factor(post_08),
         post_09 = as.factor(post_09),
         post_12 = as.factor(post_12),
         post_13 = as.factor(post_13),
         post_15 = as.factor(post_15),
         post_16 = as.factor(post_16),
         post_17 = as.factor(post_17))

#--------------------------------------------------------------------------------------------

# Adding indebtedness level dummies
dataset_total <- dataset_total %>% 
  mutate(less_20 = as.factor(ifelse(debt_to_GDP < 20, "YES", "NO")),
         bet_20_40 = as.factor(ifelse(debt_to_GDP >= 20, ifelse(debt_to_GDP < 40, "YES", "NO"), "NO")),
         bet_40_60 = as.factor(ifelse(debt_to_GDP >= 40, ifelse(debt_to_GDP < 60, "YES", "NO"), "NO")),
         bet_60_80 = as.factor(ifelse(debt_to_GDP >= 40, ifelse(debt_to_GDP < 80, "YES", "NO"), "NO")),
         bet_80_100 = as.factor(ifelse(debt_to_GDP >= 60, ifelse(debt_to_GDP < 100, "YES", "NO"), "NO")),
         more_100 = as.factor(ifelse(debt_to_GDP >= 100, "YES", "NO")))

#--------------------------------------------------------------------------------------------

# Adding FX return variable
dataset_total <- dataset_total %>%
  group_by(country) %>% 
  mutate(fx_return = (lag(fx)-lag(fx, n = 2))/lag(fx))

#--------------------------------------------------------------------------------------------

# Adding EM and AM id variable
EMAM_id <- read.csv("dataset_total.csv")

EMAM_id <- EMAM_id %>% 
  select(country, yearnum, develop) %>% 
  rename(year = yearnum)

# Joining EM and AM id variable with dataset_total
dataset_total <- dataset_total %>% 
  left_join(EMAM_id, by = c("year", "country"))


#--------------------------------------------------------------------------------------------
      # LAST ARRANGEMENTS
#--------------------------------------------------------------------------------------------

dataset_total <- dataset_total %>% 
  select(-c(yearQ, quarter, `WEO Country Code`, Scale, ticker)) %>% 
  mutate(taxes = as.numeric(taxes))


#--------------------------------------------------------------------------------------------
      # DATASET_TOTAL_JAN_2021 CSV OUTPUT
#--------------------------------------------------------------------------------------------

write_csv(dataset_total, "dataset_total_jan_2021.csv", append = F)
