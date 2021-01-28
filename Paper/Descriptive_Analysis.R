
# JANUARY 2021  

# Script to generate figures used in our paper

# Authors: Augusto Netto, Gabriela Garcia, Maria Clara Drzeviechi and Victor H. Alexandrino


#--------------------------------------------------------------------------------------------

# Libraries
library(scales)
library(dplyr)
library(ggthemes)
library(spData)
library(readr)
library(readxl)
library(spData)
library(plotly)
library(sf)
library(viridis)
library(ggcharts)
library(ggrepel)
library(cowplot)
library(tidyr)
library(dplyr)
library(grid)
library(forecast)

# Calling our dataset
dataset_total_jan_2021 <- read.csv("https://raw.githubusercontent.com/Insper-Data/Data_Macro/master/Paper/Datasets/dataset_total_jan_2021.csv") 

dataset_total <- dataset_total_jan_2021 

#--------------------------------------------------------------------------------------------
#     STYLIZED FACTS ABOUT THE DATABASE
#--------------------------------------------------------------------------------------------

# 1. The importance of debt
debt_to_gdp_graph <- dataset_total %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(debt_to_GDP = mean(debt_to_GDP)) %>% 
  ggplot(aes(x = year, y = debt_to_GDP, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Debt-to-GDP Ratio (%)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=2), name = "Year") +
  ylim(30,90)+
  theme_bw()

debt_to_gdp_graph

# 2. 

dataset_total %>%
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(develop)) %>%
  rename(Development = develop) %>% 
  bar_chart(x = country, y = debt_to_GDP, facet = year, top_n = 10, fill = Development) +
  labs( x = "", y = "Debt-to-GDP Ratio (%)",
        title = "", fill = "Development") +
  theme_classic() +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))+
  theme(legend.title = element_text(face = "bold", size = 10))

# 3.

graph_debt_foreign_pp <- dataset_total %>% 
  filter(!is.na(develop)) %>% 
  rename(Development = develop) %>% 
  group_by(year, Development) %>% 
  summarise(foreign_participation_percent_GDP = mean(foreign_participation_percent_GDP)) %>% 
  ggplot(aes(x = year, y = foreign_participation_percent_GDP, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_line()+
  labs(x = "Year", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004,2019,by=2), name = "Year") +
  #ylim(50,160)+
  theme_light()

graph_debt_foreign_pp



# 4.

dataset_total %>%
  rename(Development = develop) %>% 
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(Development)) %>%
  bar_chart(x = country, y = (foreign_participation_percent_GDP), facet = year, top_n = 10, fill = Development) +  
  labs( x = "", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)",
        title = "", fill = "Development") +
  theme_classic() +
  theme(legend.title = element_text(face = "bold", size = 10)) +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))



################################################ FUNDAMENTALS ################################################################################################################

# 5.1
dataset_total %>%
  filter(develop == "AM") %>%
  ggplot(aes(x = fx_volatility, y = foreign_participation_percent_GDP)) +
  geom_point(color="navyblue")+
  labs(x = "Exchange Rate Volatility", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  xlim(0,1)+
  theme_bw()

# 5.2
dataset_total %>%
  filter(develop == "EM" ) %>%
  ggplot(aes(x = fx_volatility, y = foreign_participation_percent_GDP)) +
  geom_point(color="red4")+
  labs(x = "Exchange Rate Volatility", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  xlim(0,1)+
  theme_bw()

# 5.3 Mostra a relação da inflação com a participação na dívida e o tamanho da bolinha é o PIB percapita (note que ele vai diminuindo)
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         upper = max(foreign_participation_percent_GDP),
         lower = min(foreign_participation_percent_GDP),
         GDP_percapita_cur_USD = GDP_percapita_cur_USD/1000) %>%
  ggplot() +
  geom_point(aes(x = mean_inflation, y = foreign_participation_percent_GDP, colour = develop,
                 size = GDP_percapita_cur_USD), alpha = .2) +
  #geom_errorbar(aes(ymin = lower, ymax = upper), width = .2) +
  labs(x = "Mean Inflation Between 2004-2019 (%)", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  guides(col=guide_legend(""),
         size=guide_legend("GDP per capita \n(thousand USD)")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 5.4 Mesmo que no de cima, mas colorindo por país
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T)) %>% 
  ggplot() +
  geom_point(aes(x = mean_inflation, y = foreign_participation_percent_GDP, colour = country,
                 size = political_stability_rank), alpha = .5)+
  #labs(x = "ln(GDP per capita USD)", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  #scale_color_viridis_d("magma") +
  theme_light() +
  theme(legend.position = "none")

# 5.5 Relação entre volatilidade do crescimento real e a participação
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         mean_share = mean(foreign_participation_percent_GDP)) %>%
  ggplot() +
  geom_label(aes(x = sd_GDP_growth, y = mean_share, colour = develop,
                 size = mean_indebt, label = country), alpha = .3) +
  #labs(x = "ln(GDP per capita USD)", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  theme_light() +
  facet_wrap(~develop) +
  theme(legend.position = "none")

# 5.6 Relação entre média do crescimento real e a participação - sensibilizando por Rule of Law
dataset_total %>%
  group_by(country) %>% 
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         mean_GDP_growth = mean(GDP_growth, na.rm = T),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         mean_rule = mean(rule_of_law_rank, na.rm = T),
         mean_share_ex_off = mean(foreign_ex_officials_participation_percent_GDP)) %>%
  ggplot() +
  geom_label(aes(x = mean_GDP_growth, y = mean_share_ex_off, colour = develop,
                       size = mean_rule, label = country), alpha = .3) +
  guides(size = guide_legend("Rule of\nLaw"),
         colour = FALSE) +
  labs(x = "Mean GDP Growth Between 2004-2019 (%)", y = "Mean Foreign Participation in\nSovereign Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8))) +
  facet_wrap(~develop)

# 5.7 Relação entre volatilidade do cambio e a participação - sensibilizando por debt-to-gdp
dataset_5.7_int <- dataset_total %>%
  filter(country == "United States") %>%
  group_by() %>% 
  select(year, inflation_end) %>% 
  rename(US_inflation_rate = inflation_end)

dataset_5.7 <- dataset_total %>% 
  left_join(dataset_5.7_int, by = "year")


dataset_5.7 %>%
  group_by(country) %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         mean_GDP_growth = mean(GDP_growth, na.rm = T),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         x = US_inflation_rate/inflation_end,
         fx_vol_real = mean(x, na.rm = T),
         mean_share_ex_off = mean(foreign_ex_officials_participation_percent_GDP)) %>%
  ggplot() +
  geom_point(aes(x = log(fx_volatility*10000), y = foreign_participation_percent_GDP, colour = develop,
                 size = debt_to_GDP), alpha = .3) +
  #labs(x = "ln(GDP per capita USD)", y = "Foreign Participation in Sovereign Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  theme_light() #+
  facet_wrap(~develop) +
  theme(legend.position = "none")

# 5.8 Níveis de inflação dividindo por desenvolvimento
x_order <- c("From -1 to 2.5", "From 2.5 to 5", "From 5 to 7.5", "From 7.5 to 10", "From 10 to 12.5",
             "From 12.5 to 15", "15 +")  

dataset_total %>%
  mutate(inflation_level = ifelse(inflation_end > - 1 & inflation_end <= 2.5, "From -1 to 2.5",
                                  ifelse(inflation_end < 5, "From 2.5 to 5",
                                         ifelse(inflation_end < 7.5, "From 5 to 7.5",
                                                ifelse(inflation_end < 10, "From 7.5 to 10",
                                                       ifelse(inflation_end < 12.5, "From 10 to 12.5",
                                                              ifelse(inflation_end < 15, "From 12.5 to 15", "15 +"))))))) %>%
  filter(!is.na(inflation_level)) %>% 
  ggplot() +
  geom_violin(aes(factor(inflation_level, levels = x_order), foreign_participation_percent_GDP, fill = develop,
                  colour = develop), trim = T) +
  geom_vline(xintercept = 1.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 2.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 3.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 4.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 5.5, 
             color = "black", size = .6) +
  geom_vline(xintercept = 6.5, 
             color = "black", size = .6) +
  scale_color_manual(values = c("navyblue", "red4")) +
  scale_fill_manual(values = c("navyblue", "red4")) +
  guides(fill = guide_legend(""),
         color = guide_legend("")) +
  theme_light() +
  labs(x = "Inflation Level (%)", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") + 
  theme(axis.line.x = element_line(colour = "black", size = .6),
        axis.line.y = element_line(colour = "black", size = .6),
        axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))

# 5.8 Níveis de inflação
x_order <- c("From -1 to 2.5", "From 2.5 to 5", "From 5 to 7.5", "From 7.5 to 10", "From 10 to 12.5",
             "From 12.5 to 15", "15 +")  

dataset_total %>%
  mutate(inflation_level = ifelse(inflation_end > - 1 & inflation_end <= 2.5, "From -1 to 2.5",
                                  ifelse(inflation_end < 5, "From 2.5 to 5",
                                         ifelse(inflation_end < 7.5, "From 5 to 7.5",
                                                ifelse(inflation_end < 10, "From 7.5 to 10",
                                                       ifelse(inflation_end < 12.5, "From 10 to 12.5",
                                                              ifelse(inflation_end < 15, "From 12.5 to 15", "15 +"))))))) %>%
  filter(!is.na(inflation_level)) %>% 
  ggplot() +
  geom_violin(aes(factor(inflation_level, levels = x_order), foreign_participation_percent_GDP), fill = "black") +
  scale_color_manual(values = c("navyblue", "red4")) +
  scale_fill_manual(values = c("navyblue", "red4")) +
  theme_light() +
  xlab("Inflation Levels (%)")



# 5.9 Relação entre volatilidade do cambio e a participação - sensibilizando por rule of law
dataset_5.9_int <- dataset_total %>%
  filter(country == "United States") %>%
  group_by() %>% 
  select(year, inflation_end) %>% 
  rename(US_inflation_rate = inflation_end)

dataset_5.9 <- dataset_total %>% 
  left_join(dataset_5.9_int, by = "year")


dataset_5.9 %>%
  group_by(country) %>%
  mutate(mean_indebt = mean(debt_to_GDP, na.rm = T),
         mean_inflation = mean(inflation_end, na.rm = T),
         mean_fiscal = mean(lending_borrowing_percent_GDP, na.rm = T),
         mean_percapita = mean(GDP_percapita_cur_USD, na.rm = T),
         GDP_growth = (GDP_cte_billions - lag(GDP_cte_billions, k = 1))/lag(GDP_cte_billions, k = 1),
         mean_GDP_growth = mean(GDP_growth, na.rm = T),
         sd_GDP_growth = sd(GDP_growth, na.rm = T),
         x = US_inflation_rate/inflation_end,
         fx_vol_real = mean(x, na.rm = T),
         mean_share_ex_off = mean(foreign_ex_officials_participation_percent_GDP)) %>%
  ggplot() +
  geom_point(aes(x = log(fx_volatility*10000), y = foreign_participation_percent_GDP, colour = develop,
                 size = rule_of_law_rank), alpha = .4) +
  labs(x = "FX Volatility*", y = "Foreign Participation in Sovereign\n Debt in Terms of GDP (%)") +
  scale_color_manual(values = c("navyblue", "red4")) +
  guides(col=guide_legend(""),
         size=guide_legend("Rule of\nLaw Rank")) +
  theme_light() +
  theme(axis.text.y = element_text(margin = margin(l = 8)),
        axis.text.x = element_text(margin = margin(b = 8)))
  

################################## EXTERNAL FACTORS ##############################################################
  
  
dataset_total2 <- dataset_total %>% 
select(vix_EUA, foreign_participation_percent_GDP, year, develop, country) 

dataset_total2 <- dataset_total2 %>% 
group_by(year) %>% 
mutate(VIX = mean(vix_EUA))

dataset_total2 <- dataset_total2 %>% 
group_by(year, develop) %>% 
mutate(foreign_GDP = mean(foreign_participation_percent_GDP))

dataset_total2 <- dataset_total2 %>% 
  select(3,4,6,7)

dataset_total2 <- dataset_total2 %>% 
  distinct()


dataset3 <- dataset_total2 

dataset3 <- dataset3 %>% 
  mutate(foreign_AM = ifelse(develop == "AM", foreign_GDP, 0))

dataset3 <- dataset3 %>% 
  filter(foreign_AM != 0)


dataset4 <- dataset_total2

dataset4 <- dataset4 %>% 
  mutate(foreign_EM = ifelse(develop =="EM", foreign_GDP, 0))

dataset4 <- dataset4 %>% 
  filter(foreign_EM != 0)

dataset3 <- dataset3 %>% 
  select(1,3,5)

dataset4 <- dataset4 %>% 
  select(1,3,5)

dataset4 <- dataset4 %>% 
  left_join(dataset3, by="year")

dataset4 <- dataset4 %>%
  select(2,3,4,7)

dataset4 <- dataset4 %>% 
  rename(VIX = 2, 
          AM = 4,
          EM = 3 ) %>% 
          mutate(VIX = (VIX/100))

dataset4 <- dataset4 %>%
  select(year, VIX, AM, EM) %>%
  gather(key = "variable", value = "value", -year)


ggplot(dataset4, aes(x = year, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("navyblue", "red4" , "black")) +
  labs(x = "Year", y = "", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004, 2019, by=2), name = "Year") +
  scale_y_continuous(breaks=NULL) +
  theme_bw()

# Another way to display the same graph:
dataset5 <- dataset_total %>% 
  select(year, vix_EUA) %>% 
  filter(row_number() <= 16) %>% 
  rename(value = vix_EUA) %>% 
  mutate(variable = "US VIX")

dataset6 <- dataset_total %>%
  filter(!is.na(foreign_participation_percent_GDP)) %>% 
  group_by(year, develop) %>%
  summarise(foreign_GDP = mean(foreign_participation_percent_GDP), year = year) %>%
  select(c(year, foreign_GDP)) %>% 
  distinct() %>% 
  rename(variable = develop, value = foreign_GDP)

p1 <- dataset5 %>% 
  ggplot() +
  geom_line(aes(x = year, y = value, colour = variable), size = 1) +
  scale_color_manual(values = c("black")) +
  labs(x = "", y = "US VIX", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2004, 2019), seq(2004, 2019, by=2), name = "Year") +
  #scale_y_continuous(breaks=NULL) +
  theme_light() +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        plot.caption = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8)))
  
(p2 <- dataset6 %>% 
  ggplot(aes(x = year, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("navyblue", "red4")) +
  labs(x = "Year", y = "Foreign Participation in \nSovereign Debt (% of GDP)", title = "", subtitle = "") +
  scale_x_continuous(limits = c(2003, 2020), seq(2004, 2019, by = 2), name = "Year") +
  #scale_y_continuous(breaks=NULL) +
  theme_light() +
  theme(legend.title = element_blank(),
        plot.title = element_blank(),
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        plot.subtitle = element_blank(),
        axis.text.y = element_text(margin = margin(l = 8))))

grid.newpage()
grid.draw(rbind(ggplotGrob(p1),
                ggplotGrob(p2),
                size = "last"))

