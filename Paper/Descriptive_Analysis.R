#instalando as libraries
library(scales)
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

#puxando a base de dados completa
#setwd("C:/Users/gabri/Documents/Insper_Data/Macro/projeto_econometria/Data_Macro/Paper")

dataset_total <- read.csv("dataset_total_jan_2021.csv") 

# Stylized Facts About the Database

#1

debt_to_gdp_graph <- dataset_total %>% 
  rename(Development = develop) %>% 
  group_by(Development, year) %>% 
  summarise(debt_to_GDP = mean(debt_to_GDP)) %>% 
  ggplot(aes(x = year, y = debt_to_GDP, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_smooth()+
  labs(x = "Year", y = "Debt-to-GDP ratio", title = "Figure 1: average debt-to-GDP ratio", subtitle = "Advanced and Emerging Markets") +
  ylim(30,90)+
  theme_bw()

debt_to_gdp_graph

#2

dataset_total %>%
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(develop)) %>%
  rename(Development = develop) %>% 
  bar_chart(x = country, y = debt_to_GDP, facet = year, top_n = 10, fill = Development) +
  labs( x = "", y = "Debt-to-GDP (%)",
        title = "Figure 2: top 10 debt-to-GDP ratio", fill = "Development") +
  theme_classic() +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))+
  theme(legend.title = element_text(face = "bold", size = 10))

#3

graph_debt_foreign_pp <- dataset_total %>% 
  filter(!is.na(develop)) %>% 
  rename(Development = develop) %>% 
  mutate(foreign_debt_perc=(foreign_debt/total_debt)*100) %>% 
  group_by(year, Development) %>% 
  summarise(debt_foreign_pp_mean = mean(foreign_debt_perc)) %>% 
  ggplot(aes(x = year, y = debt_foreign_pp_mean, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_smooth()+
  labs(x = "Year", y = "Foreign Investors (%)", title = "Figure 3: average foreign participation in sovereign debt", subtitle = "Advanced and Emerging Markets") +
  ylim(30,45)+
  theme_light()

graph_debt_foreign_pp



#4

dataset_total %>%
  mutate(foreign_debt_perc=(foreign_debt/total_debt)*100) %>% 
  rename(Development = develop) %>% 
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(Development)) %>%
  bar_chart(x = country, y = (foreign_debt_perc), facet = year, top_n = 10, fill = Development) +  
  labs( x = "", y = "Foreign Investors (%)",
        title = "Figure 4: top 10 sovereign debt held by foreign agents", fill = "Development") +
  theme_classic() +
  theme(legend.title = element_text(face = "bold", size = 10)) +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))





-------------------------------------



#5
  
dataset_total %>% 
  filter(!is.na(develop)) %>% 
  rename(Development = develop) %>% 
  filter(Development != "AM") %>% 
  group_by(year) %>% 
  summarise(foreign_participation_percent_GDP = mean(foreign_participation_percent_GDP, vix_EUA=mean(vix_EUA))) %>% 
  ggplot(aes(x = vix_EUA, y = foreign_participation_percent_GDP )) +
  geom_point() +
  labs(x = "Year", y = "Nominal Rate (%)", title = " Graph 4: Nominal Rate (%) ", subtitle = "Advanced and Emerging Markets" ) +
  theme_bw()




