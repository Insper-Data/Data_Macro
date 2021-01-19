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

dataset_total %>%
  mutate(foreign_debt_perc=(foreign_debt/total_debt)*100) %>% 
  rename(Development = develop) %>% 
  filter(year %in% c(2004, 2010, 2014, 2019), !is.na(Development)) %>%
  bar_chart(x = country, y = (foreign_debt_perc), facet = year, top_n = 10, fill = Development) +  
  labs( x = "", y = "Foreign Investors (%)",
        title = "Graph 1: Top 10 Countries with Debt Held by Foreign Investors", fill = "Development") +
  theme_classic() +
  theme(legend.title = element_text(face = "bold", size = 10)) +
  scale_fill_manual("Development", values = c("EM" = "red4", "AM" = "navyblue"))


#2

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
  labs(x = "Year", y = "Foreign Investors (%)", title = "Graph 2: Average Foreign Participation in Sovereign Debt", subtitle = "Advanced and Emerging Markets") +
  ylim(30,50)+
  theme_light()

graph_debt_foreign_pp

#3
graph_debt_foreign_sep <- dataset_total %>% 
  filter(!is.na(develop)) %>% 
  rename(Development = develop) %>% 
  mutate(foreign_debt_USD=(foreign_debt*fx)) %>% 
  group_by(year, Development) %>% 
  summarise(debt_foreign_mean = sum(foreign_debt_USD)/1000) %>% 
  ggplot(aes(x = year, y = debt_foreign_mean, color=Development )) +
  scale_color_manual(values = c("EM" = "red4", "AM" = "navyblue"))+
  geom_point() +
  geom_smooth()+
  labs(x = "Year", y = "Foreign Investors (Trillion USD)", title = " Graph 3: Total Debt Held by Foreign Investors", subtitle = "Advanced and Emerging Markets") +
  theme_bw()

graph_debt_foreign_sep



