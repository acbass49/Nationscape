# Title : LDS Income Myth Analysis
# Author : Alex Bass
# Date : 2 Feb 2023
# Description : This is a separate blog post than my purely LDS political one. 
#in this post, I look at LDS income levels over time

# Sourcing custom utilities
source('./scripts/utils.R')

# Load in relevant libraries
library(tidyverse)
library(haven)
library(data.table)
library(logger)
library(glue)
library(sjlabelled)
library(pewmethods)

#fonts
library(showtext)
font_add_google("Cairo", family = "Cairo")
showtext_auto()

# Grab data from files and combine
folders <- list.files('./data/')

vector_of_paths <- unlist(lapply(folders, function(folder){
  files <- list.files(paste0('./data/', folder))
  data_file_name <- files[grep('.+sav', files)]
  full_file_path_to_data <- paste0('./data/',folder, '/', data_file_name)
  return(full_file_path_to_data)
}))

vars_to_keep <- c('household_income', 'weight', 'religion')

data <- combine_nationscape(vector_of_paths, vars_to_keep) #calling custom function

data$household_income <- as.numeric(sjlabelled::as_labelled(data$household_income))

data$income_collapsed <- case_when(
  data$household_income %in% c(1,2,3,4) ~ "Less than $30k",
  data$household_income %in% c(5,6,7,8) ~ "30k - 49.9k",
  data$household_income %in% c(9:18) ~ "50k - 99.9k",
  data$household_income %in% c(19:24) ~ "100k+")

income_levels <- c(
  "Less than $30k",
  "30k - 49.9k",
  "50k - 99.9k",
  "100k+"
)

income_reversed <- c(
  "100k+",
  "50k - 99.9k",
  "30k - 49.9k",
  "Less than $30k"
)

data$income_collapsed <- factor(data$income_collapsed, levels = income_levels)

tmp_total <- get_totals("income_collapsed", data, wt="weight", na.rm = T)
tmp_lds <- get_totals("income_collapsed", data %>% filter(religion==3), wt="weight", na.rm = T)
tmp_jew <- get_totals("income_collapsed", data %>% filter(religion==6), wt="weight", na.rm = T)

income_groups_pcts <- round(c(tmp_total$weight, tmp_lds$weight, tmp_jew$weight))

#Figure 1 - LDS vs Others entitled `LDS_myth_1.png`
income_groups <- data.frame(survey = factor(c(rep("Overall",4), 
                                              rep("Latter-day Saints",4), 
                                              rep("Jews",4)), 
                                            levels = c("Jews",
                                                       "Latter-day Saints",
                                                       "Overall")),
                            income = rep(c("Less than $30k", "30k - 49.9k","50k - 99.9k","100k+"),3),
                            percent = income_groups_pcts) %>% 
  mutate(income = factor(income, levels = income_levels)) %>% 
  arrange(income) %>% 
  group_by(survey) %>% 
  mutate(pos = (cumsum(percent) - percent/2)/100)

income_groups <- income_groups %>% mutate(income = factor(income, levels = income_reversed))

p <- ggplot(income_groups, aes(fill = income, x = survey, y = percent))+
  geom_bar(position = "fill", stat = "identity")+
  geom_text(size = 5, aes(x=survey, 
                          y = pos, 
                          label = paste0(percent, "%"),
                          fontface = 2,
                          family = "Cairo"),
            color = "white")+
  coord_flip()+
  scale_fill_manual(values = c("dodgerblue4", "dodgerblue3", "dodgerblue2", 'dodgerblue1'))+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1.1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 1.5, size = 8, face = "italic"))+
  labs(caption = "Source: Nationscape.",
       title = "Latter-day Saints Not Proportionally Prosperous") + 
  guides(fill = guide_legend(reverse = TRUE))

png("plots/LDS_myth_1.png",width = 2000, height = 1000, units = "px", res = 72*4)
print(p)
dev.off()

#Figure 2 - LDS over Time entitled `LDS_myth_2.png`
lds_over_time <- data.frame(survey = factor(c(rep("2007",4), 
                                              rep("2014",4), 
                                              rep("2019-21",4)), 
                                            levels = c("2019-21",
                                                       "2014",
                                                       "2007")),
                            income = rep(c("Less than $30k", "30k - 49.9k","50k - 99.9k","100k+"),3),
                            percent = c(26,21,38,16,27,20,33,20,25,15,31,29)) %>% 
  mutate(income = factor(income, levels = income_levels)) %>% 
  arrange(income) %>% 
  group_by(survey) %>% 
  mutate(pos = (cumsum(percent) - percent/2)/100)

lds_over_time <- lds_over_time %>% mutate(income = factor(income, levels = income_reversed))

p2 <- ggplot(lds_over_time, aes(fill = income, x = survey, y = percent))+
  geom_bar(position = "fill", stat = "identity")+
  geom_text(size = 5, aes(x=survey, 
                          y = pos, 
                          label = paste0(percent, "%"),
                          fontface = 2,
                          family = "Cairo"),
            color = "white")+
  coord_flip()+
  scale_fill_manual(values = c("dodgerblue4", "dodgerblue3", "dodgerblue2", 'dodgerblue1'))+
  theme(axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(size = 16, hjust = 1.1),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = .5, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = 3.0, size = 8, face = "italic"))+
  labs(caption = "Source: 2007 and 2014 from Pew Research Center. 2019-21 from Nationscape.",
       title = "Latter-Day Saint 100k+ Group is Growing") + 
  guides(fill = guide_legend(reverse = TRUE))

png("plots/LDS_myth_2.png",width = 2000, height = 1000, units = "px", res = 72*4)
print(p2)
dev.off()

# Figure 3 - All Over Time
all_over_time <- data.frame(
  group_rel = rep(c("Overall Population", "Latter-day Saints","Jews"),3),
  year = c(rep(2007,3), rep(2014,3), rep(2021,3)),
  percent = c(23,16,46,28,20,44,40,29,60)) %>% 
  mutate(group_rel = factor(group_rel, levels = c("Overall Population", "Latter-day Saints","Jews"))) %>% 
  arrange(group_rel)

p3 <- ggplot(data = all_over_time, aes(x = year, y = percent, group = group_rel, color = group_rel))+
  geom_line(size = 3) +
  geom_point(size = 11, color = 'black') +
  geom_point(size = 10, color = 'white') +
  geom_text(aes(label = percent), color = 'black', family = 'Cairo')+
  ylim(c(0,75))+
  xlim(c(2005,2022))+
  theme_light()+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.background = element_blank(),
        text = element_text(face = "bold", family = "Cairo"),
        plot.title = element_text(hjust = 1, size = 20, face = "bold"),
        legend.title = element_blank(),
        plot.caption = element_text(hjust = -0.9, size = 8, face = "italic"),
        legend.key = element_rect(fill = "white"))+
  labs(caption = "Source: 2007, 2014 from Pew Research Center, 2021 from Nationscape (Jews + LDS). General Population from Census (ACS).",
       title = "Among 100k+: LDS Grow Same As Overall") +
  ylab("Share 100k+ Household Income")+
  xlab("Year")+
  scale_color_manual(values = c('black', 'goldenrod', 'dodgerblue3'))

png("plots/LDS_myth_3.png",width = 2000, height = 1500, units = "px", res = 72*4)
print(p3)
dev.off()
