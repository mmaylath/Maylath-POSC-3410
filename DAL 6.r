# Title: DAL6 (In-Class Exercise)
# Author: Madeleine Maylath
# Date Created: 2021-03-26

# Purpose: Plot and understand data from the AUTM and GTD datasets, then perform
# linear regression analysis on the data to analyze relationships.
# Also - learn to adjust data and group it together to perform linear regression
# on different chunks of the data to get higher significance (smaller p-value) 
# of lin. reg.

# Set Up####
# Libraries
library(tidyverse)
library(stargazer)
# Data
load("autm_example_df.Rdata")
load("gtd_df.Rdata")

# Make analysis df for gtd
gtd_df <- gtd_raw

# Regression Practice - AUTM Dataset ####

# scatterplot 
autm_example_df %>% 
  ggplot(aes(x = totResExp, y = grossLicInc, color = institution)) +
  geom_point() +
  theme(legend.position = "bottom")+
  facet_wrap(~institution, scales="free")

# Create Northwestern Data
nu_df <- autm_example_df %>% 
  filter(institution == "Northwestern Univ.")

warf_df <- autm_example_df %>% 
  filter(institution == "W.A.R.F./University of Wisconsin Madison")

# 
t.test(nu_df$totResExp, warf_df$totResExp)

# lm NU  
summary(lm(data = nu_df, grossLicInc ~ totResExp))
nu_df_ro <- lm(data = nu_df, grossLicInc ~ totResExp)
# p-val = .2016
# R^2 - tells how much of the change in the dep. variable can be explained by the line
# negative intercept - doesn't make a lot of sense, only gives us an idea for this data set
# stargazer(simple_model, type = "html", out = "simple_model2.html")

# lm Warf
summary(lm(data=warf_df, grossLicInc ~ totResExp ))
warf_df_ro <- lm(data=warf_df, grossLicInc ~ totResExp )
# p-val = .007319
# for a 1 unit (1$) change in totResExp, there is a .06034 change in grossLicInc
# with 99% certainty we can say this is the relationship
stargazer(warf_df_ro, type = "html", out = "warf_star.html")

# scatterplot with categorical variable 
autm_example_df %>% 
  ggplot(aes(x = totResExp, y = grossLicInc, color = institution)) +
  geom_point() +
  theme(legend.position = "bottom") +
  geom_smooth(method="lm", se=FALSE) + 
  facet_wrap(~institution, scales="free")
  

# lm autm_example_df + institution variables
summary(lm(data=autm_example_df, grossLicInc ~ totResExp + institution))
autm_df_ro <- lm(data=autm_example_df, grossLicInc ~ totResExp + institution)
stargazer(autm_df_ro, type = "html", out = "autm_df_star.html")

# GTD Data #### 

# Does the number of attacks increase over time? 
gtd_df %>% 
  group_by(iyear) %>% 
  summarize(attacks = n()) %>% 
  arrange(iyear) %>% 
  ggplot(aes(x=iyear, y=attacks)) +
  geom_point() + 
  geom_smooth(method="lm")
# lin reg. line of best fit does a good job representing data until about 1993, then
# the data differs from it by a lot
# there is a change in sampling method in early 1990s, this could effect the data

# By how much? 
# Create gtd_by_year_df
gtd_by_year_df <- gtd_df %>% 
  group_by(iyear) %>% 
  summarize(attacks = n())

# lm gtd_by_year_df
summary(lm(data = gtd_by_year_df, attacks ~ iyear)) 
gtd_ro <- lm(data = gtd_by_year_df, attacks ~ iyear)
stargazer(gtd_ro, type = "html", out = "gtd_star.html")

# How do we better fit the line? 
# choose areas that will have the same slope

# Divide data into three separate periods 
first_data <- gtd_by_year_df %>% 
  filter(iyear<1995)

middle_data <- gtd_by_year_df %>% 
  filter(iyear>=1995 & iyear<2014)

late_data <- gtd_by_year_df %>% 
  filter(iyear>=2014)

# Regressions
summary(lm(data=first_data, attacks ~ iyear)) 

summary(lm(data=middle_data, attacks ~ iyear)) 

summary(lm(data=late_data, attacks ~ iyear)) 

# Want to see this in the same readout? 
first_data <- gtd_by_year_df %>% 
  filter(iyear<1995) %>% 
  mutate(period="first")

middle_data <- gtd_by_year_df %>% 
  filter(iyear>=1995 & iyear<2014) %>% 
  mutate(period="middle")

late_data <- gtd_by_year_df %>% 
  filter(iyear>=2014)%>% 
  mutate(period="late")

# Bind Rows 
gtd_by_year_df <- bind_rows(first_data, middle_data, late_data)

#lm gtd_by_year_df with peirod as factor 
summary(lm(data=gtd_by_year_df, attacks ~ iyear + period)) 

# 3 different slopes for 3 diff periods
# doesn't include periodearly because doing factor analysis, can't have 100% fit

gtd_by_year_df %>% 
  ggplot(aes(x=iyear, y=attacks, color = period)) +
  geom_point() + 
  geom_smooth(method="lm")


# Copyright (c) Grant Allard, 2021
