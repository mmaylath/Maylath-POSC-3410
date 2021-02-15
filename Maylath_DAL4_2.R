# Title: DAL4 - Key - Part 2
# Author: Grant A. Allard
# Author's Email: grant@grantallard.com
# Date Created: 2021-02-08

# Purpose:
# We will learn how to work with the weights in the GSS survey to analyze questions using data visualizations. 

# Set Up####
# Libraries
library(tidyverse)

# Data
# Load data 
gss_df <- read.csv('gss_df.csv')

# Let's Look Only at the 2018 Data 
gss_2018_df <- gss_df %>% 
  filter(YEAR == 2018)

# Question 1 ####
# Let's look at the relationship between sex and approval of federal support for science. We are testing a theory that says sex causes people to form their opinion about whether the federal government should support science. 
# theory is falsifiable, observable, generalizable.

# We have translated this theory into a set of hypotheses that we are testing. 

H0 <- "The percent of men and women who approve of federal support for science the same." 

Ha <- "The percent of men and women who approve of federal support for science is not the same." 

# First let's check the unique value of Advfront_df
unique(gss_2018_df$ADVFRONT)

# We need to filter out observations where the value of ADVFRONT is "Not applicable". We also need to summize the data by SEX and ADVFRONT. 
sex_Advfront_df <- gss_2018_df %>%
  filter(ADVFRONT != "Not applicable" ) %>% 
  group_by(SEX, ADVFRONT) %>% 
  summarize(n = sum(WTSSALL))

# Calculate percent of men who feel a certain way
male_Advfront_df <- sex_Advfront_df %>% 
  filter(SEX == "Male")

# Total Number of men 
male_Advfront_df %>% 
 summarize(n = sum(n))

# Calculate men's responses as percentages. 
male_Advfront_df <- male_Advfront_df %>% 
  mutate(Advfront_perc = n/491 * 100) %>% 
  select(SEX, ADVFRONT,  Advfront_perc)

# Calculate percent of women who feel a certain way
female_Advfront_df <- sex_Advfront_df %>% 
 filter(SEX == "Female")

# Total Number of women 
female_Advfront_df %>% 
  summarize(n = sum(n))

# Calculate women's responses as percentages. 
female_Advfront_df <- female_Advfront_df %>% 
  mutate(Advfront_perc = n / 670*100) %>% 
  select(SEX, ADVFRONT, Advfront_perc)

# Create unified data frame of men and women by using bind_rows. 
sex_Advfront_per_df <- bind_rows(female_Advfront_df, male_Advfront_df)

# Text vector for Reordering ADVFRONT when we code it as a vector
levels <- c("Strongly agree", "Agree", "Disagree", "Strongly disagree", "Dont know", "No answer")

# ggplot bar graph of sex_Advfront_per_df
sex_Advfront_per_df %>% 
  mutate(ADVFRONT = factor(ADVFRONT, levels=levels)) %>% 
  ggplot(aes(x= SEX, y=Advfront_perc, fill=ADVFRONT)) +
  geom_bar(stat="identity")


# Question 1: Which hypothesis does the visualization provide evidence against? Why?

Answer1 <- "" #look at bar graph

# Question 2 #### 
# Now we are going to test a second theory related to Sex. We are testing the theory that Sex causes differences in how people view space exploration. 

# Hypotheses
H0 <- "The percent of men and women who approve of federal support for science the same." 

Ha <- "The percent of men and women who approve of federal support for science is not the same." 

# We will use repeat the process from question 1. 
sex_Natspac_df <- gss_2018_df %>% 
  filter(NATSPAC != "Not applicable") %>% 
  group_by(SEX, NATSPAC) %>% 
  summarize(n=sum(WTSSALL))

# Calculate percent of men who feel a certain way
male_Natspac_df <- sex_Natspac_df %>% 
  filter(SEX == "Male")

# Total Number of men 
male_Natspac_df %>% 
  summarize(n = sum(n))

# Calculate men's responses as percentages. 
male_Natspac_df <- male_Natspac_df %>% 
  mutate(Natspac_perc = n/473*100) %>% 
  select(SEX, NATSPAC, Natspac_perc)

# Calculate percent of women who feel a certain way
female_Natspac_df <- sex_Natspac_df %>% 
  filter(SEX == "Female")

# Total Number of women 
female_Natspac_df %>% 
  summarize(n = sum(n))

# Calculate women's responses as percentages. 
female_Natspac_df <- female_Natspac_df %>% 
  mutate(Natspac_perc = n/750*100) %>% 
  select(SEX, NATSPAC, Natspac_perc)

# Combine into single df using bind_rows()
sex_Natspac_perc_df <- bind_rows(male_Natspac_df, female_Natspac_df)

# Create text vector for reordering 
levels <- c("Too much", "About right", "Too little", "Don't know", "No answer")

# Create ggplot 
sex_Natspac_perc_df %>% 
  mutate(NATSPAC = factor(NATSPAC, levels = levels)) %>% 
  ggplot(aes(x = SEX, y = Natspac_perc, fill = NATSPAC)) +
  geom_bar(stat="identity") +
  scale_fill_manual(breaks = levels, values = c("#FFCC00", "#33FF00", "#0033FF", "#999999", "#FFFFFF"))
  
# Question 2: Which hypothesis does this visualization provide evidence against? 
Answer2 <- "The visualization provides evidence against the null hypothesis H0, which states that the percent of men and women who approve
of federal support for science is the same. The colored bars indicating the percentage of respondents who replied with a certain answer are different
lengths in the female and male column, indicating that the percents of men and women who approve of federal support for science are different."

# Copyright (c) Grant Allard, 2021
