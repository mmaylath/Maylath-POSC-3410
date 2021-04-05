# Title: Research Project - GSS ####
# Author: Madeleine Maylath
# Date created: 3/29/2021

# libraries
library(tidyverse)
library(stargazer)

# Wrangling data
load("gss_df.Rdata")
rm(autm_df_ro)
rm(gtd_df, gtd_ro, gtd_by_year_df,nu_df, nu_df_ro, warf_df, warf_df_ro)

# check variable values
unique(gss_df$EDUC)
unique(gss_df$NATSPAC)
unique(gss_df$RINCOME)
unique(gss_df$REALRINC)
# Controlling for income - low income ####

# Looking at impact of education within one income bracket on space views
# Will look at just the below $15000 range first
# controlling for income

# select relevant variables
gss_edu <- gss_df %>% 
  select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% 
  filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>% 
  filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
  filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer",
         !is.na(RINCOME), RINCOME != "Don't know") %>% 
  filter(RINCOME != "$15000 - 19999", RINCOME != "$20000 - 24999", RINCOME != "$25000 or more")

# check structure of EDUC (years of education)
str(gss_edu$EDUC)
# make EDUC numeric
gss_edu$EDUC <- as.numeric(as.character(gss_edu$EDUC))
# check structure of EDUC again
str(gss_edu$EDUC)

# df for no high school
edu_no_hs <- gss_edu %>%
  filter(EDUC < 9) %>% 
  mutate(ed_lev = "no hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
  #group_by(NATSPAC, ed_lev) %>% 
  #summarize(count = sum(WTSSALL))

# df for high school
edu_hs <- gss_edu %>% 
  filter(EDUC > 8 & EDUC < 13) %>% 
  mutate(ed_lev = "hs") %>% 
 select(NATSPAC, WTSSALL, ed_lev)
 #group_by(NATSPAC, ed_lev) %>% 
  #summarize(count = sum(WTSSALL))

# df for college
edu_col <- gss_edu %>% 
  filter(EDUC > 12 & EDUC < 17) %>% 
  mutate(ed_lev = "col") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
  #group_by(NATSPAC, ed_lev) %>% 
  #summarize(count = sum(WTSSALL))

# df for grad school
edu_grad <- gss_edu %>% 
  filter(EDUC > 16) %>% 
  mutate(ed_lev = "grad") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
  #group_by(NATSPAC, ed_lev) %>% 
  # summarize(count = sum(WTSSALL))

ed_sample_df <- bind_rows(edu_hs, edu_no_hs)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# p-val = 1.427e-14
# X-squared = 61.497
ed_sample_df <- bind_rows(edu_grad, edu_col)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# p-val = 0.08154
# X-squared = 5.0134
ed_sample_df <- bind_rows(edu_col, edu_hs)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# X-squared = 259.51
# p-val < 2.2e-16

# Controlling for income - mid income ####

# looking at the $15000 to $24999 bracket
gss_edu <- gss_df %>% 
  select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% 
  filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>% 
  filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
  filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer", !is.na(RINCOME), RINCOME != "Don't know") %>% 
  filter(RINCOME == "$20000 - 24999")

# check structure of EDUC (years of education)
str(gss_edu$EDUC)
# make EDUC numeric
gss_edu$EDUC <- as.numeric(as.character(gss_edu$EDUC))
# check structure of EDUC again
str(gss_edu$EDUC)

# df for no high school
edu_no_hs <- gss_edu %>%
  filter(EDUC < 9) %>% 
  mutate(ed_lev = "no hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for high school
edu_hs <- gss_edu %>% 
  filter(EDUC > 8 & EDUC < 13) %>% 
  mutate(ed_lev = "hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for college
edu_col <- gss_edu %>% 
  filter(EDUC > 12 & EDUC < 17) %>% 
  mutate(ed_lev = "col") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for grad school
edu_grad <- gss_edu %>% 
  filter(EDUC > 16) %>% 
  mutate(ed_lev = "grad") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
# summarize(count = sum(WTSSALL))

ed_sample_df <- bind_rows(edu_hs, edu_no_hs)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# p-val = 1.427e-14
# X-squared = 61.497
ed_sample_df <- bind_rows(edu_grad, edu_col)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# p-val = 0.08154
# X-squared = 5.0134
ed_sample_df <- bind_rows(edu_col, edu_hs)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))

# controlling for income - upper income ####

gss_edu <- gss_df %>% 
  select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% 
  filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>% 
  filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
  filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer", !is.na(RINCOME), RINCOME != "Don't know") %>% 
  filter(RINCOME == "$25000 or more")

# check structure of EDUC (years of education)
str(gss_edu$EDUC)
# make EDUC numeric
gss_edu$EDUC <- as.numeric(as.character(gss_edu$EDUC))
# check structure of EDUC again
str(gss_edu$EDUC)

# df for no high school
edu_no_hs <- gss_edu %>%
  filter(EDUC < 9) %>% 
  mutate(ed_lev = "no hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for high school
edu_hs <- gss_edu %>% 
  filter(EDUC > 8 & EDUC < 13) %>% 
  mutate(ed_lev = "hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for college
edu_col <- gss_edu %>% 
  filter(EDUC > 12 & EDUC < 17) %>% 
  mutate(ed_lev = "col") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for grad school
edu_grad <- gss_edu %>% 
  filter(EDUC > 16) %>% 
  mutate(ed_lev = "grad") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
# summarize(count = sum(WTSSALL))

ed_sample_df <- bind_rows(edu_hs, edu_no_hs)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# p-val = 1.427e-14
# X-squared = 61.497
ed_sample_df <- bind_rows(edu_grad, edu_col)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))
# p-val = 0.08154
# X-squared = 5.0134
ed_sample_df <- bind_rows(edu_col, edu_hs)
chisq.test(table(ed_sample_df$ed_lev, ed_sample_df$NATSPAC))

# Looking at how income within the same education bracket impacts views on space
# controlling for education - hs ####