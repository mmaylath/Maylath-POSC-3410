# Title: Updated Project Code ####

# libraries
library(tidyverse)
library(stargazer)

# Load data
load("gss_df.Rdata")
# Look at column names
names(gss_df)
# Look at variable values
unique(gss_df$EDUC)
unique(gss_df$RINCOME)
unique(gss_df$NATSPAC)

# t-test: control for income, lower income (less than $15000) ####

gss_t1 <- gss_df %>% 
  select(EDUC, NATSPAC, RINCOME, WTSSALL) %>% 
  filter(EDUC != "No answer", EDUC != "Don't know", !is.na(EDUC)) %>% 
  filter(NATSPAC != "Not applicable", NATSPAC != "No answer", NATSPAC != "Don't know", !is.na(NATSPAC)) %>% 
  filter(RINCOME != "Not applicable", RINCOME != "Refused", RINCOME != "No answer", !is.na(RINCOME), RINCOME != "Don't know") %>% 
  #filter(RINCOME != "$15000 - 19999", RINCOME != "$20000 - 24999", RINCOME != "$25000 or more")
  filter(RINCOME == "$20000 - 24999")
# check structure of EDUC (years of education)
str(gss_t1$EDUC)
# make EDUC numeric
gss_t1$EDUC <- as.numeric(as.character(gss_t1$EDUC))
# check structure of EDUC again
str(gss_t1$EDUC)

# assign education levels

# df for no high school
edu_no_hs <- gss_t1 %>%
  filter(EDUC < 9) %>% 
  mutate(ed_lev = "no hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
# assign numeric value to likert scale rating of NATSPAC variable
# -1 is "too much", 0 is "about right", 1 is "too little"
edu_no_hs$NATSPAC <- recode(edu_no_hs$NATSPAC, "Too little" = 1, "Too much" = -1, "About right" = 0)
str(edu_no_hs$NATSPAC)

# df for high school
edu_hs <- gss_t1 %>% 
  filter(EDUC > 8 & EDUC < 13) %>% 
  mutate(ed_lev = "hs") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
str(edu_hs$NATSPAC)
# assign numeric value to likert scale rating of NATSPAC
edu_hs$NATSPAC <- recode(edu_hs$NATSPAC, "Too little" = 1, "Too much" = -1, "About right" = 0)
str(edu_hs$NATSPAC)

t.test(edu_hs$NATSPAC, edu_no_hs$NATSPAC, a = "t")

# example2 <- revalue(example, c("Daily"= "3", "Never"= "-3", "Often"= "1",
# "Very Often"= "2", "Important" = "3", "Very Important"= "3",
# "Neutral"= "0", "Not Important"= "-3" ))  
#group_by(NATSPAC, ed_lev) %>% 
#summarize(count = sum(WTSSALL))

# df for college
edu_col <- gss_t1 %>% 
  filter(EDUC > 12 & EDUC < 17) %>% 
  mutate(ed_lev = "col") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
edu_col$NATSPAC <- recode(edu_col$NATSPAC, "Too little" = 1, "Too much" = -1, "About right" = 0)
str(edu_col$NATSPAC)

t.test(edu_col$NATSPAC, edu_hs$NATSPAC, a = "t")

# df for grad school
edu_grad <- gss_t1 %>% 
  filter(EDUC > 16) %>% 
  mutate(ed_lev = "grad") %>% 
  select(NATSPAC, WTSSALL, ed_lev)
#group_by(NATSPAC, ed_lev) %>% 
# summarize(count = sum(WTSSALL))
edu_grad$NATSPAC <- recode(edu_grad$NATSPAC, "Too little" = 1, "Too much" = -1, "About right" = 0)
str(edu_grad$NATSPAC)

t.test(edu_col$NATSPAC, edu_grad$NATSPAC, a = "t")

# looks like within the $20000 to $24999 income bracket, level of education impacts 
# views on science. The biggest difference is between high school and college (undergrad) groups).

no_col <- bind_rows(edu_no_hs, edu_hs)
yes_col <- bind_rows(edu_col, edu_grad)

t.test(yes_col$NATSPAC, no_col$NATSPAC, a = "t")
# very high significance >> p-value =4.133e-11
# means: yes_col = -0.08958, no_col = -0.29297 (more negative means less favorable towards increasing space funding)
