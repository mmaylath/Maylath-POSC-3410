# Title: DAL 5 Part 2 ####
# Author: Madeleine Maylath
# Date: 2021-03-12

# Purpose: To learn how to do chi-square tests on the GSS file.

# Set Up ####
# Libraries
library(tidyverse)
library(infer)
library(stringr)

# Data
load("gss_df.Rdata")

# Example

# Theory: Support of science and technology relate to views on space exploration.

# Theoretical H0: A person's views on science and technology gives more
# opportunities to the next generation (nextgen) are NOT related to their views
# on supporting space exploration (intspace).

# Theoretical Ha: A person's views on science and technology gives more
# opportunities to the next generation (nextgen) are related to their views on
# supporting space exploration (itspace).


str(gss_df$NEXTGEN)
unique(gss_df$NEXTGEN)
# NEXTGEN (IV) is a catergorical bariable, non interval. 2 groups - agree and
# disagree, non-ordinal = who fits in these groups. Need to adjust data.

str(gss_df$INTSPACE)
# data type: character
unique(gss_df$INTSPACE)
# values: Not applicable, Moderately interested, Not at all interested,
# Very interested, Dont know, No answer, NA
# INTSPACE (DV) is a categorical variable, non-interval, with 2+ categories.

# Get data ready for analysis

# get rid of unnecessary variable columns, sort rows into two values for NEXTGEN

# WTSSALL - how to weight each interviewee.

analysis_df <- gss_df %>%
  # filter to keep rows we want
  filter(YEAR == 2018 & !is.na(NEXTGEN) & !is.na(INTSPACE) & (NEXTGEN == "Agree" |
  NEXTGEN == "Strongly agree" | NEXTGEN == "Strongly disagree" | NEXTGEN == "Disagree") & 
    (INTSPACE == "Moderately interested" | INTSPACE == "Very interested" | INTSPACE == "Not at all interested")) %>% 
  # keep only vars we need
  select(INTSPACE, NEXTGEN, WTSSALL) %>% 
  group_by(NEXTGEN, INTSPACE) %>% 
  summarise(count = sum(WTSSALL))
# view result
analysis_df

# convert strongly agree/disagree to agree or disagree, then re-aggregate
# will be using stringr functions to handle character variables
analysis_df <- analysis_df %>% 
  mutate(NEXTGEN = str_remove_all(NEXTGEN, "Strongly"),
         NEXTGEN = str_to_lower(NEXTGEN),
         NEXTGEN = str_trim(NEXTGEN, side = c("both"))) %>% 
  # lower moves all rows to lower case in the data set
  # trim removes white space
  # remove_all removes all instances of the specified string
  group_by(NEXTGEN, INTSPACE) %>% 
  # IV first, DV second
  summarise(count = sum(count))

unique(analysis_df$NEXTGEN)

# reorder factor variable INTSPACE
analysis_df <- analysis_df %>% 
  mutate(INTSPACE = factor(INTSPACE))

str(analysis_df$INTSPACE)
# now shows a factor data type
# factor maps to categorical - there are levels, so 3 levels means 3 categories

# add data represented INTSPACE == "Not at all interested"
# add_rows <- tribble(
#   ~NEXTGEN, ~INTSPACE, ~count,
#   "agree", "Not at all interested", 1,
#   "disagree", "Not at all interested", 1)
# analysis_df <- bind_rows(analysis_df, add_rows)
# ^^ I commented this out because there was an error in the code that caused there 
# to be no rows for "Not at all interested". There were some people who answered this.

analysis_df <- analysis_df %>% 
  mutate(INTSPACE = factor(INTSPACE),
         NEXTGEN = factor(NEXTGEN)) %>% 
  arrange(NEXTGEN)

# create frequency table by reshaping data
frequency_table_df <- analysis_df %>% 
  spread(INTSPACE, count)

# spread a single variable - intspace

frequency_table_df
