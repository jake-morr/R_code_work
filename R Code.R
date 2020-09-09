##########################################
# TITLE: T159443
# AUTHOR: Jake Morrison
# DATE:               DETAIL:
# 06/18/2020          created program
##########################################






library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)# for creating dummy variables
library(doBy)

setwd("N:/Projects/T159443_JC")

rm(list = ls()) # remove all enviornment items


data <- read.csv("export.csv", header=T, sep =",")

data <- data %>% group_by(Name) %>% add_count(Name)

data <- data %>% group_by(Name) %>% mutate(row_id = row_number()) %>% ungroup()

data <- data %>% filter(row_id == 1)

data <- data %>% subset(select = -c(row_id))

###### Done #######

table(data$FINAL_GRADE)

data <- data %>% mutate(PELL_FIRST_GEN = case_when(PELL_RECIPIENT == '1' & FIRST_GEN == '1' ~ 1,
                           TRUE ~ 0))


data <- data %>% mutate(ROW_ID = row_number())

output <- data %>% select(ROW_ID,PELL_RECIPIENT,FIRST_GEN,PELL_FIRST_GEN)

# The student population for this group is 1,930 #

summary(data$PELL_RECIPIENT)
summary(data$FIRST_GEN)
summary(data$PELL_FIRST_GEN)
summary(data$PELL_FIRST_GEN)

#------------
# write out -
#------------

write.csv(output, file = "output.csv", row.names = FALSE)
