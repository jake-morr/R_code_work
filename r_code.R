##########################################
# TITLE: DFW Longitudinal Analysis
# AUTHOR: Jake Morrison
# DATE:               DETAIL:
# 07/14/2020          created program
##########################################

# setup #

setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/compsc_alternative_pathway")


library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)    # for creating dummy variables
library(doBy)
library(sqldf)      # for writing SQL R
library(RODBC)


rm(list = ls()) # remove all enviornment items

#---------------
# read in data -
#---------------

data <- read.csv("export.csv", header=T, sep =",")
enrollment <- read.csv("enrollment.csv", header = T, sep =",")


# observations #

data <- data %>% group_by(PERSON_UID) %>% add_count(PERSON_UID)

data <- data %>% rename(repeats = n)

data <- data %>% mutate(repeated_ind = ifelse(repeats > 1, 1,0))

# grade conversion #


data <- data %>% mutate(FINAL_GRADE = case_when(FINAL_GRADE == '4.0' ~ "A",
                                                FINAL_GRADE == '3.9' ~ "A",
                                                FINAL_GRADE == '3.8' ~ "A-",
                                                FINAL_GRADE == '3.7' ~ "A-",
                                                FINAL_GRADE == '3.6' ~ "A-",
                                                FINAL_GRADE == '3.5' ~ "B+",
                                                FINAL_GRADE == '3.4' ~ "B+",
                                                FINAL_GRADE == '3.3' ~ "B+",
                                                FINAL_GRADE == '3.2' ~ "B+",
                                                FINAL_GRADE == '3.1' ~ "B",
                                                FINAL_GRADE == '3.0' ~ "B",
                                                FINAL_GRADE == '2.9' ~ "B",
                                                FINAL_GRADE == '2.8' ~ "C+",
                                                FINAL_GRADE == '2.7' ~ "B-",
                                                FINAL_GRADE == '2.6' ~ "C+",
                                                FINAL_GRADE == '2.5' ~ "C+",
                                                FINAL_GRADE == '2.4' ~ "C+",
                                                FINAL_GRADE == '2.3' ~ "C+",
                                                FINAL_GRADE == '2.2' ~ "C",
                                                FINAL_GRADE == '2.1' ~ "C",
                                                FINAL_GRADE == '2.0' ~ "C",
                                                FINAL_GRADE == '1.9' ~ "C-",
                                                FINAL_GRADE == '1.8' ~ "C-",
                                                FINAL_GRADE == '1.7' ~ "C-",
                                                FINAL_GRADE == '1.6' ~ "D+",
                                                FINAL_GRADE == '1.5' ~ "D+",
                                                FINAL_GRADE == '1.4' ~ "D+",
                                                FINAL_GRADE == '1.3' ~ "D+",
                                                FINAL_GRADE == '1.2' ~ "D+",
                                                FINAL_GRADE == '1.1' ~ "D",
                                                FINAL_GRADE == '1.0' ~ "D",
                                                FINAL_GRADE == '0.9' ~ "D",
                                                FINAL_GRADE == '0.8' ~ "D-",
                                                FINAL_GRADE == '0.7' ~ "D-",
                                                FINAL_GRADE == '0.0' ~ "F",
                                                TRUE ~ FINAL_GRADE))

data <- data %>% group_by(PERSON_UID) %>% mutate("ranks" = rank(ACADEMIC_PERIOD)) %>% ungroup

data <- data %>% group_by(PERSON_UID) %>% mutate (repeated_grade = case_when(ranks == max(ranks) & repeated_ind == '1' ~ FINAL_GRADE)) %>% ungroup

# Merge Enrollments #

enrollment <- enrollment %>% group_by(PERSON_UID) %>% filter(ACADEMIC_PERIOD == max(ACADEMIC_PERIOD)) %>% ungroup

data <- merge(x = data, y = enrollment, by = "PERSON_UID", all.x = TRUE)

data <- data %>% filter(repeats >= 2 | FINAL_GRADE %in% c("D+","D","D-","F","W"))

data1 <- data %>% group_by(PERSON_UID) %>% filter(ranks == max(ranks)) %>% ungroup

