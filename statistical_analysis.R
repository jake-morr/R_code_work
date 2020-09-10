##########################################
# TITLE: REGISTRATION_ANALYSIS_CLEANING
# AUTHOR: Jake Morrison
# DATE:               DETAIL:
#06/01/2020          created program
##########################################


# install.packages("ROracle")
# need to figure out how to get this package to work

# library(ROracle)    # Oracle databse connector
library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)    # for creating dummy variables
##library(ROracle)  # for connecting to ORacle databse

#-----------------------
# set working directory-
#-----------------------

setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/REGISTRATION_TIME")

rm(list = ls()) # remove all enviornment items

#---------------
# read in data -
#---------------

clean <- read.csv("clean.csv", header=T, sep =",")
holds <- read.csv("holds.csv", header=T, sep=",")

holds <- holds %>% mutate(HOLD_FROM_DATE =as.Date(HOLD_FROM_DATE, "%d-%b-%y"))
holds <- holds %>% mutate(HOLD_TO_DATE =as.Date(HOLD_TO_DATE, "%d-%b-%y"))

#---------------------
# limit sample to UG -
#---------------------

clean <- clean %>% filter(level == 'UG' | level == 'US')

clean <- mutate(clean, both = (clean$person %in% holds$person))

######################################################################
#
# These people all have holds
#
#registed_holds <- merge(holds, clean, by = "person")
#
#
#
#table(clean_2$both)
#
####################################################################

# All matching holds moved to clean data #

reg_holds <- merge(x = clean, y = holds, by = "person", all.x = TRUE)

#--------------------------
# build treatment/control -
#--------------------------

treatment <- reg_holds %>% filter(both == 'TRUE')

control <- reg_holds %>% filter (both == 'FALSE')

# need to put into a group if the person ever had a registration hold #

# test <- clean %>% group_by(person) %>% mutate(reg = sum(registration_hold == "Y"))


#-------------------
# means comparison -
#-------------------

summary(control$days_diff_avg)

summary(treatment$days_diff_avg)

#-----------------------------
# statisitcal means analysis -
#-----------------------------


t.test(clean$days_diff_avg, treatment$days_diff_avg)

var.test (clean$days_diff_avg, treatment$days_diff_avg)

######################################

reg_holds <- reg_holds %>% mutate(barrier = case_when(HOLD_FROM_DATE >= registration_date_able & registration_hold == 'Y' ~ "1",
                                                      TRUE ~ "0"))

reg_holds <- reg_holds %>% group_by(person) %>% mutate(reg = sum(barrier == "1"))

barrier <- reg_holds %>% filter(reg == '1')

no_barrier <- reg_holds %>% filter (reg =='0')

summary(barrier$days_dif)
summary (no_barrier$days_dif)

t.test(barrier$days_dif, no_barrier$days_dif)

var.test (barrier$days_dif, no_barrier$days_dif)



