##########################################
# TITLE: REGISTRATION_ANALYSIS_CLEANING
# AUTHOR: Jake Morrison
# DATE:               DETAIL:
# 05/26/2020          created program
##########################################

# setup #

setwd("N:/Projects/T159688_Gina")


library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)    # for creating dummy variables


rm(list = ls()) # remove all enviornment items

#---------------
# read in data -
#---------------

registration <- read.csv("export.csv", header=T, sep =",")

#-------------
# clean data -
#-------------

#------------------------------------
# remove less than 201840 (for now) -
#------------------------------------

registration <- registration %>% filter(academic_period >= 201840)

# Add count for how many times the indvidual is observed #

registration <- registration %>% group_by(ID) %>% add_count(ID)

registration <- registration %>% rename(observations = n)

# Remove Observations with less than 2 #

registration <- registration %>% filter(observations > 1)

# reformat registration date to date #

registration <- registration %>% mutate(registration_date =as.Date(registration_date, "%d-%b-%y"))

# remove summer term #

registration <- registration %>% filter(academic_period != 201930)
registration <- registration %>% filter(academic_period != 202030)

# generate 'registration_able_date' based on banded credit hours #

registration <- registration %>% mutate(registration_date_able = case_when(credits >= 180 & academic_period == 201840 & level == 'UG' ~ '05-22-18'
                                                                           ,  between(credits, 150,179.9999) & academic_period == 201840 & level == 'UG' ~ '05-23-18'
                                                                           ,  between(credits, 120,149.9) & academic_period == 201840 & level == 'UG' ~ '05-24-18'
                                                                           ,  between(credits, 90,119.9) & academic_period == 201840 & level == 'UG' ~ '05-25-18'
                                                                           ,  between(credits, 60,89.9) & academic_period == 201840 & level == 'UG' ~ '05-29-18'
                                                                           ,  between(credits, 30,59.9) & academic_period == 201840 & level == 'UG' ~ '05-30-18'
                                                                           ,  between(credits, 0,29.9) & academic_period == 201840 & level == 'UG' ~ '05-31-18'
                                                                           ,  academic_period == 201840 & level == 'GR' | academic_period == 201840 & level == 'PB' ~ '05-21-18'
                                                                           , credits >= 180 & academic_period == 201910 & level == 'UG' ~ '11-12-18'
                                                                           ,  between(credits, 150,179.9) & academic_period == 201910 & level == 'UG' ~ '11-13-18'
                                                                           ,  between(credits, 120,149.9) & academic_period == 201910 & level == 'UG' ~ '11-14-18'
                                                                           ,  between(credits, 90,119.9) & academic_period == 201910 & level == 'UG' ~ '11-15-18'
                                                                           ,  between(credits, 60,89.9) & academic_period == 201910 & level == 'UG' ~ '11-16-18'
                                                                           ,  between(credits, 30,59.9) & academic_period == 201910 & level == 'UG' ~ '11-19-18'
                                                                           ,  between(credits, 0,29.9) & academic_period == 201910 & level == 'UG' ~ '11-20-18'
                                                                           ,  academic_period == 201910 & level == 'GR' | academic_period == 201910 & level == 'PB' ~ '11-11-18'
                                                                           , credits >= 180 & academic_period == 201920 & level == 'UG' ~ '02-27-19'
                                                                           ,  between(credits, 150,179.9) & academic_period == 201920 & level == 'UG' ~ '02-28-19'
                                                                           ,  between(credits, 120,149.9) & academic_period == 201920 & level == 'UG' ~ '03-01-19'
                                                                           ,  between(credits, 90,119.9) & academic_period == 201920 & level == 'UG' ~ '03-04-19'
                                                                           ,  between(credits, 60,89.9) & academic_period == 201920 & level == 'UG' ~ '03-05-19'
                                                                           ,  between(credits, 30,59.9) & academic_period == 201920 & level == 'UG' ~ '03-06-19'
                                                                           ,  between(credits, 0,29.9) & academic_period == 201920 & level == 'UG' ~ '03-07-19'
                                                                           ,  academic_period == 201920 & level == 'GR' | academic_period == 201920 & level == 'PB' ~ '02-26-19'
                                                                           , credits >= 180 & academic_period == 201940 & level == 'UG' ~ '05-21-19'
                                                                           ,  between(credits, 150,179.9) & academic_period == 201940 & level == 'UG' ~ '05-22-19'
                                                                           ,  between(credits, 120,149.9) & academic_period == 201940 & level == 'UG' ~ '05-23-19'
                                                                           ,  between(credits, 90,119.9) & academic_period == 201940 & level == 'UG' ~ '05-24-19'
                                                                           ,  between(credits, 60,89.9) & academic_period == 201940 & level == 'UG' ~ '05-28-19'
                                                                           ,  between(credits, 30,59.9) & academic_period == 201940 & level == 'UG' ~ '05-29-19'
                                                                           ,  between(credits, 0,29.9) & academic_period == 201940 & level == 'UG' ~ '05-30-19'
                                                                           ,  academic_period == 201940 & level == 'GR' | academic_period == 201940 & level == 'PB' ~ '05-20-19'
                                                                           , credits >= 180 & academic_period == 202010 & level == 'UG' ~ '11-07-19'
                                                                           ,  between(credits, 150,179.9) & academic_period == 202010 & level == 'UG' ~ '11-08-19'
                                                                           ,  between(credits, 120,149.9) & academic_period == 202010 & level == 'UG' ~ '11-12-19'
                                                                           ,  between(credits, 90,119.9) & academic_period == 202010 & level == 'UG' ~ '11-13-19'
                                                                           ,  between(credits, 60,89.9) & academic_period == 202010 & level == 'UG' ~ '11-14-19'
                                                                           ,  between(credits, 30,59.9) & academic_period == 202010 & level == 'UG' ~ '11-15-19'
                                                                           ,  between(credits, 0,29.9) & academic_period == 202010 & level == 'UG' ~ '11-18-19'
                                                                           ,  academic_period == 202010 & level == 'GR' | academic_period == 202010 & level == 'PB' ~ '11-06-19'
                                                                           , credits >= 180 & academic_period == 202020 & level == 'UG' ~ '02-25-20'
                                                                           ,  between(credits, 150,179.9) & academic_period == 202020 & level == 'UG' ~ '02-26-20'
                                                                           ,  between(credits, 120,149.9) & academic_period == 202020 & level == 'UG' ~ '02-27-20'
                                                                           ,  between(credits, 90,119.9) & academic_period == 202020 & level == 'UG' ~ '02-28-20'
                                                                           ,  between(credits, 60,89.9) & academic_period == 202020 & level == 'UG' ~ '03-02-20'
                                                                           ,  between(credits, 30,59.9) & academic_period == 202020 & level == 'UG' ~ '03-03-20'
                                                                           ,  between(credits, 0,29.9) & academic_period == 202020 & level == 'UG' ~ '03-04-20'
                                                                           ,  academic_period == 202020 & level == 'GR' | academic_period == 202020 & level == 'PB' ~ '02-24-20'))


# generate days difference #

registration <- registration %>% mutate(registration_date_able =as.Date(registration_date_able, "%m-%d-%y"))

registration <- registration %>% mutate(days_diff = difftime(registration_date, registration_date_able, units = "days"))

summary(as.integer(registration$days_diff))

registration$days_diff <- as.integer(registration$days_diff)

#

registration <- registration %>% group_by(ID) %>% mutate(count = row_number(ID))

registration <- registration %>% mutate(remove = case_when(count == 1 & class_standing == 'FR' ~ 1, TRUE ~ 0))

# create column with days average by student #

registration <- registration %>% group_by(ID) %>% mutate(days_diff_avg =mean(days_diff))

# This is a list of all student who registered in spring # 

registration <- registration %>% filter(academic_period == 202020 | academic_period == 202015)

registration$days_diff_avg <- round(registration$days_diff_avg, digits = 0)

registration <- registration %>% rename(GRADUATED_IND = MAX.AO.GRADUATED_IND.OVER.PARTITIONBYCSC.PERSON_UID.)

registration <- registration %>% rename(OUTCOME_GRADUATION_DATE = MAX.AO.OUTCOME_GRADUATION_DATE.OVER.PARTITIONBYCSC.PERSON_UID. )

registration <- registration %>% filter(GRADUATED_IND != 'Y')

registration <- registration %>% mutate(ACAD_STANDING_END_DESC = case_when(ACAD_STANDING_END_DESC == "" ~ ACADEMIC_STANDING_DESC,
                                                                           TRUE ~ ACAD_STANDING_END_DESC))

registration <- registration %>% group_by(name) %>% mutate(row_id = row_number())

registration <- registration %>% filter(row_id == 1)


#-----------------------
# write out clean data - 
#-----------------------

write.csv(registration, file = "clean.csv", row.names = FALSE)


