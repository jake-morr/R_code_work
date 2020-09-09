##########################################
# TITLE: REGISTRATION_ANALYSIS_CLEANING
# AUTHOR: Jake Morrison
# DATE:               DETAIL:
# 05/26/2020          created program
##########################################



##install.packages("ROracle")
# need to figure out how to get this package to work

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

setwd("N:/Projects/T159688_Gina")

rm(list = ls(all = TRUE)) # remove all enviornment items



#---------------
# read in data -
#---------------

today <- read.csv("today.csv", header=T, sep =",")
registration <- read.csv("clean.csv", header=T, sep=",")
holds <- read.csv("holds.csv", header=T, sep=",")
summer <- read.csv("summer.csv", header=T, sep=",")



registration <- registration %>% subset(select = -c(row_id))

#--------------------------------------------
# clean current data and prepare from merge -
#--------------------------------------------

# convert registration date to date #

today <- today %>% mutate(registration_date =as.Date(registration_date, "%d-%b-%y"))

# generate matching columns #

registration <- registration %>% rename(Major = major)

today <- today %>% mutate(observations = "")
today <- today %>% mutate(registration_date_able = "")
today <- today %>% mutate(days_dif = "")
today <- today %>% mutate(count = "")
today <- today %>% mutate(remove = "")
today <- today %>% mutate(days_diff_avg = "")
today <- today %>% mutate(days_diff_avg = "")


today <- today %>% rename(days_diff = days_dif)


summer <- summer %>% mutate(observations = "")
summer <- summer %>% mutate(registration_date_able = "")
summer <- summer %>% mutate(days_dif = "")
summer <- summer %>% mutate(count = "")
summer <- summer %>% mutate(remove = "")
summer <- summer %>% mutate(days_diff_avg = "")
summer <- summer %>% mutate(days_diff_avg = "")


summer <- summer %>% rename(days_diff = days_dif)

# today <- today %>% rename(GRADUATED_IND = MAX.AO.GRADUATED_IND.OVER.PARTITIONBYCSC.PERSON_UID.)
# 
# today <- today %>% rename(OUTCOME_GRADUATION_DATE = MAX.AO.OUTCOME_GRADUATION_DATE.OVER.PARTITIONBYCSC.PERSON_UID. )


#--------
# merge -
#--------

combined <- rbind(registration, today, summer)

#--------------------------
# clean data for analysis -
#--------------------------

# remove 'observations' in order to replace with current number #

combined <- select(combined, -c(observations))

# add count of students to identify who has registered #

combined <- combined %>% group_by(ID) %>% add_count(ID) %>% ungroup

combined <- combined %>% rename(observations = n)

combined <- combined %>% mutate(summer_ind = case_when(academic_period == '202030' ~ 'Y'))

combined <- combined %>% mutate(summer_ind = case_when(academic_period == '202025' ~ 'Y', TRUE ~ summer_ind))

combined <- combined %>% group_by(ID) %>% mutate(summer = case_when(any(summer_ind == 'Y', na.rm = FALSE) ~ 'Y'))

combined <- combined %>% filter((academic_period != '202030'))

combined <- combined %>% filter ((academic_period != '202025'))

combined <- select(combined, -c(summer_ind))

combined <- select(combined, -c(observations))

combined <- combined %>% group_by(ID) %>% add_count(ID) %>% ungroup

combined <- combined %>% rename(observations = n)

# remove student who have registered for current term #

combined <- combined %>% filter(observations < 2)

# generate 'registration_able_date' based on banded credit hours #

combined <- combined %>% mutate(registration_date_able = case_when(credits >= 180 & academic_period == 202020 & level == 'UG' ~ '05-19-20'
                                                                   ,  between(credits, 150,179.9999) & academic_period == 202020 & level == 'UG' ~ '05-20-20'
                                                                   ,  between(credits, 120,149.9) & academic_period == 202020 & level == 'UG' ~ '05-21-20'
                                                                   ,  between(credits, 90,119.9) & academic_period == 202020 & level == 'UG' ~ '05-22-20'
                                                                   ,  between(credits, 60,89.9) & academic_period == 202020 & level == 'UG' ~ '05-26-20'
                                                                   ,  between(credits, 30,59.9) & academic_period == 202020 & level == 'UG' ~ '05-27-20'
                                                                   ,  between(credits, 0,29.9) & academic_period == 202020 & level == 'UG' ~ '05-28-20'
                                                                   ,  academic_period == 202020 & level == 'GR' | academic_period == 202020 & level == 'PB' ~ '05-18-20'
                                                                   ,  academic_period == 202015 ~ '05-20-20' ))
# format 'registration_able_date' to date format #

combined <- combined %>% mutate(registration_date_able =as.Date(registration_date_able, "%m-%d-%y"))

# change registration date to todays date for later calculations #

combined <- combined %>% mutate(registration_date = Sys.Date())

# generate days difference #

combined <- combined %>% mutate(days_dif = difftime(registration_date, registration_date_able, units = "days"))

#---------------------------------
# create late registration flags -
#---------------------------------

combined <- combined %>% mutate(late_flag = case_when(days_dif > days_diff_avg ~ 'Y'
                                                      , TRUE ~ 'N'))

combined <- combined %>% mutate(late_20_flag = case_when(days_dif > (as.numeric(days_diff_avg) +20) ~ 'Y'
                                                      , TRUE ~ 'N'))

#----------------
# add hold info -
#----------------

# limit to undergrad #

combined <- combined %>% filter(level == 'UG' | level == 'US')

combined <- combined %>% mutate(days_past = (Sys.Date() - registration_date_able))

holds <- holds %>% filter(active.indicator == 'Y')

combined_hold <- merge(x = combined, y = holds, by = "ID", all.x = TRUE)

combined_hold <- combined_hold %>% filter(!is.na(registration_date_able))

combined_hold <- combined_hold %>% filter(INTL == 'No')

combined_hold <- combined_hold %>% filter(STUDENT_POPULATION_DESC != 'International Exchange')

combined_hold <- combined_hold %>% filter(STUDENT_POPULATION_DESC != 'Current Running Start Student')

#----------------------------
# select columns for output -
#----------------------------

output <- combined_hold %>% select(name,ID,credits,term_gpa,cum_gpa,level,class_standing,Major,COLLEGE_DESC,ACAD_STANDING_END_DESC,advisor_type,advisor,HOLD_DESC,active.indicator, registration_hold,summer,last_status,OUTCOME_GRADUATION_DATE,ADMISSIONS_POPULATION_DESC,STUDENT_POPULATION_DESC)

output <- output %>% mutate_if(is.character, funs(ifelse(is.na(.),"",.)))

output <- output %>% rename(hold_description = HOLD_DESC)

output <- output %>% rename(student_population = STUDENT_POPULATION_DESC)

output <- output %>% rename(college = COLLEGE_DESC)

output <- output %>% rename(admissions_population = ADMISSIONS_POPULATION_DESC)

output <- output %>% rename(active_hold = active.indicator)

output <- output %>% rename(academic_standing = ACAD_STANDING_END_DESC)

output <- output %>% rename(graduation_status = last_status)

output <- output %>% rename(graduation_date_applied_for = OUTCOME_GRADUATION_DATE)

output <- output %>% filter(student_population != 'Non-Matriculated')

library("sqldf")

sqldf("select distinct count(distinct ID) from output")

#------------
# write out -
#------------

write.csv(output, file = "today_analysis.csv", row.names = FALSE)
