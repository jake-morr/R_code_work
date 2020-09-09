library(tidyverse)
install.packages("haven")
install.packages("psych")
library(psych)
library(haven)

library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)    # for creating dummy variables


rm(list = ls(all = TRUE)) # remove all enviornment items

setwd("N:/Projects/JED")

################
# Read in data #
################

jed <- read_dta(file= "EastWash.dta")

###########################################
# generating race and sexuality variables #
###########################################

jed <- jed %>% mutate (race = case_when(race_black == 1 ~ "Black"
                                        , race_ainaan == 1 ~ "American Indian or Alaskan native"
                                        , race_asian == 1 ~ "Asian American / Asian"
                                        , race_his == 1 ~ "Hispanic / Latino/a"
                                        , race_pi == 1 ~ "Native Hawaiian or Pacific Islander"
                                        , race_mides == 1 ~ "Middle Eastern, Arab, or Arab American"
                                        , race_white == 1 ~ "White"
                                        , race_other == 1 ~ "Other"
                                        , TRUE ~ ""))

jed <- jed %>% mutate(sexuality = case_when(sexual_h == 1 ~ "heterosexual"
                                            , sexual_l == 1 ~ "lesbian"
                                            , sexual_g == 1 ~ "gay"
                                            , sexual_bi == 1 ~ "bi"
                                            , sexual_other == 1 ~ "other"
                                            , sexual_queer == 1 ~ "queer"
                                            , sexual_quest == 1 ~ "quest"
                                            , TRUE ~ ""))

#############################
# selecting data for env_mh #
#############################

data_env_mh <- jed %>% select(env_mh,age,sex_birth,gender,sexual_h,sexual_l,sexual_g,sexual_bi,sexual_queer,sexual_quest,
                       sexual_other,relship,race_black,race_ainaan,race_asian,race_his,race_pi,race_mides,race_white,
                       race_other,enroll,gpa_sr,aca_impa,persist)

data_env_mh <- data_env_mh %>% mutate(sexuality = case_when(sexual_h == 1 ~ "heterosexual"
                                        , sexual_l == 1 ~ "lesbian"
                                        , sexual_g == 1 ~ "gay"
                                        , sexual_bi == 1 ~ "bi"
                                        , sexual_other == 1 ~ "other"
                                        , sexual_queer == 1 ~ "queer"
                                        , sexual_quest == 1 ~ "quest"
                                        , TRUE ~ ""))

data_env_mh <- data_env_mh %>% mutate (lgbtq = case_when(sexuality == "heterosexual" ~ 0
                                           , sexuality == "lesbian" ~ 1
                                           , sexuality == "gay" ~ 1
                                           , sexuality == "bi" ~ 1
                                           , sexuality == "other" ~ 1
                                           , sexuality == "queer" ~ 1
                                           , sexuality == "quest" ~ 1
                                           , TRUE ~ 2 ))

table(data_env_mh$lgbtq)

data_env_mh <- data_env_mh %>% mutate (race = case_when(race_black == 1 ~ "Black"
                                          , race_ainaan == 1 ~ "American Indian or Alaskan native"
                                          , race_asian == 1 ~ "Asian American / Asian"
                                          , race_his == 1 ~ "Hispanic / Latino/a"
                                          , race_pi == 1 ~ "Native Hawaiian or Pacific Islander"
                                          , race_mides == 1 ~ "Middle Eastern, Arab, or Arab American"
                                          , race_white == 1 ~ "White"
                                          , race_other == 1 ~ "Other"
                                          , TRUE ~ ""))


table(data_env_mh$race)

table(data_env_mh$sexuality,data$race)

table(data_env_mh$lgbtq,data_env_mh$env_mh)


data_env_mh_lgbtq <- data_env_mh %>% filter(lgbtq == 1)
data_env_mh_non_lgbtq <- data_env_mh %>% filter(lgbtq == 0)
data_env_mh_male <- data_env_mh %>% filter(gender == 1)
data_env_mh_female <- data_env_mh %>% filter(gender == 2)
data_env_mh_black <- data_env_mh %>% filter (race == "Black")
data_env_mh_americanindian_alaskannative <- data_env_mh%>% filter (race == "American Indian or Alaskan native")
data_env_mh_asian <- data_env_mh%>% filter (race == "Asian American / Asian")
data_env_mh_pacific_islander <- data_env_mh %>% filter (race == "Native Hawaiian or Pacific Islander")
data_env_mh_middle_eastern <- data_env_mh %>% filter (race == "Middle Eastern, Arab, or Arab American")
data_env_mh_white <- data_env_mh %>% filter (race == "White")
data_env_mh_other<- data_env_mh %>% filter (race == "Other")
data_env_mh_hispanic <- data_env_mh%>% filter (race == "Hispanic / Latino/a")

data_env_mh_bi <- data_env_mh %>% filter(sexuality == "bi")
data_env_mh_gay <- data_env_mh %>% filter(sexuality == "gay")
data_env_mh_heterosexual <- data_env_mh %>% filter(sexuality == "heterosexual")
data_env_mh_lesbian <- data_env_mh %>% filter(sexuality == "lesbian")
data_env_mh_other <- data_env_mh %>% filter(sexuality == "other")
data_env_mh_queer <- data_env_mh %>% filter(sexuality == "queer")
data_env_mh_quest <- data_env_mh %>% filter(sexuality == "quest")

data_env_mh_hispanic_male <- data_env_mh_hispanic %>% filter(gender == 1)
data_env_mh_hispanic_female <- data_env_mh_hispanic %>% filter(gender == 2)
data_env_mh_white_male <- data_env_mh_white %>% filter(gender == 1)
data_env_mh_white_female <- data_env_mh_white %>% filter(gender == 2)

data_env_mh_white_male_lgbtq <- data_env_mh_white_male %>% filter(lgbtq == 1)
data_env_mh_white_male_non_lgbtq <- data_env_mh_white_male %>% filter(lgbtq == 0)
data_env_mh_white_female_lgbtq <- data_env_mh_white_female %>% filter(lgbtq == 1)
data_env_mh_white_female_non_lgbtq <- data_env_mh_white_female %>% filter(lgbtq == 0)


data_categorical <- jed %>% select(env_mh,camp_supp,fincur,belong1,belong8,race,sexuality)




###################
# env_mh analysis #
###################


mean(data_env_mh_americanindian_alaskannative$env_mh, na.rm = TRUE)
mean(data_env_mh_asian$env_mh, na.rm = TRUE)
mean(data_env_mh_black$env_mh, na.rm = TRUE)
mean(data_env_mh_hispanic$env_mh, na.rm = TRUE)
mean(data_env_mh_middle_eastern$env_mh, na.rm = TRUE)
mean(data_env_mh_pacific_islander$env_mh, na.rm = TRUE)
mean(data_env_mh_white$env_mh, na.rm = TRUE)
mean(data_env_mh_lgbtq$env_mh, na.rm = TRUE)
mean(data_env_mh_non_lgbtq$env_mh, na.rm = TRUE)
mean(data_env_mh_male$env_mh, na.rm = TRUE)
mean(data_env_mh_female$env_mh, na.rm = TRUE)
describe(data_env_mh_black$env_mh)

describe(data_env_mh_americanindian_alaskannative$env_mh)
describe(data_env_mh_asian$env_mh)
describe(data_env_mh_black$env_mh)
describe(data_env_mh_hispanic$env_mh)
describe(data_env_mh_middle_eastern$env_mh)
describe(data_env_mh_pacific_islander$env_mh)
describe(data_env_mh_other$env_mh)
describe(data_env_mh_white$env_mh)
table(jed$env_mh, useNA = "always")

describe(data_env_mh_bi$env_mh)
describe(data_env_mh_gay$env_mh)
describe(data_env_mh_heterosexual$env_mh)
describe(data_env_mh_lesbian$env_mh)
describe(data_env_mh_other$env_mh)
describe(data_env_mh_queer$env_mh)
describe(data_env_mh_quest$env_mh)
describe(data_env_mh_bi$env_mh)

describe(data_env_mh_hispanic_male$env_mh)
describe(data_env_mh_hispanic_female$env_mh)
describe(data_env_mh_white_male$env_mh)
describe(data_env_mh_white_female$env_mh)
describe(data_env_mh_white_male_non_lgbtq$env_mh)
describe(data_env_mh_white_male_lgbtq$env_mh)


# statistical testing #

# lgbtq #

t.test(data_env_mh_lgbtq$env_mh, data_env_mh_non_lgbtq$env_mh)

var.test(data_env_mh_lgbtq$env_mh, data_env_mh_non_lgbtq$env_mh)

# gender #

t.test(data_env_mh_female$env_mh, data_env_mh_male$env_mh)

var.test(data_env_mh_female$env_mh, data_env_mh_male$env_mh)

# race #
# white #

# Ameircan Indian Alaskan native

t.test(data_env_mh_white$env_mh, data_env_mh_americanindian_alaskannative$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_americanindian_alaskannative$env_mh)

# Asian 

t.test(data_env_mh_white$env_mh, data_env_mh_asian$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_asian$env_mh)

# Black

t.test(data_env_mh_white$env_mh, data_env_mh_black$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_black$env_mh)

# Hispanic Latino/A
# Var-test **
# closest to significant

t.test(data_env_mh_white$env_mh, data_env_mh_hispanic$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_hispanic$env_mh)

# Middle Eastern, Arab, or Arab American

t.test(data_env_mh_white$env_mh, data_env_mh_middle_eastern$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_middle_eastern$env_mh)

# Native Hawaiian or Pacific Islander

t.test(data_env_mh_white$env_mh, data_env_mh_pacific_islander$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_pacific_islander$env_mh)

# Other
# T-test *

t.test(data_env_mh_white$env_mh, data_env_mh_other$env_mh)

var.test(data_env_mh_white$env_mh, data_env_mh_other$env_mh)

# Sexuality #

# lgtbq compared to non #
# T-test **

t.test(data_env_mh_non_lgbtq$env_mh, data_env_mh_lgbtq$env_mh)

var.test(data_env_mh_non_lgbtq$env_mh, data_env_mh_lgbtq$env_mh)

# heterosexual #

# bi
# T-Test *

t.test(data_env_mh_heterosexual$env_mh, data_env_mh_bi$env_mh)

var.test(data_env_mh_heterosexual$env_mh, data_env_mh_bi$env_mh)

# Gay

t.test(data_env_mh_heterosexual$env_mh, data_env_mh_gay$env_mh)

var.test(data_env_mh_heterosexual$env_mh, data_env_mh_gay$env_mh)

# lesbian

t.test(data_env_mh_heterosexual$env_mh, data_env_mh_lesbian$env_mh)

var.test(data_env_mh_heterosexual$env_mh, data_env_mh_lesbian$env_mh)

# other

t.test(data_env_mh_heterosexual$env_mh, data_env_mh_other$env_mh)

var.test(data_env_mh_heterosexual$env_mh, data_env_mh_other$env_mh)

# queer

t.test(data_env_mh_heterosexual$env_mh, data_env_mh_queer$env_mh)

var.test(data_env_mh_heterosexual$env_mh, data_env_mh_queer$env_mh)

# quest

t.test(data_env_mh_heterosexual$env_mh, data_env_mh_quest$env_mh)

var.test(data_env_mh_heterosexual$env_mh, data_env_mh_quest$env_mh)

# gender#


t.test(data_env_mh_male$env_mh, data_env_mh_female$env_mh)

var.test(data_env_mh_male$env_mh, data_env_mh_female$env_mh)

describe(data_env_mh_female$env_mh)
describe(data_env_mh_male$env_mh)

# gender and race #

t.test(data_env_mh_white_male_lgbtq$env_mh, data_env_mh_white_male_non_lgbtq$env_mh)

var.test(data_env_mh_white_male_lgbtq$env_mh, data_env_mh_white_male_non_lgbtq$env_mh)

t.test(data_env_mh_white_female_lgbtq$env_mh, data_env_mh_white_female_non_lgbtq$env_mh)

var.test(data_env_mh_white_female_lgbtq$env_mh, data_env_mh_white_female_non_lgbtq$env_mh)

###############################
# plotting response variables #
###############################

data_categorical <- jed %>% select(env_mh,camp_supp,fincur,belong1,belong8,race,sexuality)

describe(data_categorical)

ggplot(data=data_categorical,aes(x=env_mh))+geom_bar(color="white",fill="black")+
  geom_vline(aes(xintercept=mean(env_mh,na.rm = TRUE)),color="blue", linetype="solid", size=1)+
  geom_vline(aes(xintercept=median(env_mh,na.rm = TRUE)),color="red", linetype="solid", size=1)+
  labs(title = "Distribution of env_mh")

ggplot(data=data_categorical,aes(x=camp_supp))+geom_bar(color="white",fill="black")+
  geom_vline(aes(xintercept=mean(camp_supp,na.rm = TRUE)),color="blue", linetype="solid", size=1)+
  geom_vline(aes(xintercept=median(camp_supp,na.rm = TRUE)),color="red", linetype="solid", size=1)+
  labs(title = "Distribution of camp_supp")

ggplot(data=data_categorical,aes(x=fincur))+geom_bar(color="white",fill="black")+
  geom_vline(aes(xintercept=mean(fincur,na.rm = TRUE)),color="blue", linetype="solid", size=1)+
  geom_vline(aes(xintercept=median(fincur,na.rm = TRUE)),color="red", linetype="solid", size=1)+
  labs(title = "Distribution of fincur")

ggplot(data=data_categorical,aes(x=belong1))+geom_bar(color="white",fill="black")+
  geom_vline(aes(xintercept=mean(belong1,na.rm = TRUE)),color="blue", linetype="solid", size=1)+
  geom_vline(aes(xintercept=median(belong1,na.rm = TRUE)),color="red", linetype="solid", size=1)+
  labs(title = "Distribution of belong1")

ggplot(data=data_categorical,aes(x=belong8))+geom_bar(color="white",fill="black")+
  geom_vline(aes(xintercept=mean(belong8,na.rm = TRUE)),color="blue", linetype="solid", size=1)+
  geom_vline(aes(xintercept=median(belong8,na.rm = TRUE)),color="red", linetype="solid", size=1)+
  labs(title = "Distribution of belong8")

table(jed$race)
table(jed$sexuality)


write.csv(jed, file = 'EastWash.csv', row.names =  FALSE)
