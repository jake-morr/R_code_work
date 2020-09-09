##########################################
# TITLE: MATH131 & MATH114 Analysis
# AUTHOR: Jake Morrison
# DATE:               DETAIL:
# 05/11/2020          created program
##########################################


# setup #
setwd("N:/Projects/MATH130_MATH131_ANALYSIS")

install.packages("dummies")

library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)    # for creating dummy variables


rm(list = ls()) # remove all enviornment items

# Read in Data

data <- read.csv("mathanalysis.csv", header=T, sep=",")

attach(data)

# Limiting term to 2019

data <- data [(data$period=="201940"),]

# Adds identifier for how many times student is observed
#   Since we are using one term, student should only ever be observed twice

data <- data %>% group_by(person) %>% add_count(person)

# Ensure the identifier ranges [1,2]

summary(data$'n')

# Subgroups for evaluation

# Grades for students who took MATH130 & MATH107

data131.114 <- data [(data$course=="MATH114") | (data$course=="MATH131"),]

# Splitting 130&107 student against just 107

data114 <- data131.114 [(data131.114$n==1),]

data131 <- data131.114 [(data131.114$n==2),]

# removing 130 grades

data114 <- data114[(data131$course=="MATH114"),]

data131 <- data131[(data131$course=="MATH114"),]

# F-test for significance

var.test (data114$grade.value, data131$grade.value)

# Not statistically different then each other. F = 1.4503 ~ p = 0.1379

# t-test

t.test(data114$grade.value, data131$grade.value)

# Not statistically significant. t= 1.1712 ~ p = 0.2448

# Ignore

#x <- dummy(data$course)
#get.dummy(x, 'course')
#x <- 0




datatest <- mutate(data114,pass = case_when(grade.value >0 ~ 1, grade.value == 0 ~ 0))

for(i in data114$person){
  datatest <- mutate(data114,pass = case_when(grade.value >0 ~ 1))
}

summary(datatest$pass)
  
data <- data %>% group_by(person) %>% add_count(person)


