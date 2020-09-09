##########################################
# TITLE: MATH130 & MATH107 Analysis
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

#data <- data [(data$period=="201940"),]

data <- filter(data, period=="202010"  | period=="202010" )

# Adds identifier for how many times student is observed
#   Since we are using one term, student should only ever be observed twice

data <- data %>% group_by(person) %>% add_count(person)

# Ensure the identifier ranges [1,2]

summary(data$'n')
table(data$n)

data <- filter(data, n==1 | n==2)

table(data$n, data$course)

# Subgroups for evaluation

# Grades for students who took MATH130 & MATH107

data130.107 <- data [(data$course=="MATH107") | (data$course=="MATH130"),]

# Splitting 130&107 student against just 107

data107 <- data130.107 [(data130.107$n==1),]

data130 <- data130.107 [(data130.107$n==2),]

# removing 130 grades

data107 <- data107[(data107$course=="MATH107"),]

data130 <- data130[(data130$course=="MATH107"),]

# F-test for significance

var.test (data107$grade.value, data130$grade.value)

# Not statistically different then each other. F = 1.0274 ~ p = 0.9667

# t-test

t.test(data107$grade.value, data130$grade.value)

# Not statistically significant. t= -0.93397 ~ p = 0.3551

hist.data.frame(data130.107)

#x <- dummy(data$course)
#get.dummy(x, 'course')
#x <- 0





