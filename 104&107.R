##########################################
# TITLE: MTHD104 & MATH107 Analysis
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

data104.107 <- data [(data$course=="MATH107") | (data$course=="MATH130") | (data$course=="MTHD104"),]

# Splitting 130&107 student against just 107

data107 <- data104.107 [(data104.107$n==2),]

data104 <- data104.107 [(data104.107$course=="MTHD104"),]

# removing 130 grades

data107 <- data107[(data107$course=="MATH107"),]

data104 <- data104[(data104$course=="MTHD104"),]

# F-test for significance

var.test (data107$grade.value, data104$grade.value)

# statistically different then each other. F = 1.7281 ~ p = 0.05132
# although significant, this isnt highly significant

# t-test

t.test(data107$grade.value, data104$grade.value)

# statistically significant. t= -3.9248 ~ p = 0.001763

summary(data104$grade.value)
summary(data107$grade.value)

# Ignore

#x <- dummy(data$course)
#get.dummy(x, 'course')
#x <- 0





