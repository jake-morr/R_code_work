
setwd("N:/Projects/T160736/3")


library(tidyr)      # data manipulation
library(dplyr)      # data manipulation
library(stringr)
library(purrr)
library(ggplot2)    # data visualization
library(data.table) # data loading speed
library(Hmisc)      # for %nin%
library(dummies)# for creating dummy variables
library(doBy)
library(forecast)
library(sqldf)
library(stringr)


rm(list = ls()) # remove all enviornment items

data <- read.csv("export.csv")

data <- data %>% group_by(ID) %>% add_count(ID)

data <- data %>% filter(COURSE_IDENTIFICATION == 'MATH107' | COURSE_IDENTIFICATION =='MTHD106' & FINAL_GRADE <= 'C+')

data <- data %>% filter(n >= 2)

data$FINAL_GRADE <- gsub("[][()]", "", data$FINAL_GRADE)

#data <- data %>% filter(COURSE_IDENTIFICATION == 'MTHD106') %>% mutate(FINAL_GRADE = substr(FINAL_GRADE,2,3))

#data <- data %>% filter(FINAL_GRADE <= 'C')


data <- data %>% group_by(ID) %>% add_count(ID)

attach(data)

data <- data[order(-n),]

detach(data)

data <- data[ -c(1,2), ]

output <- data %>% select(NAME,ID,ACADEMIC_PERIOD,COURSE_IDENTIFICATION,FINAL_GRADE)

write.csv(output, "3.csv", row.names = FALSE)
