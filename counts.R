
setwd("N:/Projects/T160736/7")


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


rm(list = ls()) # remove all enviornment items

mthd104 <- read.csv("mthd104.csv")
math114 <- read.csv("math114.csv")

combined <- merge(x=math114,y=mthd104, by = "ID", all.x = FALSE)

sqldf("select distinct count(distinct ID) from mthd104") #826
sqldf("select distinct count(distinct ID) from combined") #217

# 26.27% #


rm(list = ls()) # remove all enviornment items

mthd104 <- read.csv("mthd104.csv")
math200 <- read.csv("math200.csv")

combined <- merge(x=math200,y=mthd104, by = "ID", all.x = FALSE)

sqldf("select distinct count(distinct ID) from mthd104") #826
sqldf("select distinct count(distinct ID) from combined") #229

# 27.72 #


rm(list = ls()) # remove all enviornment items

mthd104 <- read.csv("mthd104.csv")
math208 <- read.csv("math208.csv")

combined <- merge(x=math208,y=mthd104, by = "ID", all.x = FALSE)

sqldf("select distinct count(distinct ID) from mthd104") #826
sqldf("select distinct count(distinct ID) from combined") #14

# 1.69% #


rm(list = ls()) # remove all enviornment items

mthd104 <- read.csv("mthd104.csv")
math121 <- read.csv("math121.csv")

combined <- merge(x=math121,y=mthd104, by = "ID", all.x = FALSE)

sqldf("select distinct count(distinct ID) from mthd104") #826
sqldf("select distinct count(distinct ID) from combined") #45

# 5.45% #

none <- read.csv("none.csv")

none <- none %>% group_by(ID) %>% mutate(delete = case_when(COURSE_IDENTIFICATION =='MATH114' | COURSE_IDENTIFICATION =='MATH200' | COURSE_IDENTIFICATION =='MATH208' | COURSE_IDENTIFICATION =='MATH121' ~ 1, TRUE ~ 0)) %>% ungroup

none <- none %>% group_by(ID) %>% filter(all(delete ==0))

sqldf("select distinct count(distinct ID) from none")

# 41.78% #
