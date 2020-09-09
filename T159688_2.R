setwd("N:/Projects/T159688_Gina/explore")

rm(list = ls()) # remove all enviornment items



#---------------
# read in data -
#---------------

today <- read.csv("export.csv", header=T, sep =",")

sqldf("select distinct count(distinct person_uid) from today")
