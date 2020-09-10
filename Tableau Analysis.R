  ##########################################
  # TITLE: DFW Longitudinal Analysis
  # AUTHOR: Jake Morrison
  # DATE:               DETAIL:
  # 06/09/2020          created program
  ##########################################
  
  # setup #
  
  setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/DFW_LONGITUDINAL_ANALYSIS/Tableau Exploration")
  
  
  library(tidyr)      # data manipulation
  library(dplyr)      # data manipulation
  library(stringr)
  library(purrr)
  library(ggplot2)    # data visualization
  library(data.table) # data loading speed
  library(Hmisc)      # for %nin%
  library(dummies)    # for creating dummy variables
  library(doBy)
  library(sqldf)      # for writing SQL R
  library(data.table)
  
  
  rm(list = ls()) # remove all enviornment items
  
  #---------------
  # read in data -
  #---------------
  
  data <- read.csv("export.csv", header=T, sep =",")
  enrollment <- read.csv("enrollment.csv", header = T, sep =",")
 
 
  # ------------------
  # - rename columns -
  # ------------------
  
  enrollment <- enrollment %>% rename(person = PERSON)
  data <- data %>% rename(person = PERSON_UID)
  data <- data %>% rename(course = COURSE)
  data <- data %>% rename(major_at_time = MAJOR_AT_TIME)
  data <- data %>% rename(college_at_time = COLLEGE_AT_TIME)
  
  # -------------------------------------------------------------
  # - create number of observation fields to identify repeaters -
  # -------------------------------------------------------------
  
  data <- data %>% group_by(person, course, transfer_course) %>% add_count(person) %>% ungroup
  
  data <- data %>% rename(repeats = n)
  
  data <- data %>% mutate(repeated_ind = ifelse(repeats > 1, 1,0))
  
# named repeated_ind because repeat is a function
  
  #-----------------------
  # transfer course ind -
  #----------------------
  
  data <- data %>% group_by(person,course) %>% mutate(transfer_course_ind = case_when(transfer_course == 'Y' ~ 1,
                                                         TRUE ~ 0))

  # -----------------------------------
  # - filter irrelevant* grade values -
  # -----------------------------------,lf
  
  data$FINAL_GRADE <- gsub("[][()]", "", data$FINAL_GRADE)
    
  # data <- data %>% filter(FINAL_GRADE != '(0.7)')
  # data <- data %>% filter(FINAL_GRADE != '(0.0)')
  data <- data %>% filter(FINAL_GRADE != "NC")
  data <- data %>% filter(FINAL_GRADE != "P")
  data <- data %>% filter(FINAL_GRADE != "XC")
  data <- data %>% filter(FINAL_GRADE != "XF")
  data <- data %>% filter(FINAL_GRADE != "Y")
  
  
  # ----------------------------------------------------------
  # - conver number grades to letter grades 
  # -- can make this number grades to letter grade if easier -
  # ----------------------------------------------------------  
  
  data <- data %>% mutate(FINAL_GRADE = case_when(FINAL_GRADE == 'A' ~ "4.0",
                                                  FINAL_GRADE == 'A-' ~ "3.7",
                                                  FINAL_GRADE == 'B+' ~ "3.3",
                                                  FINAL_GRADE == 'B' ~ "3.0",
                                                  FINAL_GRADE == 'B-' ~ "2.7",
                                                  FINAL_GRADE == 'C+' ~ "2.3",
                                                  FINAL_GRADE == 'C' ~ "2.0",
                                                  FINAL_GRADE == 'C-' ~ "1.7",
                                                  FINAL_GRADE == 'D+' ~ "1.3",
                                                  FINAL_GRADE == 'D' ~ "1.0",
                                                  FINAL_GRADE == 'D-' ~ "0.7",
                                                  FINAL_GRADE == 'F' ~ "0.0",
                                                  FINAL_GRADE == 'W' ~ "-0.1",
                                                  TRUE ~ FINAL_GRADE))

  #-----------------------------------------
  #- create outcome if course was repeated -
  #-----------------------------------------
  
  # highest rank of academic period = last period taking the course
  
  data <- data %>% group_by(person,course,transfer_course) %>% mutate("ranks" = rank(ACADEMIC_PERIOD)) %>% ungroup
  
  data <- data %>% mutate(fix = case_when(ranks %like% ".5" ~ 1 ))
  
  data1 <- data %>% filter(fix == 1)
  
  data1 <- data1 %>% group_by(person,course,ACADEMIC_PERIOD) %>% filter(FINAL_GRADE == max(as.numeric(FINAL_GRADE))) %>% ungroup
  
  data1 <- data1 %>% group_by(person,course,ACADEMIC_PERIOD) %>% mutate(row_id = row_number()) %>% ungroup
  
  data1 <- data1 %>% filter(row_id == 1)
  
  data1 <- data1 %>% select(-c(row_id))
  
  data2 <- data %>% filter(is.na(fix))
  
  data <- rbind(data2,data1)
  
  data <- data %>% group_by(person,course,transfer_course) %>% mutate("ranks" = rank(ACADEMIC_PERIOD)) %>% ungroup
  
  table(data$ranks)
  
  
  #--------------------------------------------------------------------------
  
  #this gets rid of previous observations and I dont want to do that 
  
  #test <- data %>% group_by(person,course) %>% filter(ranks == max(ranks))
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  #
  # This code creates the final outcome grade as the highest grade acheived
  # problems include: 
  # - what if a student did not get a better grade the second time
  # - this is simply based on lowest grade, not value
  # --- F > W, but I think we are more intersted in the W showing
  # ------- some sort of case_when statement for indicators should be able to clear this up1
  # ---------- need to figure out how to populate a column conditionally
  
  #data <- data %>% group_by(person,course) %>% mutate(repeated_grade = case_when(observations >=2 ~ min(FINAL_GRADE) ))
  
  #-----------------------------------------------------------------------------------------------------------------------
  
  data <- data %>% group_by(person,course) %>% mutate (outcome = case_when(repeated_ind == 1 ~ "re-took class",)) %>% ungroup
  
  data <- data %>% group_by(person,course) %>% mutate (repeated_grade = case_when(ranks == max(ranks) & repeated_ind == '1' ~ FINAL_GRADE)) %>% ungroup
  
  #------------------------------------------------------
  #
  # This code fills the repeated_grade column for the repeated term
  # - I want to fill to repeated grade for all terms
  #
  #------------------------------------------------------
  
  data <- data %>% group_by(person,course) %>% mutate (repeated_grade = case_when(repeated_grade == is.na(repeated_grade) ~ repeated_grade,
                                                                                   TRUE ~ repeated_grade)) %>% ungroup
  # -----------------------------------------------------------------
  # - filter to keep students who received DFW or retook the course -
  # -----------------------------------------------------------------  
  
  #data <- data %>% filter(repeats >= 2 | FINAL_GRADE %in% c("D+","D","D-","F","W"))
  
  #--------------------------------------  
  #- generating enrollment gap variable -
  #--------------------------------------
  
  # this creates the last term for which the student enrolled #
  
  enrollment <- enrollment %>% group_by(person) %>% filter(ACADEMIC_PERIOD == max(ACADEMIC_PERIOD)) %>% ungroup

  # merge all enrollment data #  
    
  data <- merge(x = data, y = enrollment, by = "person", all.x = TRUE)
  
  data <- data %>% rename(latest_major = MAJOR)
  
  data <- data %>% rename(current_period = ACADEMIC_PERIOD.x)
  
  data <- data %>% rename(last_period = ACADEMIC_PERIOD.y)
  
  data <- data %>% rename(last_college = COLLEGE)
  
  data <- data %>% mutate(difference = last_period - current_period)
  
  data <- data %>% rename(GRADUATED_IND = MAX.AO.GRADUATED_IND.)
  
 
  # this calculates immediate stop outs #
  
  data <- data %>% mutate(outcome = case_when(difference == 0 & is.na(outcome) & last_period <= '201940' ~ 'stopped out immediatly',
                                                  TRUE ~ outcome))  
  
  # changed major #
  
  data <- data %>% mutate(changed_major = case_when(major_at_time != latest_major ~ 1,
                                                       TRUE ~ 0))

  data <- data %>% mutate(outcome = case_when(changed_major == 1 & is.na(outcome) ~ "changed major",
                                                  TRUE ~ outcome))
  # Changed college #
  
  data <- data %>% mutate(changed_college_ind = case_when(college_at_time != last_college ~ 1,
                                                    TRUE ~ 0))
  
  # stopped out ever #
  
  data <- data %>% mutate(outcome = case_when(last_period <= '201940' & is.na(outcome) & GRADUATED_IND != 'Y' ~ "stopped out",
                                                  TRUE ~ outcome))

  # stop out data validation table #
  
  stop_outs <- data %>% filter(outcome == "stopped out" | outcome == "stopped out immediatly")
  
  
  data <- data %>% mutate(outcome = case_when(GRADUATED_IND == "Y" & is.na(outcome) ~ "graduated without course",
                                                    TRUE ~ outcome ))
  # ----------------------
  #- sub_column outcomes -
  #-----------------------
  
  
  data <- data %>% mutate(sub_outcome = case_when(outcome == "re-took class" & GRADUATED_IND == 'Y' & major_at_time == latest_major ~ "re-took & graduated in major"))
  
  data <- data %>% mutate(sub_outcome = case_when(outcome == "re-took class" & GRADUATED_IND == 'Y' & major_at_time != latest_major ~ "re-took & graduated in new major",
                                                        TRUE ~ sub_outcome))
  
  
  data <- data %>% mutate(sub_outcome = case_when(outcome == "changed major" & GRADUATED_IND == 'Y' ~ "changed major & graduated",
                                                      TRUE ~ sub_outcome))
  # I will want to add in 202030 & 202040 eventually #
  
  data <- data %>% mutate(sub_outcome = case_when(last_period == '202020' & GRADUATED_IND != 'Y' ~ "still at University",
                                                      TRUE ~ sub_outcome))
  # due to bad coding this goes down here #
  
  data <- data %>% mutate(outcome = case_when(sub_outcome == "still at University" & is.na(outcome) ~ "still at University",
                                                    TRUE ~ outcome ))
  #---------------------------------------------
  #- creating indicator variables for outcomes -
  #---------------------------------------------
  
  data <- data %>% mutate(stopped_out_ind = case_when(last_period <= '201940' & GRADUATED_IND != 'Y' ~ 1
                                                            , TRUE ~ 0))
  
  data <- data %>% mutate(GRADUATED_IND_IND = case_when(GRADUATED_IND == 'Y' ~ 1
                                , TRUE ~ 0))
  
  data <- data %>% mutate(persisting = case_when(last_period > '201940' & GRADUATED_IND != 'Y' ~ 1
                                                        , TRUE ~ 0) )
  
  data <- data %>% mutate(changed_major_ind = (case_when(major_at_time != latest_major ~ 1,
                                                            TRUE ~0 )))
  
  data <- data %>% mutate(changed_major_grad = case_when(GRADUATED_IND_IND == '1' & changed_major_ind == '1' ~ 1,
                                                         TRUE ~ 0))
  
  #################################################################################################
  
  data <- data %>% mutate(first_outcome = case_when(stopped_out_ind == '1' ~ "stopped out"
                                                    , GRADUATED_IND_IND == '1' ~ "graduated"
                                                    , persisting == '1' ~ "persisting"))
  
  data <- data %>% mutate(second_outcome = case_when(changed_major_ind == '1' & GRADUATED_IND_IND == '1' ~ "changed major and graduated"
                                                     , changed_major_ind == '1' & persisting == '1' ~ "changed major and persisting"
                                                     , changed_major_ind == '1' & stopped_out_ind == '1' ~ "changed major and stopped out"
                                                     , changed_major_ind == '0' & GRADUATED_IND_IND == '1' ~ "graduated in major"
                                                     , changed_major_ind == '0' & persisting == '1' ~ "persisting in major"
                                                     , changed_major_ind == '0' & stopped_out_ind == '1' ~ "stopped out in major"))
  
  
  data <- data %>% filter(ranks != 1.5)
  data <- data %>% filter(ranks != 4.5)
  
  #--------------------
  #- writing out data -
  #--------------------
  
  setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/DFW_LONGITUDINAL_ANALYSIS/Tableau Exploration")
   write.csv(data, file = "data_test.csv", row.names = FALSE)
