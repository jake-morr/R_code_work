

rm(list = ls()) # remove all enviornment items
chn <- odbcConnect("odsp", uid = "jmorrison10", pwd = "Analog!563Wood")

data <- sqlQuery(chn, "select acst.ID
, acst.academic_period
, acst.program
, acst.person_uid

from academic_study acst 


where acst.academic_period >= '200640'
and acst.primary_program_ind = 'Y'
and acst.registered_ind = 'Y'

and (exists (select 'X'

from academic_study acst1

WHERE acst1.program in ('BS-COMPSC')
and acst1.academic_period >= '200640'
and acst1.primary_program_ind = 'Y'
and acst1.registered_ind = 'Y'
and acst1.person_uid  = acst.person_uid))

order by AcSt.ID, AcSt.academic_period")

setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/compsc_alternative_pathway")
enrollment <- read.csv("enrollment.csv", header = T, sep =",")

enrollment <- enrollment %>% group_by(PERSON_UID) %>% filter(ACADEMIC_PERIOD == max(ACADEMIC_PERIOD)) %>% ungroup


data <- data %>% group_by(PERSON_UID) %>% mutate("ranks" = rank(ACADEMIC_PERIOD)) %>% ungroup
data <- data %>% group_by(PERSON_UID) %>% filter(ranks == min(ranks)) %>% ungroup

data <- merge(x = data, y = enrollment, by = "PERSON_UID", all.x = TRUE)

sqldf("select distinct count(distinct PERSON_UID) from data")

data <- data %>% rename(GRADUATED_IND = MAX.AO.GRADUATED_IND.)
data <- data %>% rename(LAST_PROGRAM = PROGRAM.y)

sqldf("select distinct count(distinct PERSON_UID) from data where GRADUATED_IND = 'Y'") #642

sqldf("select distinct count(distinct PERSON_UID) from data where GRADUATED_IND = 'Y' and LAST_PROGRAM in ('BS-COMPSC','BCS-COMPSC')") #456

data <- data %>% mutate(stopped_out = case_when(ACADEMIC_PERIOD.y < '201940' & GRADUATED_IND != 'Y' ~ 1))

sqldf("select distinct count(distinct PERSON_UID) from data where stopped_out = '1'") #264



