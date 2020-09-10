setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/compsc_alternative_pathway")



rm(list = ls()) # remove all enviornment items


enrollment <- read.csv("enrollment.csv", header = T, sep =",")


enrollment <- enrollment %>% group_by(PERSON_UID) %>% filter(ACADEMIC_PERIOD == max(ACADEMIC_PERIOD)) %>% ungroup


chn <- odbcConnect("odsp", uid = "jmorrison10", pwd = "Analog!563Wood")


data <- sqlQuery(chn, "select acst.ID
, acst.academic_period
, acst.program
, acst.person_uid

from academic_study acst 

where acst.PROGRAM in ('BS-MEGR','BS-MEGR P','BS-METECH','BS_METECH P')
and acst.academic_period >= '200640'
and acst.primary_program_ind = 'Y'
and acst.registered_ind = 'Y'

and (exists (select 'X'

from academic_study acst1

WHERE acst1.program in ('BS-MEGR', 'BS-MEGR P')
and acst1.academic_period >= '200640'
and acst1.primary_program_ind = 'Y'
and acst1.registered_ind = 'Y'
and acst1.person_uid  = acst.person_uid))


and (exists (select 'X'

from academic_study acst2

WHERE acst2.program in ('BS-METECH','BS-METCH P')
and acst2.academic_period >= '200640'
and acst2.primary_program_ind = 'Y'
and acst2.registered_ind = 'Y'
and acst2.person_uid  = acst.person_uid))

order by AcSt.ID, AcSt.academic_period")


data2 <- sqlQuery(chn, "select acst.ID
, acst.academic_period
, acst.program
, acst.person_uid

from academic_study acst 

where acst.PROGRAM in ('BS-MATE','BS-CMTE','BS-METECH','BS_METECH P')
and acst.academic_period >= '200640'
and acst.primary_program_ind = 'Y'
and acst.registered_ind = 'Y'

and (exists (select 'X'

from academic_study acst1

WHERE acst1.program in ('BS-METECH', 'BS-METECH P')
and acst1.academic_period >= '200640'
and acst1.primary_program_ind = 'Y'
and acst1.registered_ind = 'Y'
and acst1.person_uid  = acst.person_uid))


and (exists (select 'X'

from academic_study acst2

WHERE acst2.program in ('BS-MATE','BS-CMTE')
and acst2.academic_period >= '200640'
and acst2.primary_program_ind = 'Y'
and acst2.registered_ind = 'Y'
and acst2.person_uid  = acst.person_uid))

order by AcSt.ID, AcSt.academic_period")


data1 <- sqlQuery(chn, "select acst.ID
, acst.academic_period
, acst.program
, acst.person_uid

from academic_study acst 

where acst.PROGRAM in ('BS-MATE','BS-CMTE','BS-MEGR','BS_MEGR P')
and acst.academic_period >= '200640'
and acst.primary_program_ind = 'Y'
and acst.registered_ind = 'Y'

and (exists (select 'X'

from academic_study acst1

WHERE acst1.program in ('BS-MEGR', 'BS-MEGR P')
and acst1.academic_period >= '200640'
and acst1.primary_program_ind = 'Y'
and acst1.registered_ind = 'Y'
and acst1.person_uid  = acst.person_uid))


and (exists (select 'X'

from academic_study acst2

WHERE acst2.program in ('BS-MATE','BS-CMTE')
and acst2.academic_period >= '200640'
and acst2.primary_program_ind = 'Y'
and acst2.registered_ind = 'Y'
and acst2.person_uid  = acst.person_uid))

order by AcSt.ID, AcSt.academic_period")



data <- data %>% group_by(PERSON_UID) %>% mutate(row_id = row_number())

data <- data %>% group_by(PERSON_UID) %>% mutate(started = case_when(row_id == 1 & (PROGRAM == 'BS-MEGR' | PROGRAM == 'BS-MEGR P') ~ 1,
                                                                     TRUE ~ 0))

data <- data %>% filter(started == 1)

data <- rbind(data, data1, data2)

data <- data %>% group_by(PERSON_UID) %>% mutate("ranks" = rank(ACADEMIC_PERIOD)) %>% ungroup


data <- data %>% group_by(PERSON_UID) %>% filter(ranks == min(ranks)) %>% ungroup

data <- merge(x = data, y = enrollment, by = "PERSON_UID", all.x = TRUE)

write.csv(data,file = "explore.csv", row.names = FALSE)


