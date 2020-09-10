
setwd("N:/Projects/INSTITUTIONAL_ANALYSTICS_AND_PLANNING/compsc_alternative_pathway")

chn <- odbcConnect("odsp", uid = "jmorrison10", pwd = "Analog!563Wood")


persons <- sqlQuery(chn, "select acst.ID
, acst.academic_period
, acst.program
, acst.person_uid

from academic_study acst 

where acst.PROGRAM in ('BCS-COMPSC','BS-COMPSC')
and acst.academic_period >= '201040'
and acst.primary_program_ind = 'Y'
and acst.registered_ind = 'Y'

and (exists (select 'X'

from academic_study acst1

WHERE acst1.program = 'BS-COMPSC'
and acst1.academic_period >= '201040'
and acst1.primary_program_ind = 'Y'
and acst1.registered_ind = 'Y'
and acst1.person_uid  = acst.person_uid))


and (exists (select 'X'

from academic_study acst2

WHERE acst2.program = 'BCS-COMPSC'
and acst2.academic_period >= '201540'
and acst2.primary_program_ind = 'Y'
and acst2.registered_ind = 'Y'
and acst2.person_uid  = acst.person_uid))")

persons <- sqldf("select distinct PERSON_UID from persons")

# selecting all students records who had been a part of both programs #

data <- sqlQuery(chn, "select StC.id
, stc.person_uid
, StC.academic_period
, StC.course_identification
, StC.final_grade
, AcSt.program

from student_course StC

join academic_study AcSt
on StC.person_uid = AcSt.person_uid
and StC.academic_period = AcSt.academic_period

where AcSt.primary_program_ind = 'Y'
and StC.academic_period >= '201510'
and StC.final_grade not like '%T%'
and StC.person_uid in ('192844',
'404035',
'419162',
'577600',
'600305',
'605922',
'610023',
'622282',
'627809',
'631715',
'634346',
'640770',
'648639',
'650358',
'657076',
'657973',
'662993',
'677062',
'679978',
'686270',
'687394',
'688493',
'690730',
'692559',
'693076',
'699174',
'704266',
'707321',
'708631',
'711045',
'719280',
'723294',
'727234',
'727258',
'730370',
'732144',
'732231',
'738553',
'741957',
'742602',
'774361',
'776228',
'780000',
'787903',
'790402',
'791842',
'797380',
'799332',
'820754',
'821505',
'824703',
'834070',
'837435',
'838743',
'845485',
'846693',
'853557',
'894465',
'906097')

order by StC.person_uid")

data <- data %>% group_by(PERSON_UID, PROGRAM) %>% add_count(PERSON_UID,PROGRAM) %>% ungroup()

data <- data %>% group_by(PERSON_UID, PROGRAM) %>% mutate(row_id = row_number()) %>% ungroup()

data <- data %>% group_by(PERSON_UID) %>% mutate(row_id_1 = row_number()) %>% ungroup

data <- data %>% rename(change = chang)

data <- data %>% group_by(PERSON_UID) %>% mutate(change = row_id != row_id_1)

data <- data %>% group_by(PERSON_UID) %>% mutate(change = case_when(row_id == 1 & row_id_1 != 1 ~ 1,
                                                                    TRUE ~ 0))


write.csv(data,file = "explore.csv", row.names = FALSE)
