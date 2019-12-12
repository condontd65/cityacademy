library(stringr)
library(data.table)
library(plyr)
library(stringdist)
library(dplyr)
library(anytime)
library(tidyr)

##### Input data #####
old <- read.csv('tables/city_academy_old.csv',stringsAsFactors = FALSE)

##### Add in dummy columns for future work #####

old$`Do you speak a language other than English?` <- NA
old$`Please specify other languages spoken` <- NA


##### Create new datatable for conversion #####

# take only data needed
ca <- data.frame(old$First.Name,
                 old$Last.Name,
                 old$Email.Address,
                 old$Phone.Number,
                 old$Date.of.Birth,
                 old$Current.street.address.where.you.reside..if.homeless..please.provide.shelter.name.,
                 old$City,
                 old$Zipcode,
                 old$Training.Track,
                 old$Race.Ethnicity1,
                 old$Race.Ethnicity2,
                 old$Race.Ethnicity3,
                 old$`Do you speak a language other than English?`,
                 old$Non.English.Lanugage.s.,
                 old$`Please specify other languages spoken`,
                 old$Highest.Edu,
                 old$Veteran.Status,
                 old$OWD.staff.use..Location.of.Info.Session,
                 old$OWD.staff.use..Boston.Residency.Confirmed..Y.N.,
                 old$OWD.staff.use..HUD.income.guidelines.confirmed..Y.N.,
                 old$OWD.staff.use..I.9.work.authorization.Confirmed..Y.N.,
                 old$OWD.staff.use..HS.Credential.Confirmed..Y.N.,
                 old$OWD.staff.use..Drug.testing.policy.agreement.signed..Y.N.,
                 old$OWD.staff.use..Copy.of.Driver.s.License..Y.N.,
                 old$OWD.staff.use..Driving.record.release.signed..Y.N.,
                 old$OWD.staff.use..Literacy.Grade.10.6.Confirmed..Y.N.,
                 old$OWD.staff.use..Driving.record.accepted..Y.N.,
                 old$OWD.staff.use..CORI.request.form.signed...EMT.track..Y.N.,
                 old$Emergency.Contact..Phone.No.,
                 old$Household.Size,
                 old$Household.Income,
                 old$Sources.of.Income1,
                 old$Sources.of.Income2,
                 old$Income.Other1,
                 old$Currently.Employed..Y.N.,
                 old$If.N..date.of.last.employment,
                 old$Years.of.work.experience,
                 old$Blended.Score..From.unlinked.scoring.sheet.,
                 old$TABE.Score,
                 old$Accepted.Enrollment,
                 old$Basic.EMT.Course.Placement,
                 old$Hourly.Wage,
                 old$Passed.Basic.EMT.Couse
                 ,stringsAsFactors = FALSE)

# Column Names
colnames(ca) <- c('Name: First','Name: Last',
                  'Email','Phone',
                  'Date of Birth','Current Address: Street 1',
                  'Current Address: City','Current Address: Zip',
                  'Training Track of Interest','Race',
                  'Race2','Ethnicity',
                  'Do you speak a language other than English?','Please select languages spoken.',
                  'Please Specify other languages spoken','Highest Educational Attainment',
                  'US Veteran Status','Where did you attend the information session?',
                  'Proof of Boston Residency','Proof of Income',
                  '1-9/Citizenship Documentation','Proof of Education',
                  'Drug Test Form',"Valid Driver's License/Proof of Age",
                  'RMV Release Form',
                  'GLE Result','Driving Record Review Result',
                  'CORI Form','Emergency Contact Phone',
                  'Reported Family Size','Reported Yearly Household Income AND Calculated Household Income',
                  'Are you currently employed? Dont Use','Additional Income Sources',
                  'Please specify other income sources.','Are you currently employed?',
                  'Most Recent Job End Date','Total Years of Work Experience',
                  'Total Score','GLE Score',
                  'Applicant Status','EMT Cohort',
                  'Current Hourly Wage','Basic EMT Course')

##### Transformations #####

## Pad zeros for zipcode
ca$`Current Address: Zip` <- str_pad(ca$`Current Address: Zip`,
                                     width = 5,
                                     side = 'left',
                                     pad = '0')

## Training track of interest translation
ca$`Training Track of Interest` <- gsub('EMT',
                                        'Emergency Medical Technician (EMT)',
                                        ca$`Training Track of Interest`)

ca$`Training Track of Interest` <- gsub('CDL/Hoisting',
                                        "Commercial Driver's License (CDL)/Hoisting",
                                        ca$`Training Track of Interest`)

## Race and ethnicity
ca$Ethnicity_real <- ifelse(ca$Race == 'Hispanic or Latina/o' | ca$Race2 == 'Hispanic or Latina/o' | ca$Ethnicity == 'Hispanic or Latina/o',
                       'Hispanic or Latino/a','Not Hispanic or Latino/a')

ca$Race2 [ ca$Race2 == 'Hispanic or Latina/o' ] <- ''
ca$Race [ ca$Race == 'Hispanic or Latina/o' ] <- ''

ca [ ca == ''] <- NA
ca [ ca == 'N/A'] <- NA

ca$Race2 <- ifelse(!is.na(ca$Ethnicity),
                  paste(ca$Race2,', ',ca$Ethnicity, sep = ''),
                  ca$Race2)

ca$Race <- ifelse(!is.na(ca$Race2) & !is.na(ca$Race),
                  paste(ca$Race,', ',ca$Race2, sep = ''),
                  ca$Race)

## Language
# Do you speak a language other than English
ca$`Do you speak a language other than English?` <- ifelse(!is.na(ca$`Please select languages spoken.`),
                                                           'Yes', 'No')
# Clean up the Please select languages spoken
#get list of languages (This is a mess, see if it's totally necessary)
#languages <- unique(unlist(strsplit(ca$`Please select languages spoken.`, ','))) %>%
 # sort()

#ca$`Please select languages spoken.` <- gsub('Amaric', 
 #                                          'Other',
  #                                           ca$`Please select languages spoken.`)

#ca$`Please select languages spoken.` <- gsub('Hindi', 
 #                                          'Other',
  #                                           ca$`Please select languages spoken.`)

#ca$`Please select languages spoken.` <- gsub('Hindo', 
 #                                           'Other',
  #                                           ca$`Please select languages spoken.`)

#ca$`Please select languages spoken.` <- gsub('Oromghna', 
 #                                           'Other',
  #                                           ca$`Please select languages spoken.`)

#ca$`Please select languages spoken.` <- gsub('Tigirinya', 
 #                                            'Other',
  #                                           ca$`Please select languages spoken.`)

#ca$`Please select languages spoken.` <- gsub('Tigrighna', 
 #                                            'Other',
  #                                           ca$`Please select languages spoken.`)


                                             
        
## Highest Educational Attainment

ca$`Highest Educational Attainment` <- gsub('HS Diploma',
                                            'High school diploma',
                                            ca$`Highest Educational Attainment`)

ca$`Highest Educational Attainment` <- gsub('Post Grad Degree',
                                            'Postgraduate degree',
                                            ca$`Highest Educational Attainment`)

## Veteran Status
ca$`US Veteran Status` <- gsub('Never Enrolled',
                               'Not a US veteran',
                               ca$`US Veteran Status`)

ca$`US Veteran Status` <- gsub('Veteran',
                               'US veteran',
                               ca$`US Veteran Status`)

ca$`US Veteran Status` <- gsub('Active Duty',
                               'US military active duty',
                               ca$`US Veteran Status`)

## Information Session
ca$`Where did you attend the information session?` <- gsub('El Centro',
                                                           'El Centro Del Cardenal, Catholic Charities',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub(' (4/28)',
                                                           '',
                                                           ca$`Where did you attend the information session?`, fixed = TRUE)

ca$`Where did you attend the information session?` <- gsub('BCYF Perkins',
                                                           'BCYF Perkins Community Center',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('RCFE',
                                                           'Roxbury Center for Financial Empowerment',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('rcfe',
                                                           'Roxbury Center for Financial Empowerment',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('charlestown adult ed',
                                                           'Other',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('AACA',
                                                           'Asian American Civic Association',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('EBNHC',
                                                           'East Boston Community Health Center',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('JVS',
                                                           'Mass Hire Boston (JVS)',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('bcl',
                                                           'Boston EMS',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('BCL',
                                                           'Boston EMS',
                                                           ca$`Where did you attend the information session?`)

ca$`Where did you attend the information session?` <- gsub('EMS Headquarters',
                                                           'Boston EMS',
                                                           ca$`Where did you attend the information session?`)

## Yes/No fields

# Proof of Boston Residency
ca$`Proof of Boston Residency` <- gsub('y',
                                       'Complete',
                                       ca$`Proof of Boston Residency`)

ca$`Proof of Boston Residency` <- gsub('Y',
                                       'Complete',
                                       ca$`Proof of Boston Residency`)

ca$`Proof of Boston Residency` <- gsub('N',
                                       'Incomplete',
                                       ca$`Proof of Boston Residency`)

# Proof of Income
ca$`Proof of Income` <- gsub('Y',
                             'Complete',
                             ca$`Proof of Income`)

ca$`Proof of Income` <- gsub('y',
                             'Complete',
                             ca$`Proof of Income`)

ca$`Proof of Income` <- gsub('N/A (Laid off in Dec)',
                             NA,
                             ca$`Proof of Income`, fixed = TRUE)

ca$`Proof of Income` <- gsub('N',
                             'Incomplete',
                             ca$`Proof of Income`)

# 1-9/Citizenship Documentation
ca$`1-9/Citizenship Documentation` <- gsub('y',
                                      'Complete',
                                      ca$`1-9/Citizenship Documentation`)

ca$`1-9/Citizenship Documentation` <- gsub('Y',
                                      'Complete',
                                      ca$`1-9/Citizenship Documentation`)

ca$`1-9/Citizenship Documentation` <- gsub('N',
                                      'Incomplete',
                                      ca$`1-9/Citizenship Documentation`)

# Proof of Education
ca$`Proof of Education` <- gsub('y',
                                'Complete',
                                ca$`Proof of Education`)

ca$`Proof of Education` <- gsub('Y',
                                'Complete',
                                ca$`Proof of Education`)

ca$`Proof of Education` <- gsub('N',
                                'Incomplete',
                                ca$`Proof of Education`)

# Drug Test Form
ca$`Drug Test Form` <- gsub('y',
                            'Complete',
                            ca$`Drug Test Form`)

ca$`Drug Test Form` <- gsub('Y',
                            'Complete',
                            ca$`Drug Test Form`)

ca$`Drug Test Form` <- gsub('N',
                            'Incomplete',
                            ca$`Drug Test Form`)

# Valid Driver's License/Proof of Age
ca$`Valid Driver's License/Proof of Age` <- gsub('y',
                                                 'Complete',
                                                 ca$`Valid Driver's License/Proof of Age`)

ca$`Valid Driver's License/Proof of Age` <- gsub('Y',
                                                 'Complete',
                                                  ca$`Valid Driver's License/Proof of Age`)

# RMV Release Form
ca$`RMV Release Form` <- gsub('y',
                              'Complete',
                               ca$`RMV Release Form`)

ca$`RMV Release Form` <- gsub('Y',
                              'Complete',
                               ca$`RMV Release Form`)

ca$`RMV Release Form` <- gsub('N',
                              'Incomplete',
                               ca$`RMV Release Form`)

# GLE Result
ca$`GLE Result` <- gsub('y',
                        'Complete',
                        ca$`GLE Result`)

ca$`GLE Result` <- gsub('Y',
                        'Complete',
                        ca$`GLE Result`)

ca$`GLE Result` <- gsub('N',
                        'Incomplete',
                        ca$`GLE Result`)

# Driving Record Review Result
ca$`Driving Record Review Result` <- gsub('y',
                        'Complete',
                        ca$`Driving Record Review Result`)

ca$`Driving Record Review Result` <- gsub('Y',
                        'Complete',
                        ca$`Driving Record Review Result`)

ca$`Driving Record Review Result` <- gsub('N',
                        'Incomplete',
                        ca$`Driving Record Review Result`)

# CORI Form
ca$`CORI Form` <- gsub('y',
                                          'Complete',
                                          ca$`CORI Form`)

ca$`CORI Form` <- gsub('Y',
                                          'Complete',
                                          ca$`CORI Form`)

ca$`CORI Form` <- gsub('N',
                                          'Incomplete',
                                          ca$`CORI Form`)

## Are you currenlty employed?
ca$`Are you currently employed?` <- gsub('Y',
                                         'Yes',
                                         ca$`Are you currently employed?`)

ca$`Are you currently employed?` <- gsub('y',
                                         'Yes',
                                         ca$`Are you currently employed?`)

ca$`Are you currently employed?` <- gsub('N',
                                         'No',
                                         ca$`Are you currently employed?`)

ca$`Are you currently employed?` <- gsub('n',
                                         'No',
                                         ca$`Are you currently employed?`)














































