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
colnames(ca) <- c('Name: First','Name: Last','Email','Phone','Date of Birth','Current Address: Street 1',
                  'Current Address: City','Current Address: Zip','Training Track of Interest','Race',
                  'Race2','Ethnicity','Do you speak a language other than English?','Please select languages spoken.',
                  'Highest Educational Attainment','US Veteran Status','Where did you attend the information session?',
                  'Proof of Boston Residency','Proof of Income','1-9/Citizenship Documentation','Proof of Education',
                  'Drug Test Form',"Valid Driver's License/Proof of Age","Valid Driver's License/Proof of Age",
                  'RMV Release Form','GLE Result','Driving Record Review Result','CORI Form','Emergency Contact Phone',
                  'Reported Family Size','Reported Yearly Household Income AND Calculated Household Income',
                  'Are you currently employed? Dont Use','Additional Income Sources','Please specify other income sources.',
                  'Are you currently employed?','Most Recent Job End Date','Total Years of Work Experience','Total Score',
                  'GLE Score','Applicant Status','EMT Cohort','Current Hourly Wage','Basic EMT Course')

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

ca$`Highest Educational Attainment` <- gsub('EMT',
                                        'Emergency Medical Technician (EMT)',
                                        ca$`Highest Educational Attainment`)



































