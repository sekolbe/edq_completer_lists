####################################################################################
# This script combines CSU 'employment' lists across multiple years to create a single,
# multi-year completer list that can be used for analysis and matching.
#
# S. Kolbe
####################################################################################

####################################################################################
# Set up
####################################################################################

library(dplyr)
library(magrittr)
library(stringr)
library(lubridate)
library(RODBC)

options(stringsAsFactors = FALSE)
options(scipen = 999)
  
####################################################################################
# Load CSU completer data
####################################################################################

full_comp_0910 <- read.csv("raw_data/EMPLOYMENT_2011 - 2009-2010 Completers.csv")
full_comp_1011 <- read.csv("raw_data/EMPLOYMENT_2012 - 2010-2011 Completers.csv")
full_comp_1112 <- read.csv("raw_data/EMPLOYMENT_2013 - 2011-2012 Completers.csv")
full_comp_1213 <- read.csv("raw_data/EMPLOYMENT_2014 - 2012-2013 Completers.csv")
full_comp_1314 <- read.csv("raw_data/EMPLOYMENT_2015 - 2013-2014 Completers.csv")
full_comp_1415 <- read.csv("raw_data/EMPLOYMENT_2016 - 2014-2015 Completers.csv")
full_comp_1516 <- read.csv("raw_data/EMPLOYMENT_2017 - 2015-2016 Completers.csv")
full_comp_1617 <- read.csv("raw_data/EMPLOYMENT_2018 - 2016-2017 Completers.csv")
full_comp_1718 <- read.csv("raw_data/EMPLOYMENT_2019 - 2017-2018 Completers.csv")
full_comp_1819 <- read.csv("raw_data/EMPLOYMENT_2020 - 2018-2019 Completers.csv")
full_comp_1920 <- read.csv("raw_data/EMPLOYMENT_2021 - 2019-2020 Completers.csv")
 
campus_code_xwalk <- read.delim("raw_data/campus_code_xwalk.txt")

####################################################################################
# Prep recent data
#
# Beginning with the 2018 survey year, the employment/completer files need some extra steps
# to be compatible with prior years' data due to a major change in file format.
####################################################################################

prep_recent_data <- function(recent_data){
  output <- recent_data %>%
    rename(., studyCode = Study.Code, 
           ssn = SSN, 
           lName = Last.Name, 
           fName = First.Name, 
           newDOB = DOB, 
           credentialCode = Credential.Code , 
           credentialDescription = Credential.Description,
           credentialIntReg = Credential.Int.Reg, 
           credentialExamProg = Credential.Exam.Prog,   
           contentCode = Content.Code, 
           contentDescription = Content.Description,
           campusAbb = Campus, 
           campusID = Campus.ID, 
           campusType = System.NumbeCode,       
           surveyYear = CSU.Cohort, 
           programType = Program.Type,          
           teacherAddress = Address, 
           teacherAddressAddtl = Addt.l, 
           teacherCity = City,                   
           teacherState = State, 
           teacherZip = Zip,
           districtID = District.ID, 
           schoolID = School.ID,
           countyID = County.ID, 
           countyName = County.Name            
    ) %>%
    select(., -teacherAddressAddtl)
  output$campusType %<>% recode(., `1` = "CSU", `2` = "CPU")
  output$surveyYear <- output$surveyYear + 2000 
  output$School.Name[!is.na(output$School.Name) & output$School.Name == "" & !is.na(output$Other.School.Name) & output$Other.School.Name != ""] <- 
    output$Other.School.Name[!is.na(output$School.Name) & output$School.Name == "" & !is.na(output$Other.School.Name) & output$Other.School.Name != ""]
  output$Other.School.Name <- NULL
  return(output)
}

full_comp_1617 %<>% prep_recent_data(.)
full_comp_1718 %<>% prep_recent_data(.)
full_comp_1819 %<>% prep_recent_data(.)
full_comp_1920 %<>% prep_recent_data(.)

rm(prep_recent_data)

####################################################################################
# Get a list of fields present across datasets
####################################################################################

# What fields do the older and newer files have in common? This will form the basis for the combined file. 
# The 2009-10 and 2019-20 files represent what's present in the recent and older files, respectively.
common_fields <- intersect(names(full_comp_1920),names(full_comp_0910))

####################################################################################
# Combine CSU completer/employment data
# Because the file layout changes each year, each file has its own preparation steps.
####################################################################################

comp_0910 <- full_comp_0910 %>%
  rename(., teacherAddress = residentialStreet, 
         teacherCity = residentialCity, 
         teacherState = residentialState, 
         teacherZip = residentialZip,
         countyName = CountyName, 
         District.Name = DistrictName) %>%
  mutate(., School.Name = "") %>%
  select(., all_of(common_fields)) %>%
  mutate(., compCohort = "2009-2010")

# 2010-11 to 2014-15 were a beautiful era of relative stability in the file layout
prep_1011_to_1415_data <- function(full_data, cohort_year){
  output <- full_data %>%
    select(., -countyID) %>%
    rename(., countyName = CountyName, 
           countyID = County.Code) %>%
    select(., all_of(common_fields)) %>%
    mutate(., compCohort = cohort_year)
}

comp_1011 <- prep_1011_to_1415_data(full_comp_1011, "2010-2011") 
comp_1112 <- prep_1011_to_1415_data(full_comp_1112, "2011-2012") 
comp_1213 <- prep_1011_to_1415_data(full_comp_1213, "2012-2013") 
comp_1314 <- prep_1011_to_1415_data(full_comp_1314, "2013-2014") 
comp_1415 <- prep_1011_to_1415_data(full_comp_1415, "2014-2015") 
  
comp_1516 <- full_comp_1516 %>%
  select(., -countyID) %>%
  rename(., countyID = County.Code) %>%
  select(., all_of(common_fields)) %>%
  mutate(., compCohort = "2015-2016")
  
# 2016-17 to 2019-20 are also stable
prep_1617_to_present_data <- function(full_data, cohort_year){
  output <- full_data %>%
    select(., all_of(common_fields)) %>%
    mutate(., compCohort = cohort_year)
}

comp_1617 <- prep_1617_to_present_data(full_comp_1617, "2016-2017") 
comp_1718 <- prep_1617_to_present_data(full_comp_1718, "2017-2018") 
comp_1819 <- prep_1617_to_present_data(full_comp_1819, "2018-2019") 
comp_1920 <- prep_1617_to_present_data(full_comp_1920, "2019-2020") 

comp <- rbind(comp_0607, comp_0708, comp_0809, comp_0910, comp_1011, comp_1112, comp_1213, comp_1314, 
              comp_1415, comp_1516, comp_1617, comp_1718, comp_1819, comp_1920) %>%
  filter(., campusType == "CSU")

comp$newDOB[comp$newDOB == ""] <- NA
comp$newDOB <- as.Date(comp$newDOB, format = "%m/%d/%Y")
comp %<>% rename(., DOB = newDOB)

rm(full_comp_0607, full_comp_0708, full_comp_0809, full_comp_0910, full_comp_1011, full_comp_1112, full_comp_1213, full_comp_1314,
   full_comp_1415, full_comp_1516, full_comp_1617, full_comp_1718, full_comp_1819, full_comp_1920)
rm(prep_1011_to_1415_data, prep_1617_to_present_data)

####################################################################################
# Do some light cleaning based on prior QA
####################################################################################

comp$ssn %<>% gsub("-", "", .) %>%
  str_pad(., 9, pad=0)

comp$lName %<>% gsub('\"', "", ., fixed = TRUE) %>%
  gsub("^ ", "", .)
comp$fName %<>% gsub('\"', "", ., fixed = TRUE) %>%
  gsub("^ ", "", .)

comp$credentialCode %<>% toupper(.)
comp$credentialCode[comp$credentialDescription == "Single Subject"] <- "S" # One row miscoded as 'X'

# Fill in blanks
comp$credentialDescription[comp$credentialDescription == "" & comp$credentialCode == "E"] <- "Education Specialist"
comp$credentialDescription[comp$credentialDescription == "" & comp$credentialCode == "M"] <- "Multiple Subject"
comp$credentialDescription[comp$credentialDescription == "" & comp$credentialCode == "S"] <- "Single Subject"
comp$credentialDescription[comp$credentialDescription == "Multiple Subject "] <- "Multiple Subject"

# Get rid of whitespace, standardize case, recode out of range values
comp$credentialIntReg %<>% toupper(.) %>%
  gsub(" ", "", .) %>%
  recode(., "T" = "R")

comp$credentialExamProg %<>% toupper(.) %>%
  gsub(" ", "", .) %>%
  recode(., "?" = "", "E" = "X", "C" = "X", "S" = "", "Z" = "", "A" = "")


comp$contentCode %<>% toupper(.) %>%
  gsub(" ", "", .) %>%
  recode(., "ANGL" = "ENGL", 
         "BUS" = "BUSI", 
         "ESCE" = "ECSE", 
         "ECSC" = "ECSE", 
         "ENG" = "ENGL", 
         "ENGLISH" = "ENGL", 
         "FS" = "SIF", 
         "MUS" = "MUSI", 
         "MUSIC" = "MUSI", 
         "PHYSICS" = "PHS", 
         "SCIENCE" = "SIF", 
         "SIP" = "SIF", 
         "SP" = "FLS", 
         "SPAN" = "FLS")

comp$contentDescription %<>%
  recode(., "Foundation-level General Science" = "Foundational-Level General Science",
         "Foundational Level General Science" = "Foundational-Level General Science",
         "General SubjecGeneral Subjectsts" = "General Subjects",
         "Geosciences" = "Geoscience")

comp$surveyYear[comp$surveyYear == "16"] <- 2016

comp$programType %<>% toupper(.) %>%
  gsub(" ", "", .) %>%
  recode(., "E" = "", "M" = "", "S" = "")

comp %<>% filter(., is.na(School.Name))

####################################################################################
# Write output
####################################################################################

write.csv(comp, paste0("Multi_year_completer_employment_list_", Sys.Date(), ".csv"), row.names = FALSE)

####################################################################################
# Write tpdm source files for completer employment
####################################################################################

comp$School.Name %<>% gsub("/", "-", .)

comp <- comp %>%
  filter(., as.numeric(ssn)!=0 & !is.na(ssn) & ssn != "") %>%
  mutate(., `System NumbeCode` = ifelse(campusType == "CSU", 1, 2)) %>%
  select(., -campusType) %>%
  rename(., `Study Code` = studyCode, 
         SSN = ssn, 
         `Last Name` = lName, 
         `First Name` = fName,
         `Credential Code` = credentialCode, 
         `Credential Description` = credentialDescription,
         `Credential Int Reg` = credentialIntReg, 
         `Credential Exam Prog` = credentialExamProg,
         `Content Code` = contentCode, 
         `Content Description`= contentDescription, 
         Campus = campusAbb,
         `Campus ID` = campusID, 
         `CSU Cohort` = surveyYear, 
         `Program Type` = programType,
         Address = teacherAddress, 
         City = teacherCity, State = teacherState, 
         Zip = teacherZip,
         `County ID` = countyID, 
         `District ID` = districtID, 
         `School ID` = schoolID,
         `District Name` = District.Name, 
         `School Name` = School.Name)

write.csv(completer_employment_1920, paste0("tpdm completer employment source data/EDQ_COMP_EMP_1920.csv"), row.names = FALSE, quote=FALSE)

####################################################################################
# Clean up
####################################################################################

rm(campus_code_xwalk)
rm(common_fields)
rm(comp_0607, comp_0708, comp_0809, comp_0910, comp_1011, comp_1112, comp_1213, comp_1314, 
   comp_1415, comp_1516, comp_1617, comp_1718, comp_1819, comp_1920)
rm(comp, date_string)
rm(completer_employment_1920)