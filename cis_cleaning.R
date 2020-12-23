### Preamble ###
# Purpose: This code aims to clean-up the 2017 Canadian Income Survey data obtained 
# from the U of T library. It is because the data contains invaild responses and
# catergorical variables that increase the difficulities to analyze the data. Then,
# this code will clean up the dataset by removing invaild responses and replacing 
# the variable names and type. Finally, it will produce a dataset called cis_data.csv.
# Author: Shuk Yin Chung
# Data: 22 December 2020
# Contact: shukyin.chung@mail.utoronto.ca
# License: DLI
# Pre-requisites: 
# - Need to have downloaded the data from the library.

### Workspace setup ###
library(readr)
library(dplyr)
library(janitor)

setwd("C:/Users/Katy/Desktop/STA304 Final")
# Read in the raw data.
raw_data <- read_csv("AASgrJTB.csv")

# Just keep the variables that are being used in this project
cis_data <- raw_data %>% 
  select(CASEID,
         agegp,
         sex,
         hlev2g,
         ushrwk,
         wgsal)

# Fix the name
cis_data <- cis_data %>%
  rename() %>%
  select(CASEID = CASEID,
         age = agegp,
         sex = sex,
         education = hlev2g,
         weekly_working_hours = ushrwk,
         salary = wgsal)

#### Clean up ####

# Remove skipped responses from the weekly_working_hours variable
cis_data <- cis_data %>% 
  mutate_at(vars(weekly_working_hours), 
            .funs = funs(ifelse(.=="999.6", as.numeric(NA), .)))
cis_data <- na.omit(cis_data)

# Clean up the age group responses
cis_data <- cis_data %>% 
  mutate_at(vars(age), .funs = funs(case_when(
    .=="04"~"16-19",
    .=="05"~"16-19",
    .=="06"~"20-29",
    .=="07"~"20-29",
    .=="08"~"30-39",
    .=="09"~"30-39",
    .=="10"~"40-49",
    .=="11"~"40-49",
    .=="12"~"50-59",
    .=="13"~"50-59",
    .=="14"~"60-69",
    .=="15"~"60-69",
    .=="16"~"70+")))

# Clean up education variable
cis_data <- cis_data %>%
  mutate_at(vars(education), .funs = funs(case_when(
    .=="1"~"Less than high school graduation",
    .=="2"~"Graduated high school or partial postsecondary education",
    .=="3"~"Non-university postsecondary certificate or diploma",
    .=="4"~"University degree or certificate",
    .=="9"~"NA")))

# Remove skipped responses from education variable
cis_data <- cis_data %>% 
  mutate_at(vars(education), 
            .funs = funs(ifelse(.=="NA", as.numeric(NA), .))) %>%
  na.omit(cis_data)

# Change the responses of sex to "Male" and "Female"
cis_data <- cis_data %>% 
  mutate_at(vars(sex), 
            .funs = funs(ifelse(.== 1, "Male", "Female")))

# Saving the data as a csv file
write_csv(cis_data, "cis_data.csv")


## References
##1. "CHASS Microdata Analysis and Subsetting with SDA, Canadian Income Survey (CIS), 2017." Retrieved from https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/html/cis.htm
##2. "Canadian Income Survey, 2017: Public Use Microdata File Data dictionary" Statistics Canada. University of Toronto Data Library Service, 2001. Retrieved from https://sda-artsci-utoronto-ca.myaccess.library.utoronto.ca/sdaweb/dli2/cis/2017/more_doc/2017CIS_Codebook.pdf