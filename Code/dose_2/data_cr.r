
# 01 Setup ####
#Libraries
library(plyr)
library(tidyverse)
library(survival)
library(lubridate)
#Load data

Location <- "/conf/"  # Server
#Location <- "//isdsf00d03/"  # Desktop


project_path <- paste0(Location,"EAVE/GPanalysis/progs/JP/vaccine safety AZ")

setwd(project_path)

source(paste0(Location,"EAVE/GPanalysis/progs/CR/00_Read_GP_Vaccinations.R"))

#omit moderna
Vaccinations <- filter(Vaccinations, vacc_type != "Mo")

#saveRDS(Vaccinations, "Vaccinations_20211117.rds")






#get all positive tests 
cdw_full  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/CDW_full.rds"))
cdw_full <- cdw_full %>% mutate(date_ecoss_specimen = as_date(date_ecoss_specimen))
z <- cdw_full %>% filter(test_result=="POSITIVE") %>% 
  dplyr::select(EAVE_LINKNO, date_ecoss_specimen) %>% 
  arrange(EAVE_LINKNO, date_ecoss_specimen) %>%
  filter(!duplicated(paste(EAVE_LINKNO, date_ecoss_specimen)))  #get one positive test per person per day
Positive_Tests <- z #can have duplicate values here
#saveRDS(Positive_Tests, "Positive_Tests_20211117.rds")

death.df  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/all_deaths.rds"))
all_deaths <- death.df %>% dplyr::select(EAVE_LINKNO, NRS.Date.Death)
#saveRDS(all_deaths, "all_deaths_20211117.rds")

#get age gender for vaccinated cohort
#EAVE_cohort <- readRDS(paste0(Location,"EAVE/GPanalysis/outputs/temp/Cohort_Demog_Endpoints_Times2021-07-28.rds"))
#table(Vaccinations$EAVE_LINKNO %in% EAVE_cohort$EAVE_LINKNO )
#z <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))%>% dplyr::select(EAVE_LINKNO,  Sex) 

