
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

a.start <- as.Date("2020-12-01")

#  need to change for rerun with updated data
a.end <-  as.Date("2021-11-07")
a.prior <- as.Date("2019-09-01")



df  <- readRDS(paste0(Location,"EAVE/GPanalysis/data/cleaned_data/AZ_safety_2021-11-15.rds"))

df <- df %>% filter(df$StartDate <= max(df$ExtractDate,na.rm=T))
df <- df %>% mutate(YEAR = lubridate::year(StartDate))
df <- df %>% mutate(month = lubridate::month(StartDate, label =T) )

table(df$YEAR, df$month)

table(df$Quick_groups,exclude=NULL)
df <- df %>% mutate(Age = as.numeric(Age))

df <- df %>% mutate(AgeGrp = case_when(Age <15 ~ "0-15", 
                                       Age >= 15 & Age < 20 ~ "15-19",
                                       Age >= 20 & Age < 40 ~ "20-39",
                                       Age >= 40 & Age < 60 ~ "40-59",
                                       Age >= 60 & Age < 80 ~ "60-79",
                                       Age >= 80  ~ "80+"))
table(df$AgeGrp, df$Quick_groups, df$YEAR)

#StartDate is the event date

#df <- filter(df, ExtractDate <= as.Date("2021-03-29") | Quick_groups %in% c("CVT","SVT"))

GP_Consultations <- df 


endpoint_groups <- list()
endpoint_groups$any_throm <- c("dv_throm","pulm_emb","throm", "pv_throm")
endpoint_groups$throm_cvst <- c("dv_throm","pulm_emb","throm", "pv_throm", "CVT","SVT")
endpoint_groups$any_haem <- c("haem","cereb_haem","sub_arach_haem", "th_haem" )
endpoint_groups$cerebral_haem <- c("cereb_haem","sub_arach_haem" )
endpoint_groups$non_cereb_haem <- c("haem", "th_haem" )
endpoint_groups$any_throm_haem <- unique(c(endpoint_groups$throm_cvst, endpoint_groups$any_haem))
#endpoint_groups$all_haem <- c("haem","cereb_haem","sub_arach_haem", "haem_gigu")
endpoint_groups$CVT_SVT <- c("CVT","SVT")
endpoint_groups$any_itp <- c("itp", "itp_gen")
endpoint_groups$any_pericarditis <- c("pericarditis", "pericarditis_extra")


endpoint_names <- c("throm_cvst", "any_throm","dv_throm", "pulm_emb","Arterial_thromb",
                    "any_haem","cerebral_haem", "non_cereb_haem",
                    "any_itp", "itp_gen","itp","CVT_SVT", "any_pericarditis","myocarditis")





Positive_Tests<-readRDS("Positive_Tests_2021117.rds")
all_deaths <- readRDS("all_deaths_2021117.rds")
Vaccinations <- readRDS("Vaccinations_202117.rds")




for (i in 1:length(endpoint_names)) {
  
  event.type.s <-  endpoint_names[i]
  # z_event_endpoint <- "Arterial_thromb"
  print( event.type.s)
  

  #z_event_endpoint <- "itp" # set in 03a_CC_Control.R
  #output_list$endpoint <- z_event_endpoint
  #print(z_event_endpoint)
  if (event.type.s %in% names(endpoint_groups)) {
    #combination of groups
    z_values <- as.character(unlist(endpoint_groups[event.type.s]))
    z_event_all <- filter(GP_Consultations , Quick_groups %in% z_values)
  }  else {
    #single group
    z_event_all <- filter(GP_Consultations , Quick_groups == event.type.s)
  } 
  
  z.event <- z_event_all %>% 
    subset( StartDate >= a.start & StartDate <= a.end) %>%
    arrange(EAVE_LINKNO, StartDate) %>%
    subset(!duplicated(EAVE_LINKNO))
  
  #remove patients who have admissions 2 year before 2020-12-01
  
  z.event.prior <-  z_event_all %>% subset(StartDate < a.start &StartDate >= a.prior)
  id <- unique(z.event.prior$EAVE_LINKNO)
  length(id)#
  z.event <- subset(z.event, EAVE_LINKNO %in% id ==FALSE)
  dim(z.event)#48  

#event.type.s <- "myocarditis"

z.vacc <- filter(Vaccinations, date_vacc_1 <=a.end) #






#merge with vaccination

df <- merge(z.event, z.vacc)

#remove <16s

df <- subset(df, Age >=16)

#merge with death data

df <- left_join(df, all_deaths)

#merge for gender
#z <- filter(EAVE_cohort, !duplicated(EAVE_LINKNO))%>% dplyr::select(EAVE_LINKNO,  Sex) 
#df <- df <- left_join(df, z)

#merge with test data
z <- Positive_Tests %>%subset( date_ecoss_specimen>=a.start & date_ecoss_specimen<=a.end)%>%
  arrange(EAVE_LINKNO, date_ecoss_specimen) %>%
  subset(!duplicated(EAVE_LINKNO))
df <- left_join(df, z)

#covid before vaccine
#z <- Positive_Tests %>% merge(z.vacc) %>%
#  subset(date_ecoss_specimen < date_vacc_1)
#df$prior.covid <- ifelse(df$EAVE_LINKNO %in% z$EAVE_LINKNO,1,0)
#table(df$prior.covid )


saveRDS(df, paste(event.type.s,".df.rds",sep=""))

}
