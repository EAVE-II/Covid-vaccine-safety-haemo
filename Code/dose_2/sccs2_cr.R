library(survival)
source("results function.r")
z.rp0.end <- 15
z.rp1.begin <- 0 
z.rp1.end <- 0
z.rp2.begin <- 1 # 
z.rp2.end <- 6
z.rp3.begin <- 7 # 
z.rp3.end <- 13
z.rp4.begin <- 14 # 
z.rp4.end <- 20
z.rp5.begin <- 21 # 
z.rp5.end <- 27
length_period <- 7
conflevel <- 0.95
group <- FALSE

#event.type.s <- "CVT_SVT"
#event.type.s <- "any_pericarditis"
#event.type.s <- "myocarditis"
#k=1
#consider change period for myocarditis but might not work as well.. too little event

#i=12


for (i in 1:length(endpoint_names)) {
  event.type.s  <- endpoint_names[i]
  print(event.type.s)
z_event <- readRDS(paste(event.type.s,".df.rds",sep=""))
z_event$event.date <- z_event$StartDate
z_event <- z_event %>% filter(!is.na(date_vacc_2))  # remove those who have had only 1 vaccine dose

  #for each individual - have a dataframe of followup time in day - and then add in markers for period and exposure group for each day
  
  #develop a per-person data including demog and vaccine infromation z1
  #then full join with the date dataset date.df - so each individual has a date.df
  #then merge with the event dataset z.e by ID and event date
  
  
  #develop a per-person data including demog and vaccine infromation z1 
  
  z1 <- z_event[,c("EAVE_LINKNO","vacc_type","date_vacc_1","date_vacc_2","date_ecoss_specimen", "NRS.Date.Death")]

  #event dataset z.e
  z.e <- z_event[,c("EAVE_LINKNO","event.date")]
  z.e$event = 1
  dim(z.e)#71
  
  #date dataset date.df
  date.df <- data.frame(date =seq(a.start,a.end,by=1))#
  
  #set up period varaible - depending on the length_period set in the top of the file
  n_p <- ceiling(nrow(date.df)/length_period)
  period <- rep(1:(n_p-1),each=length_period)
  period <- c(period,rep(n_p, nrow(date.df)-length(period)))
  date.df <- cbind(date.df, period)
  
  
  #full join per-person data z1 with the date dataset date.df - so each individual has a date.df
  
  #set up exposure variable - baseline, pre-risk, risk periods
  
  
  z <- full_join(z1,date.df, by=character())  
  z$days <- as.numeric(z$date-z$date_vacc_1)
  z$days2 <- as.numeric(z$date-z$date_vacc_2)
  z$expgr <- 0# baseline period
  z$expgr <- ifelse(z$days>=-z.rp0.end+1 & z$days<=z.rp1.begin-1 & z$vacc_type=="PB",1,z$expgr)
  z$expgr <- ifelse(z$days>=z.rp1.begin & z$days<=z.rp1.end & z$vacc_type=="PB",2,z$expgr)
  z$expgr <- ifelse(z$days>=z.rp2.begin & z$days<=z.rp2.end & z$vacc_type=="PB",3,z$expgr)#
  z$expgr <- ifelse(z$days>=z.rp3.begin & z$days<=z.rp3.end & z$vacc_type=="PB",4,z$expgr)#
  z$expgr <- ifelse(z$days>=z.rp4.begin & z$days<=z.rp4.end & z$vacc_type=="PB",5,z$expgr)#
  z$expgr <- ifelse(z$days>=z.rp5.begin & z$days<=z.rp5.end & z$vacc_type=="PB",6,z$expgr)#
  z$expgr <- ifelse(z$days2>=-z.rp0.end+1 & z$days2<=z.rp1.begin-1 &!is.na(z$days2) & z$days>z.rp5.end & z$vacc_type=="PB",7,z$expgr)#pre-risk period dose 2 
  z$expgr <- ifelse(z$days2>=z.rp1.begin & z$days2<=z.rp1.end &!is.na(z$days2) & z$vacc_type=="PB",8,z$expgr)
  z$expgr <- ifelse(z$days2>=z.rp2.begin & z$days2<=z.rp2.end &!is.na(z$days2) & z$vacc_type=="PB",9,z$expgr)
  z$expgr <- ifelse(z$days2>=z.rp3.begin & z$days2<=z.rp3.end &!is.na(z$days2) & z$vacc_type=="PB",10,z$expgr)
  z$expgr <- ifelse(z$days2>=z.rp4.begin & z$days2<=z.rp4.end &!is.na(z$days2) & z$vacc_type=="PB",11,z$expgr)
  z$expgr <- ifelse(z$days2>=z.rp5.begin & z$days2<=z.rp5.end &!is.na(z$days2) & z$vacc_type=="PB",12,z$expgr)
  #View(subset(z, as.numeric(date_vacc_2 - date_vacc_1)<28)) #check
  
  
  table(z$expgr,exclude=NULL)
  z$expgr <- factor(z$expgr, levels=0:12, labels=c("PB Baseline","PB Dose1 clearance","PB Dose1 0-6","PB Dose1 0-6",
                                                   "PB Dose1 7-13" ,"PB Dose1 14-20","PB Dose1 21-27","PB Dose2 clearance","PB Dose2 0-6",
                                                   "PB Dose2 0-6","PB Dose2 7-13","PB Dose2 14-20","PB Dose2 21-27")
  ) 

  
  
  z$expgr2 <- 0# baseline period
  z$expgr2 <- ifelse(z$days>=-z.rp0.end+1 & z$days<=z.rp1.begin-1 & z$vacc_type=="AZ",1,z$expgr2)
  z$expgr2 <- ifelse(z$days>=z.rp1.begin & z$days<=z.rp1.end & z$vacc_type=="AZ",2,z$expgr2)
  z$expgr2 <- ifelse(z$days>=z.rp2.begin & z$days<=z.rp2.end & z$vacc_type=="AZ",3,z$expgr2)#
  z$expgr2 <- ifelse(z$days>=z.rp3.begin & z$days<=z.rp3.end & z$vacc_type=="AZ",4,z$expgr2)#
  z$expgr2 <- ifelse(z$days>=z.rp4.begin & z$days<=z.rp4.end & z$vacc_type=="AZ",5,z$expgr2)#
  z$expgr2 <- ifelse(z$days>=z.rp5.begin & z$days<=z.rp5.end & z$vacc_type=="AZ",6,z$expgr2)#
  z$expgr2 <- ifelse(z$days2>=-z.rp0.end+1 & z$days2<=z.rp1.begin-1 &!is.na(z$days2) & z$days>z.rp5.end & z$vacc_type=="AZ",7,z$expgr2)#pre-risk period dose 2 
  z$expgr2 <- ifelse(z$days2>=z.rp1.begin & z$days2<=z.rp1.end &!is.na(z$days2) & z$vacc_type=="AZ",8,z$expgr2)
  z$expgr2 <- ifelse(z$days2>=z.rp2.begin & z$days2<=z.rp2.end &!is.na(z$days2) & z$vacc_type=="AZ",9,z$expgr2)
  z$expgr2 <- ifelse(z$days2>=z.rp3.begin & z$days2<=z.rp3.end &!is.na(z$days2) & z$vacc_type=="AZ",10,z$expgr2)
  z$expgr2 <- ifelse(z$days2>=z.rp4.begin & z$days2<=z.rp4.end &!is.na(z$days2) & z$vacc_type=="AZ",11,z$expgr2)
  z$expgr2 <- ifelse(z$days2>=z.rp5.begin & z$days2<=z.rp5.end &!is.na(z$days2) & z$vacc_type=="AZ",12,z$expgr2)
  #View(subset(z, as.numeric(date_vacc_2 - date_vacc_1)<28)) #check
  
  
  table(z$expgr2,exclude=NULL)
  z$expgr2 <- factor(z$expgr2, levels=0:12, labels=c("AZ Baseline","AZ Dose1 clearance","AZ Dose1 0-6","AZ Dose1 0-6",
                                                   "AZ Dose1 7-13" ,"AZ Dose1 14-20","AZ Dose1 21-27","AZ Dose2 clearance","AZ Dose2 0-6",
                                                   "AZ Dose2 0-6","AZ Dose2 7-13","AZ Dose2 14-20","AZ Dose2 21-27")
  ) 
  
  z$days3 <- as.numeric(z$date-z$date_ecoss_specimen)
  
  z$expgr3 <- 0# baseline period
  z$expgr3 <- ifelse(!is.na(z$days3) & z$days3>=-z.rp0.end+1 & z$days3<=z.rp1.begin-1,1,z$expgr3)
  z$expgr3 <- ifelse(!is.na(z$days3) & z$days3>=z.rp1.begin & z$days3<=z.rp1.end ,2,z$expgr3)
  z$expgr3 <- ifelse(!is.na(z$days3) & z$days3>=z.rp2.begin & z$days3<=z.rp2.end,3,z$expgr3)#
  z$expgr3 <- ifelse(!is.na(z$days3) & z$days3>=z.rp3.begin & z$days3<=z.rp3.end,4,z$expgr3)#
  z$expgr3 <- ifelse(!is.na(z$days3) & z$days3>=z.rp4.begin & z$days3<=z.rp4.end,5,z$expgr3)#
  z$expgr3 <- ifelse(!is.na(z$days3) & z$days3>=z.rp5.begin & z$days3<=z.rp5.end,6,z$expgr3)#
  table(z$expgr3,exclude=NULL)
  z$expgr3 <- factor(z$expgr3, levels=0:6, labels=c("infection Baseline","infection clearance","infection 0-6","infection 0-6",
                                                    "infection 7-13" ,"infection1 14-20","infection 21-27")
  )
  #remove follow up post date of death
  
  z <- subset(z, date<=NRS.Date.Death | is.na(NRS.Date.Death))
  
  #merge event
  z <-left_join(z, z.e, by = c("date" = "event.date", "EAVE_LINKNO"="EAVE_LINKNO"))
  
  z[is.na(z$event), "event"] <- 0
  #table(z$event)#
  
  
  z$period <- as.factor(z$period)
  
  # levels(z$period)[1:2] <- "1-2"# combine first 2 months for period otherwise priod effect not estimated properly for AZ due to low number in first month
  
  z$interval <- 1#each line is for one day
  #z.agg <- 
  data <- z
  df_sccs <- z
  
  if(group==TRUE){
    levels(z$expgr)[3:6] <- "PB Dose1 0-27"
    levels(z$expgr)[5:8] <- "PB Dose2 0-27"
    
    levels(z$expgr2)[3:6] <- "AZ Dose1 0-27"
    levels(z$expgr2)[5:8] <- "AZ Dose2 0-27"
    
    levels(z$expgr3)[3:6] <- "infection 0-27"

  }
  
  data <- z

  #res.fun1 and res.fun2 defined in the "results function.r" to for the output format
  z.r <- res.fun1a(data)
  z.rr <- res.fun2(z.r,conflevel)#
  z.rrr <- z.rr
  
 #write.csv(z.rrr, paste(event.type.s,"sccs.csv", sep="."))
  
  write.csv(z.rrr, paste(event.type.s,"sccs.group.csv", sep="."))
 # if(k==1) z.r4 <- z.rrr else
#    z.r4 <- rbind(z.r4,z.rrr)
#  k=k+1
#  print(k)
  
}



for (i in 1:length(endpoint_names)) {
  event.type.s <-  endpoint_names[i]
  z.rrr <- read.csv(paste(event.type.s,"sccs.group.csv", sep="."))
  if(i==1) z.r4 <- z.rrr else
       z.r4 <- rbind(z.r4,z.rrr)
  
}

if (!group) write.csv(z.r4, "sccs.fullv3.csv")

if (group) write.csv(z.r4, "sccs.full.groupv3.csv")
