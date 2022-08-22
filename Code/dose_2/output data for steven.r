
z_event_endpoint.list <- c("itp", "CVT_SVT","itp_gen", "myocarditis")

for(i in 1:4){
  #i <- 2
  z_event_endpoint <- z_event_endpoint.list[i]
z <- readRDS(paste0(z_event_endpoint,".df_sccs.rds"))
z<- subset(z, event==1)
print(z_event_endpoint)
print(nrow(z))
print(summary(z$date))

z$expgr_combine <- ifelse(z$expgr2!="AZ Baseline", as.character(z$expgr2),as.character(z$expgr))
z$expgr_combine <- ifelse(z$expgr_combine=="PB Baseline", "Baseline", z$expgr_combine)
z$event_type <- z_event_endpoint

if(i==1)
  z.r <- z else
    z.r <- rbind(z.r,z)
}
saveRDS(z.r, "data_for_steven.rds")
