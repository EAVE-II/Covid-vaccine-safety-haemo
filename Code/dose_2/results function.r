

res.fun1 <- function(data){

z.agg <- data %>% group_by(expgr) %>% 
  dplyr::summarise(event=sum(event), personday=sum(interval))
z.agg$rate_week<- z.agg$event/z.agg$personday*7
names(z.agg)[names(z.agg)=="expgr"] <- "level"
z.agg <- cbind.data.frame(event_type=event.type.s,z.agg)


z.agg2 <- data %>% group_by(period) %>% 
  dplyr::summarise(event=sum(event), personday=sum(interval))
z.agg2$rate_week<- z.agg2$event/z.agg2$personday*7
names(z.agg2)[names(z.agg2)=="period"] <- "level"
z.agg2 <- cbind.data.frame( event_type=event.type.s,z.agg2)

z.agg3 <- rbind(z.agg,z.agg2)

  
  
  z.mod <- clogit(event ~ expgr + period + strata(EAVE_LINKNO) + offset(log(interval)), data=data, method="efron")
s <- summary(z.mod)
#s$coefficients
#s$conf.int
n1 <- length(levels(z.agg$level))
n2 <- length(levels(z.agg2$level))
z.r <- cbind.data.frame( event_type=event.type.s,level=c(paste("exposure",levels(z.agg$level)),
                                                                       paste("period",levels(z.agg2$level))),
                        coef=c(0,s$coefficients[1:(n1-1),"coef"],0,s$coefficients[(n1):(n1+n2-2),"coef"]), 
                        sd=c("-",s$coefficients[1:(n1-1),"se(coef)"],"-",s$coefficients[(n1):(n1+n2-2),"se(coef)"]),
                        p=c("-",s$coefficients[1:(n1-1),"Pr(>|z|)"],"-",s$coefficients[(n1):(n1+n2-2),"Pr(>|z|)"]))

z.r <- cbind(z.r[,1:2],z.agg3[,3:5],z.r[,3:5])

return(z.r)
}



res.fun1a <- function(data){
  df_sccs <- data
  
  data <- subset(df_sccs,vacc_type=="PB")
  z.agg <- data %>% group_by(expgr) %>% 
    dplyr::summarise(event=sum(event), personday=sum(interval))
  z.agg$rate_week<- z.agg$event/z.agg$personday*7
  names(z.agg)[names(z.agg)=="expgr"] <- "level"
  z.agg <- cbind.data.frame(event_type=event.type.s,z.agg)
  
  data <- subset(df_sccs,vacc_type=="AZ")
  z.agg3 <- data %>% group_by(expgr2) %>% 
    dplyr::summarise(event=sum(event), personday=sum(interval))
  z.agg3$rate_week<- z.agg3$event/z.agg3$personday*7
  names(z.agg3)[names(z.agg3)=="expgr2"] <- "level"
  z.agg3 <- cbind.data.frame(event_type=event.type.s,z.agg3)  
  
  data <- subset(df_sccs,!is.na(date_ecoss_specimen))
  z.agg5 <- data %>% group_by(expgr3) %>% 
    dplyr::summarise(event=sum(event), personday=sum(interval))
  z.agg5$rate_week<- z.agg5$event/z.agg5$personday*7
  names(z.agg5)[names(z.agg5)=="expgr3"] <- "level"
  z.agg5 <- cbind.data.frame(event_type=event.type.s,z.agg5) 
  
  data <- df_sccs
  z.agg2 <- data %>% group_by(period) %>% 
    dplyr::summarise(event=sum(event), personday=sum(interval))
  z.agg2$rate_week<- z.agg2$event/z.agg2$personday*7
  names(z.agg2)[names(z.agg2)=="period"] <- "level"
  z.agg2 <- cbind.data.frame( event_type=event.type.s,z.agg2)
  
  z.agg4 <- rbind(z.agg,z.agg3,z.agg5, z.agg2)
  
  
  
  z.mod <- clogit(event ~ expgr + expgr2 + expgr3 + period + strata(EAVE_LINKNO) + offset(log(interval)), data=data, method="efron")
  s <- summary(z.mod)
  #s$coefficients
  #s$conf.int
  n1 <- length(levels(z.agg$level))
  n3 <- length(levels(z.agg3$level))
  n5 <- length(levels(z.agg5$level))
  n2 <- length(levels(z.agg2$level))
  z.r <- cbind.data.frame( event_type=event.type.s,level=c(paste("exposure",levels(z.agg$level)),
                                                           paste("exposure",levels(z.agg3$level)),
                                                           paste("infection",levels(z.agg5$level)),
                                                           paste("period",levels(z.agg2$level))),
                           coef=c(0,s$coefficients[1:(n1-1),"coef"],0,s$coefficients[(n1):(n1+n3-2),"coef"],
                                  0,s$coefficients[(n1+n3-1):(n1+n3+n5-3),"coef"],
                                  0,s$coefficients[(n1+n3+n5-2):(n1+n3+n5+n2-4),"coef"]), 
                           sd=c("-",s$coefficients[1:(n1-1),"se(coef)"],
                                "-",s$coefficients[(n1):(n1+n3-2),"se(coef)"], 
                                "-",s$coefficients[(n1+n3-1):(n1+n3+n5-3),"se(coef)"], 
                                "-",s$coefficients[(n1+n3+n5-2):(n1+n3+n5+n2-4),"se(coef)"]), 
                           p=c("-",s$coefficients[1:(n1-1),"Pr(>|z|)"],
                               "-",s$coefficients[(n1):(n1+n3-2),"Pr(>|z|)"],
                               "-",s$coefficients[(n1+n3-1):(n1+n3+n5-3),"Pr(>|z|)"],
                               "-",s$coefficients[(n1+n3+n5-2):(n1+n3+n5+n2-4),"Pr(>|z|)"])) 
  
  z.r <- cbind(z.r[,1:2],z.agg4[,3:5],z.r[,3:5])
  
  return(z.r)
}




#z is the output from SCCS.r loop eg. z.r_1day
res.fun2 <- function(z,conf.level){
  z$rate_ratio <- round(exp(z$coef),2)
  z$CIlower <- round(exp(z$coef-qnorm(1-(1-conf.level)/2,0,1)*as.numeric(as.character(z$sd))),2)
  z$CIupper <- round(exp(z$coef+qnorm(1-(1-conf.level)/2,0,1)*as.numeric(as.character(z$sd))),2)
  z$CIlower[is.na(z$CIlower)] <- ""
  z$CIupper[is.na(z$CIupper)] <- ""
  z$rrCI <- paste(z$rate_ratio, " (",z$CIlower,", ",z$CIupper,")",sep="")
  z <- z[,c( "event_type", "level", "event", "personday",  "rate_week", "rate_ratio", "CIlower", "CIupper", "rrCI","p")]
  return(z)
}
