### plots for checking cluster model running ok  ###


# plots<-"/Users/Rebecca/Vaccine_china/Plots"
# setwd(plots)
clusteroutput<-"/Users/Rebecca/Vaccine_china/Outputs/Cluster_outputs/151013"
setwd(clusteroutput)

#data to fit to
population2010<- c(1359822,246707,863710,135859,113546)
population2050<- c(1384976,204187,623982,225492,331315)
population2025<-195605                                           #just elderly
population2050u<-c(3867612,300552,722200,248041,364447)
population2050l<-c(1153148,124968,527054,202943,298184)

incidence2010<- c(74.31,3.16,75.14,121.35,166.35)
incidence2010u<- c(84.09,3.57,85.02,137.32,188.24)
incidence2010l<- c(65.21,2.77,65.94,106.49,145.98)

#noitfication lower limit is the reported notifications, notification upper limit assumes that all from private hospitals are missed from notification reporting (80% of cases reported to CDC, 20% private hospital)  
notif2010 <- c(63.91,2.72,64.62,104.36,143.07)
notif2010u<- c(63.91,2.72,64.62,104.36,143.07)*(100/80)
notif2010l<- c(63.91,2.72,64.62,104.36,143.07)

prevalence2000<- c(178,92,119,213,596)
prevalence2000u<- c(163,116,146,260,698)
prevalence2000l<- c(195,72,96,174,510)

prevalence2010<- c(116,59,73,133,346)
prevalence2010u<- c(101,86,99,168,407)
prevalence2010l<- c(132,40,54,106,294)

mortality2010<- c(4.69,0.29,1.91,15.69)
mortality2010u<- c(4.84,0.32,2.10,17.26)
mortality2010u<- mortality2010 + ((mortality2010u-mortality2010)*10)
mortality2010l<- c(4.54,0.27,1.72,14.12)

max<-which.max(L_1m[,3])
max<-89
#insert number of max L run here
xoutmax<-as.data.frame(fread(paste("xout_","1379",".csv",sep='')),check.names=TRUE)
xoutmax<-xoutmax[xoutmax$fit==max,]
xoutmax<-xoutplot[xoutplot$run_count==max,]

## notifications plot

par(mfrow=c(2,3))
plot(seq(2000,2010),xoutmax$TBNtot[1:11],ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='orange')
points(2010,notif2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
plot(seq(2000,2010),xoutmax$"TBN0-14"[1:11], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='red')
points(2010,notif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
plot(seq(2000,2010),xoutmax$"TBN15-54"[1:11], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='blue')
points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
plot(seq(2000,2010),xoutmax$"TBN55-64"[1:11], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='purple')
points(2010,notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
plot(seq(2000,2010),xoutmax$"TBN65+"[1:11], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='green')
points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))


## prevalence plot

par(mfrow=c(2,3))
plot(seq(2000,2010),xoutmax$"TBPb15+"[1:11], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,200),type='l',col='orange')
points(2000,prevalence2000[1])
points(2010,prevalence2010[1])
segments(2000,prevalence2000l[1],2000,prevalence2000u[1])
segments(2010,prevalence2010l[1],2010,prevalence2010u[1])
plot(seq(2000,2010),xoutmax$"TBPb15-29"[1:11], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='lightblue')
points(2000,prevalence2000[2])
points(2010,prevalence2010[2])
segments(2000,prevalence2000l[2],2000,prevalence2000u[2])
segments(2010,prevalence2010l[2],2010,prevalence2010u[2])
plot(seq(2000,2010),xoutmax$"TBPb30-44"[1:11], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='blue')
points(2000,prevalence2000[3])
points(2010,prevalence2010[3])
segments(2000,prevalence2000l[3],2000,prevalence2000u[3])
segments(2010,prevalence2010l[3],2010,prevalence2010u[3])
plot(seq(2000,2010),xoutmax$"TBPb45-59"[1:11], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='purple')
points(2000,prevalence2000[4])
points(2010,prevalence2010[4])
segments(2000,prevalence2000l[4],2000,prevalence2000u[4])
segments(2010,prevalence2010l[4],2010,prevalence2010u[4])
plot(seq(2000,2010),xoutmax$"TBPb60+"[1:11], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='green')
points(2000,prevalence2000[5])
points(2010,prevalence2010[5])
segments(2000,prevalence2000l[5],2000,prevalence2000u[5])
segments(2010,prevalence2010l[5],2010,prevalence2010u[5])
plot.new()
legend("center",c("Overall(15+) years","15-29 years","30-44 years","45-59 years","≥60 years"), lty=1,col=c("orange","lightblue","blue","purple","green"))


# Mortality plot
par(mfrow=c(2,3))
plot(seq(2000,2010),xoutmax$TBMtot[1:11], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
plot(seq(2000,2010),xoutmax$"TBM0-14"[1:11], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,5),type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
plot.new()
plot(seq(2000,2010),xoutmax$"TBM15-54"[1:11], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
plot(seq(2000,2010),xoutmax$"TBM60+"[1:11], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
plot.new()
legend("top",c("Overall","0-14 years","15-59 years","≥60 years"), lty=1,col=c("orange","red","blue","green"))

## incidence plot
par(mfrow=c(1,1))
plot(seq(2000,2010),xoutmax$TBItot[1:11], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])

# 
# # # prevalence of infection plot
par(mfrow=c(2,3))
plot(seq(1990,2050),xoutmax$TBPItot[1:11], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='orange')
plot(seq(1990,2050),xoutmax$"TBPI0-14"[1:11], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='red')
plot(seq(1990,2050),xoutmax$"TBPI15-54"[1:11], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='blue')
plot(seq(1990,2050),xoutmax$"TBPI55-64"[1:11], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='purple')
plot(seq(1990,2050),xoutmax$"TBPI65+"[1:11], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='green')
# legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))



##reinfection/reactivation plot 
par(mfrow=c(2,3))
plot(seq(1990,2050),xoutmax$TBRatot[1:11], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='orange')
lines(seq(1990,2050),xoutmax$TBRitot[1:11],lty=5,col='orange')
plot(seq(1990,2050),xoutmax$"TBRa0-14"[1:11], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='red')
lines(seq(1990,2050),xoutmax$"TBRi0-14"[1:11],lty=5,col='red')
plot(seq(1990,2050),xoutmax$"TBRa15-54"[1:11], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='blue')
lines(seq(1990,2050),xoutmax$"TBRi15-54"[1:11],lty=5,col='blue')
plot(seq(1990,2050),xoutmax$"TBRa55-64"[1:11], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='purple')
lines(seq(1990,2050),xoutmax$"TBRi55-64"[1:11],lty=5,col='purple')
plot(seq(1990,2050),xoutmax$"TBRa65+"[1:11], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='green')
lines(seq(1990,2050),xoutmax$"TBRi65+"[1:11],lty=5,col='green')
plot.new()
legend("center",c("New Infection","Reactivation","Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=c(5,1,1,1,1,1,1),col=c("black","black","orange","red","blue","purple","green"))




#################################


## notifications plot

par(mfrow=c(2,3))
# plot(seq(1990,2050),xout$TBNtot[91:151], ylab="10xM Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='orange')
# lines(seq(1990,2050),xoutmax$TBNtot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='orange',lty=2)
plot(seq(1990,2050),xoutmax$TBNtot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],ylab="10xM Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='orange')
points(2010,notif2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
#plot(seq(1990,2050),xout$"TBN0-14"[91:151], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='red')
plot(seq(1990,2050),xoutmax$"TBN0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='red')
#lines(seq(1990,2050),xoutmax$"TBN0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='red',lty=2)
points(2010,notif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
#plot(seq(1990,2050),xout$"TBN15-54"[91:151], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='blue')
plot(seq(1990,2050),xoutmax$"TBN15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='blue')
#lines(seq(1990,2050),xoutmax$"TBN15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='blue',lty=2)
points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
#plot(seq(1990,2050),xout$"TBN55-64"[91:151], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='purple')
plot(seq(1990,2050),xoutmax$"TBN55-64"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='purple')
#lines(seq(1990,2050),xoutmax$"TBN55-64"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='purple',lty=2)
points(2010,notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
#plot(seq(1990,2050),xout$"TBN65+"[91:151], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='green')
plot(seq(1990,2050),xoutmax$"TBN65+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='green')
#lines(seq(1990,2050),xoutmax$"TBN65+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='green',lty=2)
points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))




## prevalence plot

par(mfrow=c(2,3))
# plot(seq(1990,2050),xout$TBPbtot[91:151], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='orange')
# lines(seq(1990,2050),xoutmax$TBPbtot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='orange',lty=2)
plot(seq(1990,2050),xoutmax$TBPbtot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,200),type='l',col='orange')
points(2000,prevalence2000[1])
points(2010,prevalence2010[1])
segments(2000,prevalence2000l[1],2000,prevalence2000u[1])
segments(2010,prevalence2010l[1],2010,prevalence2010u[1])
# plot(seq(1990,2050),xout$"TBPb15-29"[91:151], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='lightblue')
# lines(seq(1990,2050),xoutmax$"TBPb15-29"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='lightblue',lty=2)
plot(seq(1990,2050),xoutmax$"TBPb15-29"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='lightblue')
points(2000,prevalence2000[2])
points(2010,prevalence2010[2])
segments(2000,prevalence2000l[2],2000,prevalence2000u[2])
segments(2010,prevalence2010l[2],2010,prevalence2010u[2])
# plot(seq(1990,2050),xout$"TBPb30-44"[91:151], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='blue')
# lines(seq(1990,2050),xoutmax$"TBPb30-44"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='blue',lty=2)
plot(seq(1990,2050),xoutmax$"TBPb30-44"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='blue')
points(2000,prevalence2000[3])
points(2010,prevalence2010[3])
segments(2000,prevalence2000l[3],2000,prevalence2000u[3])
segments(2010,prevalence2010l[3],2010,prevalence2010u[3])
# plot(seq(1990,2050),xout$"TBPb45-59"[91:151], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='purple')
# lines(seq(1990,2050),xoutmax$"TBPb45-59"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='purple',lty=2)
plot(seq(1990,2050),xoutmax$"TBPb45-59"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='purple')
points(2000,prevalence2000[4])
points(2010,prevalence2010[4])
segments(2000,prevalence2000l[4],2000,prevalence2000u[4])
segments(2010,prevalence2010l[4],2010,prevalence2010u[4])
# plot(seq(1990,2050),xout$"TBPb60+"[91:151], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='green')
# lines(seq(1990,2050),xoutmax$"TBPb60+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='green',lty=2)
plot(seq(1990,2050),xoutmax$"TBPb60+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='green')
points(2000,prevalence2000[5])
points(2010,prevalence2010[5])
segments(2000,prevalence2000l[5],2000,prevalence2000u[5])
segments(2010,prevalence2010l[5],2010,prevalence2010u[5])
plot.new()
legend("center",c("Overall(15+) years","15-29 years","30-44 years","45-59 years","≥60 years"), lty=1,col=c("orange","lightblue","blue","purple","green"))


# Mortality plot
par(mfrow=c(2,3))
# plot(seq(1990,2050),xout$TBMtot[91:151], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='orange')
# lines(seq(1990,2050),xoutmax$TBMtot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='orange',lty=2)
plot(seq(1990,2050),xoutmax$TBMtot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
# plot(seq(1990,2050),xout$"TBM0-14"[91:151], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,5),type='l',col='red')
# lines(seq(1990,2050),xoutmax$"TBM0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='red',lty=2)
plot(seq(1990,2050),xoutmax$"TBM0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,5),type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
plot.new()
# plot(seq(1990,2050),xout$"TBM15-59"[91:151], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='blue')
# lines(seq(1990,2050),xoutmax$"TBM15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='blue',lty=2)
plot(seq(1990,2050),xoutmax$"TBM15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
# plot(seq(1990,2050),xout$"TBM60+"[91:151], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='green')
# lines(seq(1990,2050),xoutmax$"TBM60+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='green',lty=2)
plot(seq(1990,2050),xoutmax$"TBM60+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
plot.new()
legend("top",c("Overall","0-14 years","15-59 years","≥60 years"), lty=1,col=c("orange","red","blue","green"))


par(mfrow=c(1,1))
# plot(seq(1990,2050),xout$TBItot[91:151], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='orange')
# lines(seq(1990,2050),xoutmax$TBItot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],type='l', col='orange',lty=2)
plot(seq(1990,2050),xoutmax$TBItot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])

# 
# # # prevalence of infection plot
par(mfrow=c(2,3))
plot(seq(1990,2050),xoutmax$TBPItot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='orange')
plot(seq(1990,2050),xoutmax$"TBPI0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='red')
plot(seq(1990,2050),xoutmax$"TBPI15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='blue')
plot(seq(1990,2050),xoutmax$"TBPI55-64"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='purple')
plot(seq(1990,2050),xoutmax$"TBPI65+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='green')
# # plot(seq(1990,2050),TBPI[91:151,15], ylab="Prevalence of Infection (%) 6574",xlab="year", ylim=c(0,100),type='l',col='green')
# # plot(seq(1990,2050),TBPI[91:151,16], ylab="Prevalence of Infection (%) 75+",xlab="year", ylim=c(0,100),type='l',col='green')
# plot.new()
# legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))



##reinfection/reactivation plot 
par(mfrow=c(2,3))
plot(seq(1990,2050),xoutmax$TBRatot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='orange')
lines(seq(1990,2050),xoutmax$TBRitot[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],lty=5,col='orange')
#lines(seq(1970,2050),((TBRi[71:151,1])+TBRa[71:151,1]),lty=5,col='orange')
plot(seq(1990,2050),xoutmax$"TBRa0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='red')
lines(seq(1990,2050),xoutmax$"TBRi0-14"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],lty=5,col='red')
#lines(seq(1970,2050),((TBRi[71:151,2])+TBRa[71:151,2]),lty=5,col='red')
plot(seq(1990,2050),xoutmax$"TBRa15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='blue')
lines(seq(1990,2050),xoutmax$"TBRi15-54"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],lty=5,col='blue')
#lines(seq(1970,2050),((TBRi[71:151,3])+TBRa[71:151,3]),lty=5,col='blue')
plot(seq(1990,2050),xoutmax$"TBRa55-64"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='purple')
lines(seq(1990,2050),xoutmax$"TBRi55-64"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],lty=5,col='purple')
#lines(seq(1970,2050),((TBRi[71:151,4])+TBRa[71:151,4]),lty=5,col='green')
plot(seq(1990,2050),xoutmax$"TBRa65+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='green')
lines(seq(1990,2050),xoutmax$"TBRi65+"[(((yearend-year1+1)*(max-1)*1/dt)+91):(((yearend-year1+1)*(max-1)*1/dt)+151)],lty=5,col='green')
#lines(seq(1970,2050),((TBRi[71:151,5])+TBRa[71:151,5]),lty=5,col='pink')
plot.new()
legend("center",c("New Infection","Reactivation","Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=c(5,1,1,1,1,1,1),col=c("black","black","orange","red","blue","purple","green"))

