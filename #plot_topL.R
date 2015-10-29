### Plotting the top likelihood runs ###


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


max<-which.max(L[,3])

top_L<-top_L

## notifications plot

par(mfrow=c(2,2))
plot(seq(fityrs),xoutplot$TBNtot[1:(length(fityrs))], ylab="TOPLI_Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='white')
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[1])-1)*(length(fityrs)))+1):((top_L[1])*(length(fityrs)))],type='l', col='blue',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[2])-1)*(length(fityrs)))+1):((top_L[2])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[3])-1)*(length(fityrs)))+1):((top_L[3])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[4])-1)*(length(fityrs)))+1):((top_L[4])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[5])-1)*(length(fityrs)))+1):((top_L[5])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[6])-1)*(length(fityrs)))+1):((top_L[6])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[7])-1)*(length(fityrs)))+1):((top_L[7])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[8])-1)*(length(fityrs)))+1):((top_L[8])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[9])-1)*(length(fityrs)))+1):((top_L[9])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBNtot[((((top_L[10])-1)*(length(fityrs)))+1):((top_L[10])*(length(fityrs)))],type='l', col='orange',lty=2)
points(11,notif2010[1])
segments(11,notif2010l[1],11,notif2010u[1])

## prevalence plot
plot(seq(fityrs),xoutplot$"TBPb15+"[1:(length(fityrs))], ylab="Prevalence/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='orange')
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[1])-1)*(length(fityrs)))+1):((top_L[1])*(length(fityrs)))],type='l', col='blue',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[2])-1)*(length(fityrs)))+1):((top_L[2])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[3])-1)*(length(fityrs)))+1):((top_L[3])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[4])-1)*(length(fityrs)))+1):((top_L[4])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[5])-1)*(length(fityrs)))+1):((top_L[5])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[6])-1)*(length(fityrs)))+1):((top_L[6])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[7])-1)*(length(fityrs)))+1):((top_L[7])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[8])-1)*(length(fityrs)))+1):((top_L[8])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[9])-1)*(length(fityrs)))+1):((top_L[9])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$"TBPb15+"[((((top_L[10])-1)*(length(fityrs)))+1):((top_L[10])*(length(fityrs)))],type='l', col='orange',lty=2)
points(1,prevalence2000[1])
points(11,prevalence2010[1])
segments(1,prevalence2000l[1],1,prevalence2000u[1])
segments(11,prevalence2010l[1],11,prevalence2010u[1])

# Mortality plot
plot(seq(fityrs),xoutplot$TBMtot[1:(length(fityrs))], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,20),type='l',col='orange')
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[1])-1)*(length(fityrs)))+1):((top_L[1])*(length(fityrs)))],type='l', col='blue',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[2])-1)*(length(fityrs)))+1):((top_L[2])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[3])-1)*(length(fityrs)))+1):((top_L[3])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[4])-1)*(length(fityrs)))+1):((top_L[4])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[5])-1)*(length(fityrs)))+1):((top_L[5])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[6])-1)*(length(fityrs)))+1):((top_L[6])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[7])-1)*(length(fityrs)))+1):((top_L[7])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[8])-1)*(length(fityrs)))+1):((top_L[8])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[9])-1)*(length(fityrs)))+1):((top_L[9])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBMtot[((((top_L[10])-1)*(length(fityrs)))+1):((top_L[10])*(length(fityrs)))],type='l', col='orange',lty=2)
points(11,mortality2010[1])
segments(11,mortality2010l[1],11,mortality2010u[1])

# Incidence plot
plot(seq(fityrs),xoutplot$TBItot[1:(length(fityrs))], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='orange')
lines(seq(fityrs),xoutplot$TBItot[((((top_L[1])-1)*(length(fityrs)))+1):((top_L[1])*(length(fityrs)))],type='l', col='blue',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[2])-1)*(length(fityrs)))+1):((top_L[2])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[3])-1)*(length(fityrs)))+1):((top_L[3])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[4])-1)*(length(fityrs)))+1):((top_L[4])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[5])-1)*(length(fityrs)))+1):((top_L[5])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[6])-1)*(length(fityrs)))+1):((top_L[6])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[7])-1)*(length(fityrs)))+1):((top_L[7])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[8])-1)*(length(fityrs)))+1):((top_L[8])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[9])-1)*(length(fityrs)))+1):((top_L[9])*(length(fityrs)))],type='l', col='orange',lty=2)
lines(seq(fityrs),xoutplot$TBItot[((((top_L[10])-1)*(length(fityrs)))+1):((top_L[10])*(length(fityrs)))],type='l', col='orange',lty=2)
points(11,incidence2010[1])
segments(11,incidence2010l[1],11,incidence2010u[1])
