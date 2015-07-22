### BASIC PLOTS FOR FITTING ###

# plots<-"/Users/Rebecca/Vaccine_china/Plots"
# setwd(plots)

print(TBI[111])
print(TBN[111,4])
print(TBN[111,5])


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
mortality2010u<- c(4.54,0.32,2.10,17.26)
mortality2010l<- c(4.84,0.27,1.72,14.12)


# yrplot2000<-rep(2000,4)
# yrplot2010<-rep(2010,5)
# yrplot2050<-rep(2050,5)
# yrplot2010b<-rep(2010,4)
# yrplot2010c<-rep(2010,3)
# poplist<- as.list(c("totpop","pop014","pop1554","pop5564","pop65plus")) #population 
# poplist2<- as.list(c("pop014","pop1554","pop5564","pop65plus"))  #incidence
# poplist3<- as.list(c("pop1529","pop3044","pop4559","pop60plus"))  #prevalence
# poplist3<- as.list(c("pop014","pop1559","pop60plus")) #mortality
# colnames(population2050)<-poplist


## PLOTS TEMP REPLACE GWENS



##incidence plot

par(mfrow=c(2,3))
plot(seq(1990,2050),TBI[91:151,1], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='orange')
points(2010,incidence2010[1])
#points(2050,TBI[151,1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
plot(seq(1990,2050),TBI[91:151,2], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='red')
# points(2010,incidence2010[2])
# segments(2010,incidence2010l[2],2010,incidence2010u[2])
plot(seq(1990,2050),TBI[91:151,3], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='blue')
# points(2010,incidence2010[3])
# segments(2010,incidence2010l[3],2010,incidence2010u[3])
plot(seq(1990,2050),TBI[91:151,4], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='purple')
# points(2010,incidence2010[4])
# segments(2010,incidence2010l[4],2010,incidence2010u[4])
plot(seq(1990,2050),TBI[91:151,5], ylab="Incidence/100,000pop/yr",xlab="Year", ylim=c(0,300),type='l',col='green')
# points(2010,incidence2010[5])
# segments(2010,incidence2010l[5],2010,incidence2010u[5])
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))


## notifications plot

par(mfrow=c(2,3))
plot(seq(1990,2050),TBN[91:151,1], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='orange')
points(2010,notif2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
plot(seq(1990,2050),TBN[91:151,2], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='red')
points(2010,notif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
plot(seq(1990,2050),TBN[91:151,3], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='blue')
points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
plot(seq(1990,2050),TBN[91:151,4], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='purple')
points(2010,notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
plot(seq(1990,2050),TBN[91:151,5], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,200),type='l',col='green')
points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))



# plot(seq(1990,2050),TBI[91:151,1], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),type='l')
# points(seq(1990,2050),TBI[91:151,2],type='l',col='red')
# points(seq(1990,2050),TBI[91:151,3],type='l',col='blue')
# points(seq(1990,2050),TBI[91:151,4],type='l',col='green')
# points(seq(1990,2050),TBI[91:151,5],type='l',col='pink')
# points(yrplot2010,incidence2010)

# plot(seq(1900,2050),TBI[1:151,1], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),type='l')
# plot(seq(1900,2050),Xn[,"new_I"][1:151], ylim=c(0,1000))
# plot(seq(1900,2050,0.5),Xn[,"new_I"], ylim=c(0,1000))
#plot(Xn[,"TBMtot"],ylim=c(0,max(Xn[,"TBMtot"])))

#CDR plot
# par(mfrow=c(1,1))
# plot(seq(1990,2050),cdrm["China",], ylab="CDR",xlab="year", ylim=c(0,1),main="CDR",type='l',col='purple')

## prevalence plot

par(mfrow=c(2,3))
plot(seq(1990,2050),TBPb[91:151,8], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='orange')
points(2000,prevalence2000[1])
points(2010,prevalence2010[1])
segments(2000,prevalence2000l[1],2000,prevalence2000u[1])
segments(2010,prevalence2010l[1],2010,prevalence2010u[1])
plot(seq(1990,2050),TBPb[91:151,3], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='lightblue')
points(2000,prevalence2000[2])
points(2010,prevalence2010[2])
segments(2000,prevalence2000l[2],2000,prevalence2000u[2])
segments(2010,prevalence2010l[2],2010,prevalence2010u[2])
plot(seq(1990,2050),TBPb[91:151,4], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='blue')
points(2000,prevalence2000[3])
points(2010,prevalence2010[3])
segments(2000,prevalence2000l[3],2000,prevalence2000u[3])
segments(2010,prevalence2010l[3],2010,prevalence2010u[3])
plot(seq(1990,2050),TBPb[91:151,5], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='purple')
points(2000,prevalence2000[4])
points(2010,prevalence2010[4])
segments(2000,prevalence2000l[4],2000,prevalence2000u[4])
segments(2010,prevalence2010l[4],2010,prevalence2010u[4])
plot(seq(1990,2050),TBPb[91:151,6], ylab="Prevalence/100,000population",xlab="Year", ylim=c(0,700),type='l',col='green')
points(2000,prevalence2000[5])
points(2010,prevalence2010[5])
segments(2000,prevalence2000l[5],2000,prevalence2000u[5])
segments(2010,prevalence2010l[5],2010,prevalence2010u[5])
plot.new()
legend("center",c("Overall(15+) years","15-29 years","30-44 years","45-59 years","≥60 years"), lty=1,col=c("orange","lightblue","blue","purple","green"))




# par(mfrow=c(1,1))
# plot(seq(1900,2050,0.5),Xn[,'L'], ylab="L",xlab="year", ylim=c(0,2000000),type='l')
# lines(seq(1900,2050,0.5),Xn[,"PSIZE"], col='red')
# points(seq(1990,2050),TBP[91:151,3],type='l',col='red')
# points(seq(1990,2050),TBP[91:151,4],type='l',col='blue')
# points(seq(1990,2050),TBP[91:151,5],type='l',col='green')
# points(seq(1990,2050),TBP[91:151,6],type='l',col='pink')
# points(yrplot2000,prevalence2000)
# points(yrplot2010b,prevalence2010)

# Mortality plot
par(mfrow=c(2,3))
plot(seq(1990,2050),TBM[91:151,1], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
plot(seq(1990,2050),TBM[91:151,2], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,5),type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
plot.new()
plot(seq(1990,2050),TBM[91:151,6], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
plot(seq(1990,2050),TBM[91:151,7], ylab="Mortality/100,000pop/yr",xlab="Year", ylim=c(0,50),type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
plot.new()
legend("top",c("Overall","0-14 years","15-59 years","≥60 years"), lty=1,col=c("orange","red","blue","green"))


# # prevalence of infection plot
par(mfrow=c(2,3))
plot(seq(1990,2050),TBPI[91:151,1], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='orange')
plot(seq(1990,2050),TBPI[91:151,2], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='red')
plot(seq(1990,2050),TBPI[91:151,3], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='blue')
plot(seq(1990,2050),TBPI[91:151,4], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='purple')
plot(seq(1990,2050),TBPI[91:151,5], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='green')
# plot(seq(1990,2050),TBPI[91:151,15], ylab="Prevalence of Infection (%) 6574",xlab="year", ylim=c(0,100),type='l',col='green')
# plot(seq(1990,2050),TBPI[91:151,16], ylab="Prevalence of Infection (%) 75+",xlab="year", ylim=c(0,100),type='l',col='green')
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))

# ## Prev of Inf plot to compare with gao data
# par(mfrow=c(1,1))
# 
# plot(seq(1990,2050),TBPI[91:151,1], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),type='l',col='orange')
# 




##population plot
par(mfrow=c(2,3))
plot(seq(1990,2050),PSIZEy[91:151,1], ylab="Population (thousands)",xlab="Year", ylim=c(0,4000000),type='l',col='orange')
points(2050,population2050[1])
points(2010,population2010[1])
segments(2050,population2050l[1],2050,population2050u[1])
plot(seq(1990,2050),PSIZEy[91:151,2], ylab="Population (thousands)",xlab="Year", ylim=c(0,1000000),type='l',col='red')
points(2050,population2050[2])
points(2010,population2010[2])
segments(2050,population2050l[2],2050,population2050u[2])
plot(seq(1990,2050),PSIZEy[91:151,3], ylab="Population (thousands)",xlab="Year", ylim=c(0,1000000),type='l',col='blue')
points(2050,population2050[3])
points(2010,population2010[3])
segments(2050,population2050l[3],2050,population2050u[3])
plot(seq(1990,2050),PSIZEy[91:151,4], ylab="Population (thousands)",xlab="Year", ylim=c(0,1000000),type='l',col='purple')
points(2050,population2050[4])
points(2010,population2010[4])
segments(2050,population2050l[4],2050,population2050u[4])
plot(seq(1990,2050),PSIZEy[91:151,5], ylab="Population 65+ (thousands)",xlab="Year", ylim=c(0,1000000),type='l',col='green')
points(2050,population2050[5])
points(2025,population2025)
points(2010,population2010[5])
segments(2050,population2050l[5],2050,population2050u[5])
# plot(seq(1990,2050),PSIZEy[91:151,20], ylab="Population 5574 (thousands)",xlab="Year", ylim=c(0,1000000),type='l',col='green')
# plot(seq(1990,2050),PSIZEy[91:151,21], ylab="Population 75+ (thousands)",xlab="Year", ylim=c(0,1000000),type='l',col='green')
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))
# points(seq(1990,2050),PSIZEy[91:151,2],type='l',col='red')
# points(seq(1990,2050),PSIZEy[91:151,3],type='l',col='blue')
# points(seq(1990,2050),PSIZEy[91:151,4],type='l',col='green')
# points(seq(1990,2050),PSIZEy[91:151,5],type='l',col='pink')


##reinfection/reactivation plot 
par(mfrow=c(2,3))
plot(seq(1990,2050),TBRa[91:151,1], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='orange')
lines(seq(1990,2050),TBRi[91:151,1],lty=5,col='orange')
#lines(seq(1970,2050),((TBRi[71:151,1])+TBRa[71:151,1]),lty=5,col='orange')
plot(seq(1990,2050),TBRa[91:151,2], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='red')
lines(seq(1990,2050),TBRi[91:151,2],lty=5,col='red')
#lines(seq(1970,2050),((TBRi[71:151,2])+TBRa[71:151,2]),lty=5,col='red')
plot(seq(1990,2050),TBRa[91:151,3], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='blue')
lines(seq(1990,2050),TBRi[91:151,3],lty=5,col='blue')
#lines(seq(1970,2050),((TBRi[71:151,3])+TBRa[71:151,3]),lty=5,col='blue')
plot(seq(1990,2050),TBRa[91:151,4], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='purple')
lines(seq(1990,2050),TBRi[91:151,4],lty=5,col='purple')
#lines(seq(1970,2050),((TBRi[71:151,4])+TBRa[71:151,4]),lty=5,col='green')
plot(seq(1990,2050),TBRa[91:151,5], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='green')
lines(seq(1990,2050),TBRi[91:151,5],lty=5,col='green')
#lines(seq(1970,2050),((TBRi[71:151,5])+TBRa[71:151,5]),lty=5,col='pink')
plot.new()
legend("center",c("New Infection","Reactivation","Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=c(5,1,1,1,1,1,1),col=c("black","black","orange","red","blue","purple","green"))


# par(mfrow=c(2,3))
# plot(seq(1990,2050),TBRa2[91:151,1], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='orange')
# lines(seq(1990,2050),TBRi2[91:151,1],lty=5,col='orange')
# #lines(seq(1970,2050),((TBRi[71:151,1])+TBRa[71:151,1]),lty=5,col='orange')
# plot(seq(1990,2050),TBRa2[91:151,2], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='red')
# lines(seq(1990,2050),TBRi2[91:151,2],lty=5,col='red')
# #lines(seq(1970,2050),((TBRi[71:151,2])+TBRa[71:151,2]),lty=5,col='red')
# plot(seq(1990,2050),TBRa2[91:151,3], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='blue')
# lines(seq(1990,2050),TBRi2[91:151,3],lty=5,col='blue')
# #lines(seq(1970,2050),((TBRi[71:151,3])+TBRa[71:151,3]),lty=5,col='blue')
# plot(seq(1990,2050),TBRa2[91:151,4], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='purple')
# lines(seq(1990,2050),TBRi2[91:151,4],lty=5,col='purple')
# #lines(seq(1970,2050),((TBRi[71:151,4])+TBRa[71:151,4]),lty=5,col='green')
# plot(seq(1990,2050),TBRa2[91:151,5], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='green')
# lines(seq(1990,2050),TBRi2[91:151,5],lty=5,col='green')
# #lines(seq(1970,2050),((TBRi[71:151,5])+TBRa[71:151,5]),lty=5,col='pink')
# plot.new()
# legend("center",c("New Infection","Reactivation","Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=c(5,1,1,1,1,1,1),col=c("black","black","orange","red","blue","purple","green"))
# 

#### ARI ###

# par(mfrow=c(2,2))
# plot(seq(1990,2050),ARI[91:151,1], ylab="ARI (%)",xlab="Year", ylim=c(0,2),type='l',col='orange')
# plot(seq(1990,2050),ARI[91:151,2], ylab="ARI 0-14yr (%)",xlab="Year", ylim=c(0,2),type='l',col='red')
# plot(seq(1990,2050),ARI[91:151,3], ylab="ARI 15-54yr (%)",xlab="Year", ylim=c(0,2),type='l',col='blue')
# plot(seq(1990,2050),ARI[91:151,4], ylab="ARI 55+yr (%)",xlab="Year", ylim=c(0,2),type='l',col='green')


### % population and cases per age group ###

PcPop<-matrix(0,4,6)
popsizeall50<-sum(psize[301:302])
casesall50<-sum(new_actv[301:302,])
popsizeall25<-sum(psize[251:252])
casesall25<-sum(new_actv[251:252,])


PcPop[1,1]<-sum(psize014[251:252])/popsizeall25*100
PcPop[1,2]<-sum(psize1524[251:252])/popsizeall25*100
PcPop[1,3]<-sum(psize2554[251:252])/popsizeall25*100
PcPop[1,4]<-sum(psize5564[251:252])/popsizeall25*100
PcPop[1,5]<-((sum(psize5574[251:252]))-(sum(psize5564[251:252])))/popsizeall25*100
PcPop[1,6]<-sum(psize75plus[251:252])/popsizeall25*100


PcPop[2,1]<-sum(new_actv[251:252,1:15])/casesall25*100
PcPop[2,2]<-sum(new_actv[251:252,16:25])/casesall25*100
PcPop[2,3]<-sum(new_actv[251:252,26:55])/casesall25*100
PcPop[2,4]<-sum(new_actv[251:252,56:65])/casesall25*100
PcPop[2,5]<-sum(new_actv[251:252,66:75])/casesall25*100
PcPop[2,6]<-sum(new_actv[251:252,76:Mnage])/casesall25*100

PcPop[3,1]<-sum(psize014[301:302])/popsizeall50*100
PcPop[3,2]<-sum(psize1524[301:302])/popsizeall50*100
PcPop[3,3]<-sum(psize2554[301:302])/popsizeall50*100
PcPop[3,4]<-sum(psize5564[301:302])/popsizeall50*100
PcPop[3,5]<-((sum(psize5574[301:302]))-(sum(psize5564[301:302])))/popsizeall50*100
PcPop[3,6]<-sum(psize75plus[301:302])/popsizeall50*100


PcPop[4,1]<-sum(new_actv[301:302,1:15])/casesall50*100
PcPop[4,2]<-sum(new_actv[301:302,16:25])/casesall50*100
PcPop[4,3]<-sum(new_actv[301:302,26:55])/casesall50*100
PcPop[4,4]<-sum(new_actv[301:302,56:65])/casesall50*100
PcPop[4,5]<-sum(new_actv[301:302,66:75])/casesall50*100
PcPop[4,6]<-sum(new_actv[301:302,76:Mnage])/casesall50*100

casesall90<-sum(new_actv[181:182,])
PcPop2<-matrix(0,1,6)
PcPop2[1,1]<-sum(new_actv[181:182,1:15])/casesall90*100
PcPop2[1,2]<-sum(new_actv[181:182,16:25])/casesall90*100
PcPop2[1,3]<-sum(new_actv[181:182,26:55])/casesall90*100
PcPop2[1,4]<-sum(new_actv[181:182,56:65])/casesall90*100
PcPop2[1,5]<-sum(new_actv[181:182,66:75])/casesall90*100
PcPop2[1,6]<-sum(new_actv[181:182,76:Mnage])/casesall90*100


colnames(PcPop)<-c("0-14","15-24","25-54","55-64","65-74","75+")
rownames(PcPop)<-c("2025 % population", "2025 %cases","2050 % population", "2050 %cases")

setwd("Outputs")
write.table(PcPop,'Percent pop_cases per agegrp.csv',sep=",",row.names=TRUE,col.names=TRUE)
setwd(home)


