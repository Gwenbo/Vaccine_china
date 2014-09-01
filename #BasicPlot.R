### BASIC PLOTS FOR FITTING ###

#data to fit to
population2010<- c(1359822,246707,863710,135859,113546)
population2050<- c(1384976,204187,623982,225492,331315)
population2050u<-c(3867612,300552,722200,248041,364447)
population2050l<-c(1153148,124968,527054,202943,298184)

incidence2010<- c(73.78,3.02,74.56,120.54,165.63)
incidence2010u<- c(83.48,3.41,84.37,136.40,187.43)
incidence2010l<- c(64.74,2.65,65.43,105.78,145.35)

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
plot(seq(1970,2050),TBI[71:151,1], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="Total Incidence",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
plot(seq(1970,2050),TBI[71:151,2], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="0-14years Incidence",type='l',col='red')
points(2010,incidence2010[2])
segments(2010,incidence2010l[2],2010,incidence2010u[2])
plot(seq(1970,2050),TBI[71:151,3], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="15-54years Incidence",type='l',col='blue')
points(2010,incidence2010[3])
segments(2010,incidence2010l[3],2010,incidence2010u[3])
plot(seq(1970,2050),TBI[71:151,4], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="55-64years Incidence",type='l',col='green')
points(2010,incidence2010[4])
segments(2010,incidence2010l[4],2010,incidence2010u[4])
plot(seq(1970,2050),TBI[71:151,5], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="65+ years Incidence",type='l',col='pink')
points(2010,incidence2010[5])
segments(2010,incidence2010l[5],2010,incidence2010u[5])
plot.new()


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
plot(seq(1970,2050),TBPb[71:151,1], ylab="Prevalence/100,000pop",xlab="year", ylim=c(0,700),main="Total Prevalence",type='l',col='orange')
points(2000,prevalence2000[1])
points(2010,prevalence2010[1])
segments(2000,prevalence2000l[1],2000,prevalence2000u[1])
segments(2010,prevalence2010l[1],2010,prevalence2010u[1])
plot(seq(1970,2050),TBPb[71:151,3], ylab="Prevalence/100,000pop",xlab="year", ylim=c(0,700),main="15-29years Prevalence",type='l',col='blue')
points(2000,prevalence2000[2])
points(2010,prevalence2010[2])
segments(2000,prevalence2000l[2],2000,prevalence2000u[2])
segments(2010,prevalence2010l[2],2010,prevalence2010u[2])
plot(seq(1970,2050),TBPb[71:151,4], ylab="Prevalence/100,000pop",xlab="year", ylim=c(0,700),main="30-44years Prevalence",type='l',col='green')
points(2000,prevalence2000[3])
points(2010,prevalence2010[3])
segments(2000,prevalence2000l[3],2000,prevalence2000u[3])
segments(2010,prevalence2010l[3],2010,prevalence2010u[3])
plot(seq(1970,2050),TBPb[71:151,5], ylab="Prevalence/100,000pop",xlab="year", ylim=c(0,700),main="45-59years Prevalence",type='l',col='pink')
points(2000,prevalence2000[4])
points(2010,prevalence2010[4])
segments(2000,prevalence2000l[4],2000,prevalence2000u[4])
segments(2010,prevalence2010l[4],2010,prevalence2010u[4])
plot(seq(1970,2050),TBPb[71:151,6], ylab="Prevalence/100,000pop",xlab="year", ylim=c(0,1000),main="60+ years Prevalence",type='l',col='red')
points(2000,prevalence2000[5])
points(2010,prevalence2010[5])
segments(2000,prevalence2000l[5],2000,prevalence2000u[5])
segments(2010,prevalence2010l[5],2010,prevalence2010u[5])
plot.new()


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
par(mfrow=c(2,2))
plot(seq(1970,2050),TBM[71:151,1], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,80),main="Total Mortality",type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
plot(seq(1970,2050),TBM[71:151,2], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,10),main="0-14years Mortality",type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
plot(seq(1970,2050),TBM[71:151,6], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,80),main="15-59years Mortality",type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
plot(seq(1970,2050),TBM[71:151,7], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,80),main="60+years Mortality",type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])

## prevalence of infection plot
par(mfrow=c(2,3))
plot(seq(1970,2050),TBPI[71:151,1], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),main="Total Prevalence Infection",type='l',col='orange')
plot(seq(1970,2050),TBPI[71:151,2], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),main="0-14years Prevalence Infection",type='l',col='pink')
plot(seq(1970,2050),TBPI[71:151,3], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),main="15-54years Prevalence Infection",type='l',col='red')
plot(seq(1970,2050),TBPI[71:151,4], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),main="55-64years Prevalence Infection",type='l',col='blue')
plot(seq(1970,2050),TBPI[71:151,5], ylab="Prevalence of Infection (%)",xlab="year", ylim=c(0,100),main="65+ years Prevalence Infection",type='l',col='green')


##population plot
par(mfrow=c(2,3))
plot(seq(1900,2050),PSIZEy[1:151,1], ylab="Population (thousands)",xlab="year", ylim=c(0,4000000),main="Total Population",type='l',col='orange')
points(2050,population2050[1])
points(2010,population2010[1])
segments(2050,population2050l[1],2050,population2050u[1])
plot(seq(1970,2050),PSIZEy[71:151,2], ylab="Population (thousands)",xlab="year", ylim=c(0,1000000),main="0-14years Population",type='l',col='red')
points(2050,population2050[2])
points(2010,population2010[2])
segments(2050,population2050l[2],2050,population2050u[2])
plot(seq(1970,2050),PSIZEy[71:151,3], ylab="Population (thousands)",xlab="year", ylim=c(0,1000000),main="15-54years Population",type='l',col='blue')
points(2050,population2050[3])
points(2010,population2010[3])
segments(2050,population2050l[3],2050,population2050u[3])
plot(seq(1970,2050),PSIZEy[71:151,4], ylab="Population (thousands)",xlab="year", ylim=c(0,1000000),main="55-64years Population",type='l',col='green')
points(2050,population2050[4])
points(2010,population2010[4])
segments(2050,population2050l[4],2050,population2050u[4])
plot(seq(1900,2050),PSIZEy[1:151,5], ylab="Population (thousands)",xlab="year", ylim=c(0,1000000),main="65+ years Population",type='l',col='pink')
points(2050,population2050[5])
points(2010,population2010[5])
segments(2050,population2050l[5],2050,population2050u[5])
plot.new()
points(seq(1990,2050),PSIZEy[91:151,2],type='l',col='red')
points(seq(1990,2050),PSIZEy[91:151,3],type='l',col='blue')
points(seq(1990,2050),PSIZEy[91:151,4],type='l',col='green')
points(seq(1990,2050),PSIZEy[91:151,5],type='l',col='pink')


##reinfection/reactivation plot 
par(mfrow=c(2,3))
plot(seq(1970,2050),TBRa[71:151,1], ylab="%",xlab="year", ylim=c(0,100),main="Total Population New Infection vs Reactivation",type='l',col='orange')
lines(seq(1970,2050),TBRi[71:151,1],lty=5,col='orange')
#lines(seq(1970,2050),((TBRi[71:151,1])+TBRa[71:151,1]),lty=5,col='orange')
plot(seq(1970,2050),TBRa[71:151,2], ylab="%",xlab="year", ylim=c(0,100),main="0-14years  New Infection vs Reactivation",type='l',col='red')
lines(seq(1970,2050),TBRi[71:151,2],lty=5,col='red')
#lines(seq(1970,2050),((TBRi[71:151,2])+TBRa[71:151,2]),lty=5,col='red')
plot(seq(1970,2050),TBRa[71:151,3], ylab="%",xlab="year", ylim=c(0,100),main="15-54years New Infection vs Reactivation",type='l',col='blue')
lines(seq(1970,2050),TBRi[71:151,3],lty=5,col='blue')
#lines(seq(1970,2050),((TBRi[71:151,3])+TBRa[71:151,3]),lty=5,col='blue')
plot(seq(1970,2050),TBRa[71:151,4], ylab="%",xlab="year", ylim=c(0,100),main="55-64years New Infection vs Reactivation",type='l',col='green')
lines(seq(1970,2050),TBRi[71:151,4],lty=5,col='green')
#lines(seq(1970,2050),((TBRi[71:151,4])+TBRa[71:151,4]),lty=5,col='green')
plot(seq(1970,2050),TBRa[71:151,5], ylab="%",xlab="year", ylim=c(0,100),main="65+ years New Infection vs Reactivation",type='l',col='pink')
lines(seq(1970,2050),TBRi[71:151,5],lty=5,col='pink')
#lines(seq(1970,2050),((TBRi[71:151,5])+TBRa[71:151,5]),lty=5,col='pink')
plot.new()
legend("center",c("New Infection","Reactivation"), lty=c(5,1))
