###VACXCINE PLOTS

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


par(mfrow=c(1,1))
#set up how to find first row of each run
#typen is -1 as dont use the first one at the moment
lastcol<-(((typen-1)*(combn))+1)*steps
y1<-seq(1,lastcol,steps)

### TBI ####

#TBItot plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="Total Incidence pre-exposure",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),15], col='orange',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),15], col='orange',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),15], col='orange',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),15], col='orange',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),15], col='orange',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),15], col='orange',lty=6, lwd=2)

#TBItot plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="Total Incidence post-exp",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),15], col='orange',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),15], col='orange',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),15], col='orange',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),15], col='orange',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),15], col='orange',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),15], col='orange',lty=6, lwd=2)



#TBI014 plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),16], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="0-14yrs Incidence  pre-exp",type='l',col='red')
points(2010,incidence2010[2])
segments(2010,incidence2010l[2],2010,incidence2010u[2])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),16], col='red',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),16], col='red',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),16], col='red',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),16], col='red',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),16], col='red',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),16], col='red',lty=6, lwd=2)

#TBI014 plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),16], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="0-14yrs Incidence post-exp",type='l',col='red')
points(2010,incidence2010[2])
segments(2010,incidence2010l[2],2010,incidence2010u[2])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),16], col='red',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),16], col='red',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),16], col='red',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),16], col='red',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),16], col='red',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),16], col='red',lty=6, lwd=2)


#TBI15-54 plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),17], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="15-54yrs Incidence  pre-exp",type='l',col='blue')
points(2010,incidence2010[3])
segments(2010,incidence2010l[3],2010,incidence2010u[3])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),17], col='blue',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),17], col='blue',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),17], col='blue',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),17], col='blue',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),17], col='blue',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),17], col='blue',lty=6, lwd=2)

#TBI15-54 plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),17], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="15-54yrs Incidence post-exp",type='l',col='blue')
points(2010,incidence2010[3])
segments(2010,incidence2010l[3],2010,incidence2010u[3])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),17], col='blue',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),17], col='blue',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),17], col='blue',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),17], col='blue',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),17], col='blue',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),17], col='blue',lty=6, lwd=2)


#TBI55-64 plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),18], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="55-64yrs Incidence  pre-exp",type='l',col='green')
points(2010,incidence2010[4])
segments(2010,incidence2010l[4],2010,incidence2010u[4])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),18], col='green',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),18], col='green',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),18], col='green',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),18], col='green',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),18], col='green',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),18], col='green',lty=6, lwd=2)

#TBI55-64 plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),18], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="55-64yrs Incidence post-exp",type='l',col='green')
points(2010,incidence2010[4])
segments(2010,incidence2010l[4],2010,incidence2010u[4])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),18], col='green',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),18], col='green',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),18], col='green',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),18], col='green',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),18], col='green',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),18], col='green',lty=6, lwd=2)

#TBI65+ plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),19], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="65+yrs Incidence  pre-exp",type='l',col='pink')
points(2010,incidence2010[5])
segments(2010,incidence2010l[5],2010,incidence2010u[5])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),19], col='pink',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),19], col='pink',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),19], col='pink',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),19], col='pink',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),19], col='pink',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),19], col='pink',lty=6, lwd=2)

#TBI65+ plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),19], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,300),main="65+yrs Incidence post-exp",type='l',col='pink')
points(2010,incidence2010[5])
segments(2010,incidence2010l[5],2010,incidence2010u[5])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),19], col='pink',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),19], col='pink',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),19], col='pink',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),19], col='pink',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),19], col='pink',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),19], col='pink',lty=6, lwd=2)





######## TBM #########

#TBMtot plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),20], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="Total Mortality pre-exposure",type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),20], col='orange',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),20], col='orange',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),20], col='orange',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),20], col='orange',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),20], col='orange',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),20], col='orange',lty=6, lwd=2)

#TBMtot plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),20], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="Total Mortality post-exp",type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),20], col='orange',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),20], col='orange',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),20], col='orange',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),20], col='orange',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),20], col='orange',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),20], col='orange',lty=6, lwd=2)



#TBM014 plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),21], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="0-14yrs Mortality  pre-exp",type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),21], col='red',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),21], col='red',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),21], col='red',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),21], col='red',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),21], col='red',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),21], col='red',lty=6, lwd=2)

#TBM014 plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),21], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="0-14yrs Mortality post-exp",type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),21], col='red',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),21], col='red',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),21], col='red',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),21], col='red',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),21], col='red',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),21], col='red',lty=6, lwd=2)


#TBM15-59 plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),22], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="15-59yrs Mortality  pre-exp",type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),22], col='blue',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),22], col='blue',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),22], col='blue',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),22], col='blue',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),22], col='blue',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),22], col='blue',lty=6, lwd=2)

#TBM15-59 plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),22], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="15-59yrs Mortality post-exp",type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),22], col='blue',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),22], col='blue',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),22], col='blue',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),22], col='blue',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),22], col='blue',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),22], col='blue',lty=6, lwd=2)


#TBM60+ plot vxtype2
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),23], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="60+yrs Incidence  pre-exp",type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[2]+70):(y1[2]+150),23], col='green',lty=2)
lines(seq(1970,2050),dfvx[(y1[3]+70):(y1[3]+150),23], col='green',lty=3)
lines(seq(1970,2050),dfvx[(y1[4]+70):(y1[4]+150),23], col='green',lty=4)
lines(seq(1970,2050),dfvx[(y1[5]+70):(y1[5]+150),23], col='green',lty=5)
lines(seq(1970,2050),dfvx[(y1[6]+70):(y1[6]+150),23], col='green',lty=6)
lines(seq(1970,2050),dfvx[(y1[7]+70):(y1[7]+150),23], col='green',lty=6, lwd=2)

#TBM60+ plot vxtype3
#baseline
plot(seq(1970,2050),dfvx[(y1[1]+70):(y1[1]+150),23], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,300),main="60+yrs Mortality post-exp",type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
#vx2 40ve,30cov
lines(seq(1970,2050),dfvx[(y1[8]+70):(y1[8]+150),23], col='green',lty=2)
lines(seq(1970,2050),dfvx[(y1[9]+70):(y1[9]+150),23], col='green',lty=3)
lines(seq(1970,2050),dfvx[(y1[10]+70):(y1[10]+150),23], col='green',lty=4)
lines(seq(1970,2050),dfvx[(y1[11]+70):(y1[11]+150),23], col='green',lty=5)
lines(seq(1970,2050),dfvx[(y1[12]+70):(y1[12]+150),23], col='green',lty=6)
lines(seq(1970,2050),dfvx[(y1[13]+70):(y1[13]+150),23], col='green',lty=6, lwd=2)

