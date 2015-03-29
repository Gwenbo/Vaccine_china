###VACCINE PLOTS

population2010<- c(1359822,246707,863710,135859,113546)
population2050<- c(1384976,204187,623982,225492,331315)
population2050u<-c(3867612,300552,722200,248041,364447)
population2050l<-c(1153148,124968,527054,202943,298184)

incidence2010<- c(73.78,3.02,74.56,120.54,165.63)
incidence2010u<- c(83.48,3.41,84.37,136.40,187.43)
incidence2010l<- c(64.74,2.65,65.43,105.78,145.35)

#noitfication lower limit is the reported notifications, notification upper limit assumes that all from private hospitals are missed from notification reporting (80% of cases reported to CDC, 20% private hospital)  
notif2010 <- c()
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


par(mfrow=c(1,1))
#set up how to find first row of each run
#typen is -1 as dont use the first one at the moment
lastcol<-(((typen-1)*(combn))+1)*steps
y1<-seq(1,lastcol,steps)

###### TBI ########
par(mfcol=c(2,2))

#TBItot plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="Total Incidence pre-exposure",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),15], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),15], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),15], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),15], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),15], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),15], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")


#TBItot plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="Total Incidence post-exp",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),15], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),15], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),15], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),15], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),15], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),15], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")

#TBItot plot vxtype4
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="Total Incidence Latency vacc",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[14]+100):(y1[14]+150),15], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[15]+100):(y1[15]+150),15], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[16]+100):(y1[16]+150),15], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[17]+100):(y1[17]+150),15], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[18]+100):(y1[18]+150),15], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[19]+100):(y1[19]+150),15], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")


#TBItot plot vxtype5 (adult)
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="Total Incidence adult pre-exp vacc",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[20]+100):(y1[20]+150),15], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[21]+100):(y1[21]+150),15], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[22]+100):(y1[22]+150),15], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[23]+100):(y1[23]+150),15], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[24]+100):(y1[24]+150),15], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[25]+100):(y1[25]+150),15], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")

#TBItot plot vxtype6 (adult)
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="Total Incidence adult mixed effects vacc",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[26]+100):(y1[26]+150),15], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[27]+100):(y1[27]+150),15], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[28]+100):(y1[28]+150),15], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[29]+100):(y1[29]+150),15], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[30]+100):(y1[30]+150),15], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[31]+100):(y1[31]+150),15], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")


#TBItot plot vxtype7 (adult)
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),15], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="Total Incidence adult latency vacc",type='l',col='orange')
points(2010,incidence2010[1])
segments(2010,incidence2010l[1],2010,incidence2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[32]+100):(y1[32]+150),15], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[33]+100):(y1[33]+150),15], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[34]+100):(y1[34]+150),15], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[35]+100):(y1[35]+150),15], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[36]+100):(y1[36]+150),15], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[37]+100):(y1[37]+150),15], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")





#TBI014 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),16], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,20),main="0-14yrs Incidence  pre-exp",type='l',col='red')
points(2010,incidence2010[2])
segments(2010,incidence2010l[2],2010,incidence2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),16], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),16], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),16], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),16], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),16], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),16], col='red',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="red")

#TBI014 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),16], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,20),main="0-14yrs Incidence post-exp",type='l',col='red')
points(2010,incidence2010[2])
segments(2010,incidence2010l[2],2010,incidence2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),16], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),16], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),16], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),16], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),16], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),16], col='red',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="red")


#TBI15-54 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),17], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="15-54yrs Incidence  pre-exp",type='l',col='blue')
points(2010,incidence2010[3])
segments(2010,incidence2010l[3],2010,incidence2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),17], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),17], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),17], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),17], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),17], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),17], col='blue',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="blue")


#TBI15-54 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),17], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="15-54yrs Incidence post-exp",type='l',col='blue')
points(2010,incidence2010[3])
segments(2010,incidence2010l[3],2010,incidence2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),17], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),17], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),17], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),17], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),17], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),17], col='blue',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="blue")


#TBI55-64 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),18], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="55-64yrs Incidence  pre-exp",type='l',col='purple')
points(2010,incidence2010[4])
segments(2010,incidence2010l[4],2010,incidence2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),18], col='purple',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),18], col='purple',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),18], col='purple',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),18], col='purple',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),18], col='purple',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),18], col='purple',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="purple")

#TBI55-64 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),18], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,200),main="55-64yrs Incidence post-exp",type='l',col='purple')
points(2010,incidence2010[4])
segments(2010,incidence2010l[4],2010,incidence2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),18], col='purple',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),18], col='purple',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),18], col='purple',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),18], col='purple',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),18], col='purple',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),18], col='purple',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="purple")

#TBI65+ plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),19], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,400),main="65+yrs Incidence  pre-exp",type='l',col='green')
points(2010,incidence2010[5])
segments(2010,incidence2010l[5],2010,incidence2010u[5])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),19], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),19], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),19], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),19], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),19], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),19], col='green',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="green")

#TBI65+ plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),19], ylab="Incidence/100,000pop/yr",xlab="year", ylim=c(0,400),main="65+yrs Incidence post-exp",type='l',col='green')
points(2010,incidence2010[5])
segments(2010,incidence2010l[5],2010,incidence2010u[5])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),19], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),19], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),19], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),19], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),19], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),19], col='green',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="green")





###### TBN ########
par(mfcol=c(2,2))

#TBNtot plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),22], ylab="Notifications/100,000pop/yr",xlab="year", ylim=c(0,100),main="Total Notifications Pre-exposure",type='l',col='orange')
#points(2010,incidence2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),22], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),22], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),22], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),22], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),22], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),22], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")


#TBNtot plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),22], ylab="Notifications/100,000pop/yr",xlab="year", ylim=c(0,100),main="Total Notifications Post-exp",type='l',col='orange')
#points(2010,notif2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),22], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),22], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),22], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),22], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),22], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),22], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")

#TBNtot plot vxtype4
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),22], ylab="Notifications/100,000pop/yr",xlab="year", ylim=c(0,100),main="Total Notifications Latency vacc",type='l',col='orange')
#points(2010,notif2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[14]+100):(y1[14]+150),22], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[15]+100):(y1[15]+150),22], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[16]+100):(y1[16]+150),22], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[17]+100):(y1[17]+150),22], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[18]+100):(y1[18]+150),22], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[19]+100):(y1[19]+150),22], col='orange',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="orange")



#TBN014 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),23], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,20),main="0-14yrs Notification  pre-exp",type='l',col='red')
#points(2010,notif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),23], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),23], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),23], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),23], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),23], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),23], col='red',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="red")

#TBN014 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),23], ylab="Notifications/100,000pop/yr",xlab="year", ylim=c(0,20),main="0-14yrs Notification post-exp",type='l',col='red')
#points(2010,noif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),23], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),23], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),23], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),23], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),23], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),23], col='red',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="red")


#TBN014 plot vxtype4
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),23], ylab="Notifications/100,000pop/yr",xlab="year", ylim=c(0,20),main="0-14yrs Notification Latency vacc",type='l',col='red')
#points(2010,notif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[14]+100):(y1[14]+150),23], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[15]+100):(y1[15]+150),23], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[16]+100):(y1[16]+150),23], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[17]+100):(y1[17]+150),23], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[18]+100):(y1[18]+150),23], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[19]+100):(y1[19]+150),23], col='red',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="red")



#TBN15-54 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),24], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,100),main="15-54yrs Notification  pre-exp",type='l',col='blue')
#points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),24], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),24], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),24], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),24], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),24], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),24], col='blue',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="blue")


#TBN15-54 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),24], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,100),main="15-54yrs Notification post-exp",type='l',col='blue')
#points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),24], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),24], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),24], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),24], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),24], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),24], col='blue',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="blue")

#TBN15-54 plot vxtype4
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),24], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,100),main="15-54yrs Notification Latency Vaccine",type='l',col='blue')
#points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[14]+100):(y1[14]+150),24], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[15]+100):(y1[15]+150),24], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[16]+100):(y1[16]+150),24], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[17]+100):(y1[17]+150),24], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[18]+100):(y1[18]+150),24], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[19]+100):(y1[19]+150),24], col='blue',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="blue")


#TBN55-64 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),25], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,100),main="55-64yrs Notification  pre-exp",type='l',col='purple')
#points(2010, notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),25], col='purple',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),25], col='purple',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),25], col='purple',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),25], col='purple',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),25], col='purple',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),25], col='purple',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="purple")

#TBN55-64 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),25], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,100),main="55-64yrs Notification post-exp",type='l',col='purple')
#points(2010,notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),25], col='purple',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),25], col='purple',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),25], col='purple',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),25], col='purple',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),25], col='purple',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),25], col='purple',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="purple")

#TBN55-64 plot vxtype4
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),25], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,100),main="55-64yrs Notification Latency Vacc",type='l',col='purple')
#points(2010,notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[14]+100):(y1[14]+150),25], col='purple',lty=2)
lines(seq(2000,2050),dfvx[(y1[15]+100):(y1[15]+150),25], col='purple',lty=3)
lines(seq(2000,2050),dfvx[(y1[16]+100):(y1[16]+150),25], col='purple',lty=4)
lines(seq(2000,2050),dfvx[(y1[17]+100):(y1[17]+150),25], col='purple',lty=5)
lines(seq(2000,2050),dfvx[(y1[18]+100):(y1[18]+150),25], col='purple',lty=6)
lines(seq(2000,2050),dfvx[(y1[19]+100):(y1[19]+150),25], col='purple',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="purple")


#TBN65+ plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),26], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,200),main="65+yrs Notification  pre-exp",type='l',col='green')
#points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),26], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),26], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),26], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),26], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),26], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),26], col='green',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="green")

#TBN65+ plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),26], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,200),main="65+yrs Notification post-exp",type='l',col='green')
#points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),26], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),26], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),26], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),26], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),26], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),26], col='green',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="green")


#TBN65+ plot vxtype4
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),26], ylab="Notification/100,000pop/yr",xlab="year", ylim=c(0,200),main="65+yrs Notification Latency Vacc",type='l',col='green')
#points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
#vx4 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[14]+100):(y1[14]+150),26], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[15]+100):(y1[15]+150),26], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[16]+100):(y1[16]+150),26], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[17]+100):(y1[17]+150),26], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[18]+100):(y1[18]+150),26], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[19]+100):(y1[19]+150),26], col='green',lty=6, lwd=2)
plot.new()
legend("right",c("Baseline","40%VE, 30%coverage","40%VE, 70%coverage","60%VE, 30%coverage","60%VE, 70%coverage","80%VE, 30%coverage","80%VE, 70%coverage"), lty=c(1,2,3,4,5,6,6),lwd=c(1,1,1,1,1,1,2),col="green")





######## TBM #########

par(mfcol=c(1,2))
#TBMtot plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),29], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="Total Mortality pre-exposure",type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),29], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),29], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),29], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),29], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),29], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),29], col='orange',lty=6, lwd=2)

#TBMtot plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),29], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="Total Mortality post-exp",type='l',col='orange')
points(2010,mortality2010[1])
segments(2010,mortality2010l[1],2010,mortality2010u[1])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),29], col='orange',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),29], col='orange',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),29], col='orange',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),29], col='orange',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),29], col='orange',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),29], col='orange',lty=6, lwd=2)



#TBM014 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),30], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="0-14yrs Mortality  pre-exp",type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),30], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),30], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),30], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),30], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),30], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),30], col='red',lty=6, lwd=2)

#TBM014 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),30], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="0-14yrs Mortality post-exp",type='l',col='red')
points(2010,mortality2010[2])
segments(2010,mortality2010l[2],2010,mortality2010u[2])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),30], col='red',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),30], col='red',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),30], col='red',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),30], col='red',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),30], col='red',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),30], col='red',lty=6, lwd=2)


#TBM15-59 plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),31], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="15-59yrs Mortality  pre-exp",type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),31], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),31], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),31], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),31], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),31], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),31], col='blue',lty=6, lwd=2)

#TBM15-59 plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),31], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="15-59yrs Mortality post-exp",type='l',col='blue')
points(2010,mortality2010[3])
segments(2010,mortality2010l[3],2010,mortality2010u[3])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),31], col='blue',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),31], col='blue',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),31], col='blue',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),31], col='blue',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),31], col='blue',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),31], col='blue',lty=6, lwd=2)


#TBM60+ plot vxtype2
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),32], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="60+yrs Mortality  pre-exp",type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[2]+100):(y1[2]+150),32], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[3]+100):(y1[3]+150),32], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[4]+100):(y1[4]+150),32], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[5]+100):(y1[5]+150),32], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[6]+100):(y1[6]+150),32], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[7]+100):(y1[7]+150),32], col='green',lty=6, lwd=2)

#TBM60+ plot vxtype3
#baseline
plot(seq(2000,2050),dfvx[(y1[1]+100):(y1[1]+150),32], ylab="Mortality/100,000pop/yr",xlab="year", ylim=c(0,6),main="60+yrs Mortality post-exp",type='l',col='green')
points(2010,mortality2010[4])
segments(2010,mortality2010l[4],2010,mortality2010u[4])
#vx2 40ve,30cov
lines(seq(2000,2050),dfvx[(y1[8]+100):(y1[8]+150),32], col='green',lty=2)
lines(seq(2000,2050),dfvx[(y1[9]+100):(y1[9]+150),32], col='green',lty=3)
lines(seq(2000,2050),dfvx[(y1[10]+100):(y1[10]+150),32], col='green',lty=4)
lines(seq(2000,2050),dfvx[(y1[11]+100):(y1[11]+150),32], col='green',lty=5)
lines(seq(2000,2050),dfvx[(y1[12]+100):(y1[12]+150),32], col='green',lty=6)
lines(seq(2000,2050),dfvx[(y1[13]+100):(y1[13]+150),32], col='green',lty=6, lwd=2)


######  cumulative plots)

# rownames(cumulvx)<-c("baseline","pre4030","pre6030","pre8030","pre4070","pre6070","pre8070","post4030","post6030","post8030","post4070","post6070","pre8070")
# namec<-c("baseline","pre4030","pre6030","pre8030","pre4070","pre6070","pre8070","post4030","post6030","post8030","post4070","post6070","pre8070")
# 
# plot(cumulvx[1:13,1],xlab="Run", ylab="Cumulative mortality (thousands)", main='All ages cumulative mortality')
# plot(cumulvx[1:13,2],xlab="Run", ylab="Cumulative mortality (thousands)",main='0-14yrs cumulative mortality')
# plot(cumulvx[1:13,3],xlab="Run", ylab="Cumulative mortality (thousands)",main='15-54yrs cumulative mortality')
# plot(cumulvx[1:13,4],xlab="Run", ylab="Cumulative mortality (thousands)",main='55-64yrs cumulative mortality')
# plot(cumulvx[1:13,5],xlab="Run", ylab="Cumulative mortality (thousands)",main='65+ yrs cumulative mortality')
# 
# plot(cumulvx[1:13,8],xlab="Run", ylab="Cumulative incidence (thousands)", main='All ages cumulative incidence')
# plot(cumulvx[1:13,9],xlab="Run", ylab="Cumulative incidence (thousands)",main='0-14yrs cumulative incidence')
# plot(cumulvx[1:13,10],xlab="Run", ylab="Cumulative incidence (thousands)",main='15-54yrs cumulative incidence')
# plot(cumulvx[1:13,11],xlab="Run", ylab="Cumulative incidence (thousands)",main='55-64yrs cumulative incidence')
# plot(cumulvx[1:13,12],xlab="Run", ylab="Cumulative incidence (thousands)",main='65+ yrs cumulative incidence')
# 
# redu<-1000*rbind((cumulvx[1,]-cumulvx[2,]),(cumulvx[1,]-cumulvx[3,]),(cumulvx[1,]-cumulvx[4,]),(cumulvx[1,]-cumulvx[5,]),(cumulvx[1,]-cumulvx[6,]),(cumulvx[1,]-cumulvx[7,]),(cumulvx[1,]-cumulvx[8,]),(cumulvx[1,]-cumulvx[9,]),(cumulvx[1,]-cumulvx[10,]),(cumulvx[1,]-cumulvx[11,]),(cumulvx[1,]-cumulvx[12,]),(cumulvx[1,]-cumulvx[13,]))
# plot(redu[1:12,1],xlab="Run", ylab="Reduction in cumulative mortality", main='All ages reduction in cumulative mortality')
# plot(redu[1:12,2],xlab="Run", ylab="Reduction in cumulative mortality", main='0-14yrs reduction in cumulative mortality')
# plot(redu[1:12,3],xlab="Run", ylab="Reduction in cumulative mortality", main='15-54yrs reduction in cumulative mortality')
# plot(redu[1:12,4],xlab="Run", ylab="Reduction in cumulative mortality", main='55-64yrs reduction in cumulative mortality')
# plot(redu[1:12,5],xlab="Run", ylab="Reduction in cumulative mortality", main='65+ yrs reduction in cumulative mortality')
# 
# plot(redu[1:12,8],xlab="Run", ylab="Reduction in cumulative incidence", main='all ages reduction in cumulative incidence')
# plot(redu[1:12,9],xlab="Run", ylab="Reduction in cumulative incidence", main='0-14yrs reduction in cumulative incidence')
# plot(redu[1:12,10],xlab="Run", ylab="Reduction in cumulative incidence", main='15-54yrs reduction in cumulative incidence')
# plot(redu[1:12,11],xlab="Run", ylab="Reduction in cumulative incidence", main='55-64yrs reduction in cumulative incidence')
# plot(redu[1:12,12],xlab="Run", ylab="Reduction in cumulative incidence", main='65+yrs reduction in cumulative incidence')
# 

## barcharts for reduction in disease by age and overall
## used excel in report!

# plots<-"/Users/Rebecca/Vaccine_china/Plots"
# setwd(plots)
