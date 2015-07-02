#### NNV ####

setwd("Outputs")

###cases averted ###

#select appropriate active disease columns. typen -1 as not using infant scenario
activecols<-seq(4,(((typen-1)*count*3)+1),3)
#calc cases averted
CAV<-matrix(0,(yearend-year1+1),((typen-1)*count))
CAV<-new_active[,1]-new_active[,(activecols)]
vacnames<-c("O_pre_c30_v40","O_pre_c30_v60","O_pre_c30_v80","O_pre_70_v40","O_pre_c70_v60","O_pre_c70_v80","O_L_c30_v40","O_L_c30_v60","O_L_c30_v80","O_L_70_v40","O_L_c70_v60","O_L_c70_v80","O_Mix_c30_v40","O_Mix_c30_v60","O_Mix_c30_v80","O_Mix_70_v40","O_Mix_c70_v60","O_Mix_c70_v80","Y_pre_c30_v40","Y_pre_c30_v60","Y_pre_c30_v80","Y_pre_70_v40","Y_pre_c70_v60","Y_pre_c70_v80","Y_L_c30_v40","Y_L_c30_v60","Y_L_c30_v80","Y_L_70_v40","Y_L_c70_v60","Y_L_c70_v80","Y_Mix_c30_v40","Y_Mix_c30_v60","Y_Mix_c30_v80","Y_Mix_70_v40","Y_Mix_c70_v60","Y_Mix_c70_v80")
colnames(CAV)<-vacnames
cavnames<-c("2,1","2,2","2,3","2,4","2,5","2,6","3,1","3,2","3,3","3,4","3,5","3,6","4,1","4,2","4,3","4,4","4,5","4,6","5,1","5,2","5,3","5,4","5,5","5,6","6,1","6,2","6,3","6,4","6,5","6,6","7,1","7,2","7,3","7,4","7,5","7,6")
write.table(CAV,'annual_cases_averted.csv',sep=",",row.names=FALSE)

#calc NNVc per yr
numvcols<-seq(1,((typen-1)*count*3),3)
NNVc<-NumV[,(numvcols)]/CAV
colnames(NNVc)<-vacnames
write.table(NNVc,'annual_NNV_case.csv',sep=",",row.names=FALSE)
NNVc

#calc NNVC for period 2025-2050
NNVcp<-matrix(0,1,((typen-1)*count))
CAV2550<-sum(new_active[126:151,1])-colSums(new_active[126:151,(activecols)])
doses<- colSums(NumV[,(numvcols)])
NNVcp[,1:((typen-1)*count)]<-doses/CAV2550
colnames(NNVcp)<-vacnames
write.table(NNVcp,'period_NNV_case.csv',sep=",",row.names=FALSE)
NNVcp


#by age
sum_ac_age<-colSums(new_ac_age[126:151,])
ACAV<-sum_ac_age[1]-sum_ac_age[-1]
ACAV
write.table(ACAV,'Age_cases_averted.csv',sep=",",row.names=FALSE)




casesav<-colSums(CAV)
chart<-rbind(doses,casesav)
colnames(chart)<-vacnames
chart
write.table(chart,'chart.csv',sep=",",row.names=FALSE)



### Deaths averted ###

#calc deaths averted
DAV<-matrix(0,(yearend-year1+1),((typen-1)*count))
DAV<-new_mort[,1]-new_mort[,(activecols)]
colnames(DAV)<-vacnames
write.table(DAV,'annual_deaths_averted.csv',sep=",",row.names=FALSE)
deathsav<-colSums(DAV)

#calc NNVd per yr
NNVd<-NumV[,(numvcols)]/DAV
colnames(NNVd)<-vacnames
write.table(NNVd,'annual_NNV_death.csv',sep=",",row.names=FALSE)
NNVd

#calc NNVC for period 2025-2050
NNVdp<-matrix(0,1,((typen-1)*count))
NNVdp[,1:((typen-1)*count)]<-colSums(NumV[,(numvcols)])/colSums(DAV)
colnames(NNVdp)<-vacnames
write.table(NNVdp,'period_NNV_death.csv',sep=",",row.names=FALSE)
NNVdp


##### PLOTS #####

# setwd("Plots")

### plot NNVc 30% cov 40% VE
par(mfcol=c(2,2))
plot(seq(2025,2050,1),NNVc[126:151,1], ylab="Annual NNV per case averted", ylim=c(0,10000), xlab="Year", main="Annual NNV per case averted by vaccine type (all 30% cov, 40%ve)", type="l",col=1)
lines(seq(2025,2050,1), NNVc[126:151,7], lty=1, col=2)
lines(seq(2025,2050,1), NNVc[126:151,13], lty=1, col=3)
lines(seq(2025,2050,1), NNVc[126:151,19], lty=1, col=7)
lines(seq(2025,2050,1), NNVc[126:151,25], lty=1, col=5)
lines(seq(2025,2050,1), NNVc[126:151,31], lty=1, col="orange")
plot.new()
legend("center",c("Old_Pre","Old_Latent","Old_Mixed","Young_Pre","Young_Latent","Young_Mixed"), lty=c(1,1,1,1,1,1),col=c(1,2,3,7,5,"orange"))

### plot NNVc 70% cov 80% VE
plot(seq(2025,2050,1),NNVc[126:151,6], ylab="Annual NNV per case averted", xlab="Year", ylim=c(0,10000), main="Annual NNV per case averted by vaccine type (all 70% cov, 80%ve)", type="l",col=1)
lines(seq(2025,2050,1), NNVc[126:151,12], lty=1, col=2)
lines(seq(2025,2050,1), NNVc[126:151,18], lty=1, col=3)
lines(seq(2025,2050,1), NNVc[126:151,24], lty=1, col=7)
lines(seq(2025,2050,1), NNVc[126:151,30], lty=1, col=5)
lines(seq(2025,2050,1), NNVc[126:151,36], lty=1, col="orange")


par(mfcol=c(1,2))
##plot comparing VE and coverage - elderly mixed
plot(seq(2025,2050,1),NNVc[126:151,13], ylab="Annual NNV per case averted", xlab="Year", ylim=c(0,200), main="Annual NNV per case averted - Older adults, Mixed effect", type="l",col=1)
lines(seq(2025,2050,1), NNVc[126:151,14], lty=1, col=2)
lines(seq(2025,2050,1), NNVc[126:151,15], lty=1, col=3)
lines(seq(2025,2050,1), NNVc[126:151,16], lty=1, col=7)
lines(seq(2025,2050,1), NNVc[126:151,17], lty=1, col=5)
lines(seq(2025,2050,1), NNVc[126:151,18], lty=1, col="orange")
plot.new()
legend("center",c("40%VE, 30%coverage","60%VE, 30%coverage","80%VE, 30%coverage","40%VE, 70%coverage","60%VE, 70%coverage","80%VE, 70%coverage"), lty=c(1,1,1,1,1,1),col=c(1,2,3,7,5,"orange"))

# par(mfcol=c(1,1))
# ##plot comparing VE and coverage - elderly mixed
# plot(seq(2025,2028,1),NNVc[126:129,13], ylab="Annual NNV per case averted", xlab="Year", main="Annual NNV per case averted - Older adults, Mixed effect", type="l",col=1)
# lines(seq(2025,2028,1), NNVc[126:129,14], lty=1, col=2)
# lines(seq(2025,2028,1), NNVc[126:129,15], lty=1, col=3)
# lines(seq(2025,2028,1), NNVc[126:129,16], lty=1, col=7)
# lines(seq(2025,2028,1), NNVc[126:129,17], lty=1, col=5)
# lines(seq(2025,2028,1), NNVc[126:129,18], lty=1, col="orange")


# par(mfcol=c(1,1))
# ##plot comparing VE and coverage - elderly mixed
# plot(seq(2027,2035,1),NNVc[135:143,13], ylab="Annual NNV per case averted", xlab="Year",ylim=c(0,200), main="Annual NNV per case averted - Older adults, Mixed effect", type="l",col=1)
# lines(seq(2027,2035,1), NNVc[135:143,14], lty=1, col=2)
# lines(seq(2027,2035,1), NNVc[135:143,15], lty=1, col=3)
# lines(seq(2027,2035,1), NNVc[135:143,16], lty=1, col=7)
# lines(seq(2027,2035,1), NNVc[135:143,17], lty=1, col=5)
# lines(seq(2027,2035,1), NNVc[135:143,18], lty=1, col="orange")


par(mfcol=c(1,2))
##plot comparing VE and coverage - ado mixed
plot(seq(2025,2050,1),NNVc[126:151,31], ylab="Annual NNV per case averted", xlab="Year", ylim=c(0,600), main="Annual NNV per case averted - Ados, Mixed effect", type="l",col=1)
lines(seq(2025,2050,1), NNVc[126:151,32], lty=1, col=2)
lines(seq(2025,2050,1), NNVc[126:151,33], lty=1, col=3)
lines(seq(2025,2050,1), NNVc[126:151,34], lty=1, col=7)
lines(seq(2025,2050,1), NNVc[126:151,35], lty=1, col=5)
lines(seq(2025,2050,1), NNVc[126:151,36], lty=1, col="orange")
plot.new()
legend("center",c("40%VE, 30%coverage","60%VE, 30%coverage","80%VE, 30%coverage","40%VE, 70%coverage","60%VE, 70%coverage","80%VE, 70%coverage"), lty=c(1,1,1,1,1,1),col=c(1,2,3,7,5,"orange"))



## NNVcp plot
par(mfcol=c(1,1))
barplot(NNVcp[1:length(NNVcp)], col=c(1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange"))






#### death plots

par(mfcol=c(1,2))
### plot NNVd 70% cov 80% VE
plot(seq(2025,2050,1),NNVd[126:151,6], ylab="Annual NNV per death averted", xlab="Year", ylim=c(0,25000), main="Annual NNV per death averted by vaccine type (all 70% cov, 80%ve)", type="l",col=1)
lines(seq(2025,2050,1), NNVd[126:151,12], lty=1, col=2)
lines(seq(2025,2050,1), NNVd[126:151,18], lty=1, col=3)
lines(seq(2025,2050,1), NNVd[126:151,24], lty=1, col=7)
lines(seq(2025,2050,1), NNVd[126:151,30], lty=1, col=5)
lines(seq(2025,2050,1), NNVd[126:151,36], lty=1, col="orange")
plot.new()
legend("center",c("Old_Pre","Old_Latent","Old_Mixed","Young_Pre","Young_Latent","Young_Mixed"), lty=c(1,1,1,1,1,1),col=c(1,2,3,7,5,"orange"))



par(mfcol=c(1,2))
##plot comparing VE and coverage - elderly mixed
plot(seq(2025,2050,1),NNVd[126:151,13], ylab="Annual NNV per death averted", xlab="Year", ylim=c(0,4000), main="Annual NNV per death averted - Older adults, Mixed effect", type="l",col=1)
lines(seq(2025,2050,1), NNVd[126:151,14], lty=1, col=2)
lines(seq(2025,2050,1), NNVd[126:151,15], lty=1, col=3)
lines(seq(2025,2050,1), NNVd[126:151,16], lty=1, col=7)
lines(seq(2025,2050,1), NNVd[126:151,17], lty=1, col=5)
lines(seq(2025,2050,1), NNVd[126:151,18], lty=1, col="orange")
plot.new()
legend("center",c("40%VE, 30%coverage","60%VE, 30%coverage","80%VE, 30%coverage","40%VE, 70%coverage","60%VE, 70%coverage","80%VE, 70%coverage"), lty=c(1,1,1,1,1,1),col=c(1,2,3,7,5,"orange"))


## NNVdp plot
par(mfcol=c(1,1))
barplot(NNVdp[1:length(NNVdp)], col=c(1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange",1,2,3,7,5,"orange"))


setwd(home)