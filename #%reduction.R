#### Calculate % reduction in I and M in 2050 ####

#set up how to find first row of each run
#typen is -1 as dont use the first one at the moment
lastcol<-(((typen-1)*(combn))+1)*steps
y1<-seq(1,lastcol,steps)

#calc the difference between baseline and 2050 result for each vaccine, and calc percentage reduction. y1 is minus 1 to get rid of the baseline number
vacnames<-c("O_pre_c30_v40","O_pre_c30_v60","O_pre_c30_v80","O_pre_70_v40","O_pre_c70_v60","O_pre_c70_v80","O_L_c30_v40","O_L_c30_v60","O_L_c30_v80","O_L_70_v40","O_L_c70_v60","O_L_c70_v80","O_Mix_c30_v40","O_Mix_c30_v60","O_Mix_c30_v80","O_Mix_70_v40","O_Mix_c70_v60","O_Mix_c70_v80","Y_pre_c30_v40","Y_pre_c30_v60","Y_pre_c30_v80","Y_pre_70_v40","Y_pre_c70_v60","Y_pre_c70_v80","Y_L_c30_v40","Y_L_c30_v60","Y_L_c30_v80","Y_L_70_v40","Y_L_c70_v60","Y_L_c70_v80","Y_Mix_c30_v40","Y_Mix_c30_v60","Y_Mix_c30_v80","Y_Mix_70_v40","Y_Mix_c70_v60","Y_Mix_c70_v80")
#pcrednames=c("30%cov, 40%VE", "30%cov, 60%VE","30%cov, 80%VE","70%cov, 40%VE","70%cov, 60%VE","70%cov, 80%VE","30%cov, 40%VE", "30%cov, 60%VE","30%cov, 80%VE","70%cov, 40%VE","70%cov, 60%VE","70%cov, 80%VE", "30%cov, 40%VE", "30%cov, 60%VE","30%cov, 80%VE","70%cov, 40%VE","70%cov, 60%VE","70%cov, 80%VE","30%cov, 40%VE", "30%cov, 60%VE","30%cov, 80%VE","70%cov, 40%VE","70%cov, 60%VE","70%cov, 80%VE","30%cov, 40%VE", "30%cov, 60%VE","30%cov, 80%VE","70%cov, 40%VE","70%cov, 60%VE","70%cov, 80%VE","30%cov, 40%VE", "30%cov, 60%VE","30%cov, 80%VE","70%cov, 40%VE","70%cov, 60%VE","70%cov, 80%VE")
pcredrow=c("All","0-14years","15-54years","55-64years","≥65years", "<55 years","55+ years","15-24years","25-54years")
#pcredrowM=c("All","0-14years","15-59years","≥60years")
agecols<-c("black","red","blue","purple","green")
pcreduI[1,]<-100*((dfvx[151,15]-dfvx[(y1[-1]+150),15]))/(dfvx[151,15])
pcreduI[2,]<-100*((dfvx[151,16]-dfvx[(y1[-1]+150),16]))/(dfvx[151,16])
pcreduI[3,]<-100*((dfvx[151,17]-dfvx[(y1[-1]+150),17]))/(dfvx[151,17])
pcreduI[4,]<-100*((dfvx[151,18]-dfvx[(y1[-1]+150),18]))/(dfvx[151,18])
pcreduI[5,]<-100*((dfvx[151,19]-dfvx[(y1[-1]+150),19]))/(dfvx[151,19])
pcreduI[6,]<-100*((dfvx[151,21]-dfvx[(y1[-1]+150),21]))/(dfvx[151,21])
pcreduI[7,]<-100*((dfvx[151,20]-dfvx[(y1[-1]+150),20]))/(dfvx[151,20])
pcreduI[8,]<-100*((dfvx[151,89]-dfvx[(y1[-1]+150),89]))/(dfvx[151,89])
pcreduI[9,]<-100*((dfvx[151,90]-dfvx[(y1[-1]+150),90]))/(dfvx[151,90])


colnames(pcreduI)<-vacnames
rownames(pcreduI)<-pcredrow

###chk numberd
pcreduM[1,]<-100*((dfvx[151,29]-dfvx[(y1[-1]+150),29]))/(dfvx[151,29])
pcreduM[2,]<-100*((dfvx[151,30]-dfvx[(y1[-1]+150),30]))/(dfvx[151,30])
pcreduM[3,]<-100*((dfvx[151,31]-dfvx[(y1[-1]+150),31]))/(dfvx[151,31])
pcreduM[4,]<-100*((dfvx[151,32]-dfvx[(y1[-1]+150),32]))/(dfvx[151,32])
pcreduM[5,]<-100*((dfvx[151,33]-dfvx[(y1[-1]+150),33]))/(dfvx[151,33])
pcreduM[6,]<-100*((dfvx[151,37]-dfvx[(y1[-1]+150),37]))/(dfvx[151,37])
pcreduM[7,]<-100*((dfvx[151,36]-dfvx[(y1[-1]+150),36]))/(dfvx[151,36])
pcreduM[8,]<-100*((dfvx[151,91]-dfvx[(y1[-1]+150),91]))/(dfvx[151,91])
pcreduM[9,]<-100*((dfvx[151,92]-dfvx[(y1[-1]+150),92]))/(dfvx[151,92])

colnames(pcreduM)<-vacnames
rownames(pcreduM)<-pcredrow

setwd("Outputs")
write.table(pcreduI,'2050_reduction_incidence.csv',sep=",",row.names=FALSE)
write.table(pcreduM,'2050_reduction_mortality.csv',sep=",",row.names=FALSE)
setwd(home)

# #plots
# par(mfcol=c(1,1))
# barplot(pcreduI[,1:6], col=agecols, ylim=c(0,50), beside=TRUE, legend.text=TRUE)
# barplot(pcreduI[,7:12], legend.text=TRUE, col=agecols, beside=TRUE)
par(mfcol=c(1,1))
vaxname<-c("pre-elderly","latent-elderly","mixed-elderly","pre-ado","latent-ado","mixed-ado")
barplot(pcreduI[1,(seq(6,36,6))], space=0.5, names.arg=(vaxname), col=c(3,3,3,4,4,4), ylim=c(0,30), xlab='Vaccine type', ylab='% reduction in incidence rate in 2050 vs baseline', axis.lty=1)

