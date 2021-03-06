##### Plotting TBPI by age every 10 yrs

agegrps=c("5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")

tTBPI<-t(TBPI)

setwd("/Users/Rebecca/Vaccine_china/Data")
gaodata<-suppressWarnings(as.matrix(read.csv('gaoLTBI.csv',header=TRUE,check.names=F, row.names=1)))
gaodata<-t(gaodata)
gaomodel<-cbind(gaodata,tTBPI[7:14,114])
colnames(gaomodel, do.NULL=FALSE, prefix="col")
colnames(gaomodel)<-c("QFN", "TST", "Model")
rownames(gaomodel)<-c("5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")
gaomodel

### comparison to Gao
par(mfrow=c(1,1))
ages<-c("5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")
plot(seq(1,8), gaomodel[,1], xaxt = "n", xlab="Age group(years)", ylab="LTBI Prevalence (%)",ylim=c(0,80),type='l', col=4)
axis(1, at=1:8, labels=ages)
points(seq(1,8), gaomodel[,2], type='l', col=3)
points(seq(1,8), gaomodel[,3], type='l', col=2)
legend(c(70,70), c("Model", "TST", "QFN"), lty=1,col=c(2,3,4), border="white")





##plots of TBPI by age and every 10 years
# 
# par(mfrow=c(2,3))
# plot(seq(1,8),tTBPI[7:14,101], ylab="Prevalence of Infection 2000",xlab="Age group(years)", ylim=c(0,100),type='l',col='red')
# 
# plot(seq(1,8),tTBPI[7:14,111], ylab="Prevalence of Infection 2010",xlab="Age group(years)", ylim=c(0,100),type='l',col='orange')
# 
# plot(seq(1,8),tTBPI[7:14,121], ylab="Prevalence of Infection 2020",xlab="Age group(years)", ylim=c(0,100),type='l',col='yellow')
# 
# plot(seq(1,8),tTBPI[7:14,131], ylab="Prevalence of Infection 2030",xlab="Age group(years)", ylim=c(0,100),type='l',col='green')
# 
# plot(seq(1,8),tTBPI[7:14,141], ylab="Prevalence of Infection 2040",xlab="Age group(years)", ylim=c(0,100),type='l',col='blue')
# 
# plot(seq(1,8),tTBPI[7:14,151], ylab="Prevalence of Infection 2050",xlab="Age group(years)", ylim=c(0,100),type='l',col='purple')
# 



