## To plot all outputs

time<-seq(year1,yearend,dt)
library("plotrix", lib.loc="/Library/Frameworks/R.framework/Versions/2.15/Resources/library")

# Remove first row - blank
# ADeaths<-ADeaths[-1,]; ADeathsH<-ADeathsH[-1,];
# TBDeaths<-TBDeaths[-1,];TBDeathsH<-TBDeathsH[-1,]
# VX<-VX[-1,];Deaths<-Deaths[-1,];
# TBRx<-TBRx[-1,];TBI<-TBI[-1,];TBM<-TBM[-1,];

plot(time,TBI[,1],type='l')
lines(time,TBI[,2],col='red')
title(main="TBI")
plot(time,TBM[,1],type='l')
lines(time,TBM[,2],col='red')
title(main="TBM")
## Sums over ages
total<-matrix(0,(1/dt)*(yearend-year1)+1,21);

total[,1]<-rowSums(S);total[,2]<-rowSums(L);total[,3]<-rowSums(R);total[,4]<-rowSums(I);total[,5]<-rowSums(NI);
total[,6]<-rowSums(SH);total[,7]<-rowSums(LH);total[,8]<-rowSums(RH);total[,9]<-rowSums(IH);total[,10]<-rowSums(NIH);
total[,11]<-rowSums(Sv);total[,12]<-rowSums(Lv);total[,13]<-rowSums(Rv);
total[,14]<-rowSums(SvH);total[,15]<-rowSums(LvH);total[,16]<-rowSums(RvH);
total[,17]<-rowSums(new_I);total[,18]<-rowSums(new_NI);
total[,19]<-rowSums(new_IH);total[,20]<-rowSums(new_NIH);
total[,21]<-lambda

total<-cbind(times,total)
colnames(total)<-c("Time","S","L","R","I","NI","SH","LH","RH","IH","NIH","Sv","Lv","Rv","SvH","LvH","RvH","new_I","new_NI","new_IH","new_NIH","lambda")

plot(total[,"S"])
plot(total[,"L"])
plot(total[,"R"])
plot(total[,"new_I"])
plot(total[,"new_NI"])

plot(total[,"SH"],type='l')
lines(total[,"LH"],col='red')
lines(total[,"RH"],col='blue')
lines(total[,"new_IH"],col='green')
lines(total[,"new_NIH"],col='cyan')
legend("right", c("SH","LH","RH","new_IH","new_NIH"), col = c("black","red","blue","green","cyan"), lwd = 2)



# DEMOG ••••••••••••••••••••••••••••••
yrs<-seq(1,1+(1/dt)*(yearend-year1),(1/dt))
# Births
if(year1<2009){
  plot(seq(year1,yearend,1),c(bv,bb),ylim=c(0,max(bb,bv)))  
} else {
  plot(seq(year1,yearend,1),bb[(year1-2008):(yearend-2008)],ylim=c(0,max(bb,bv)))  
}
title(c(cntry,"Births"))
plot(seq(year1,yearend,1),S[yrs,1])

# Age over time
#yrstart<-year1 ## Vary +x so can see how changes over the year
if(year1<2009){
  plot(seq(year1,yearend,1),c(bv,bb),ylab="Number of people",xlab="year",ylim=c(0,max(bb)),xlim=c(year1,2050))  
} else {
  plot(seq(year1,yearend,1),bb[(year1-2008):(yearend-2008)],ylab="Number of people",xlab="year",ylim=c(0,max(bb)),xlim=c(year1,2050))
}
for (i in 1:Mnage){
lines(seq(year1,yearend,1),S[yrs,i])
}
title(c(cntry,"Age all"))


# All times points
if(year1<2009){
  plot(seq(1,dim(S)[1],1),SH[,1],ylab="Number of people",xlab="year",ylim=c(0,max(S)))  
} else {
  plot(seq(1,dim(S)[1],1),SH[,1],ylab="Number of people",xlab="year",ylim=c(0,max(S)))
}
for (i in 1:Mnage){
  lines(seq(1,dim(S)[1],1),SH[,i])
}
title(c(cntry,"Age all timepints"))



# Population size
# Data
Ana<-matrix(0,1,4)
Ana[1:2]<-Popsize[1:2,cntry]
Ana[3]<-as.numeric(TBIm[cntry])
Ana[4]<-as.numeric(TBMm[cntry])
plot(c(2009,2050),Ana[1:2],xlim=c(year1,yearend),ylim=c(0,max(psize)))
# Output
lines(seq(year1,yearend,1),psize[yrs],ylab="Number of people",xlab="year")
# title vs data
title(c(cntry,"Output vs Data"))
plot(seq(1,length(psize),1),psize,ylim=c(0,max(Ana)),type="l")
points(c((1/dt)*(2009-year1),(1/dt)*(yearend-year1)),Ana[1:2],pch=2,col='red')


# Deaths
plot(seq(year1,yearend,1),Deaths[yrs,2],xlab="year",ylab="Average age of death",ylim=c(0,85))
title(c(cntry,"Av Age death"))
# Mortality rate
plot(seq(1,Mnage,1),(1-0.99)*mort[1,c(-1)],ylab="Mortality rate",xlab="Age")
for (i in 1:(yearend-2009)){
lines(seq(1,Mnage,1),(1-0.99)*mort[i,c(-1)])
}
title(c(cntry,"Mortality rate"))


# Age structure
par(mar=pyramid.plot(100*((S[1,])/sum(S[1,])),100*((S[(yearend-year1)*1/dt+1,])/sum(S[(yearend-year1)*1/dt+1,])),top.labels=c("Initially","Age","End"),main=c("Output",cntry)))
ps5<-1000*pstruc50[2:87,cntry]
par(mar=pyramid.plot(100*((ps)/sum(ps)),100*((ps5)/sum(ps5)),top.labels=c("Initially","Age","End"),main=c("Data",cntry)))

if (year1<2009){
  par(mar=pyramid.plot(100*((S[(2009-year1)*1/dt+1,]+SH[(2009-year1)*1/dt+1,])/sum(S[(2009-year1)*1/dt+1,],SH[(2009-year1)*1/dt+1,])),100*((S[(yearend-year1)*1/dt+1,]+SH[(yearend-year1)*1/dt+1,])/sum(S[(yearend-year1)*1/dt+1,],SH[(yearend-year1)*1/dt+1,])),top.labels=c("2009","Age","End"),main=c("Output",cntry)))}
if (year1<2009){
par(mar=pyramid.plot(100*((S[(2009-year1)*1/dt+1,]+SH[(2009-year1)*1/dt+1,])/sum(S[(2009-year1)*1/dt+1,],SH[(2009-year1)*1/dt+1,])),100*((ps)/sum(ps)),top.labels=c("Output","Age","data"),main=c("2009",cntry)))}
par(mar=pyramid.plot(100*((S[(yearend-year1)*1/dt+1,]+SH[(yearend-year1)*1/dt+1,])/sum(S[(yearend-year1)*1/dt+1,],SH[(yearend-year1)*1/dt+1,])),100*((ps5)/sum(ps5)),top.labels=c("Output","Age","data"),main=c("2050",cntry)))

plot(seq(1,Mnage,1),ps,ylab="Number of Susceptibles people",xlab="Age",ylim=c(0,max(S[(yearend-year1)*1/dt+1,])))
lines(seq(1,Mnage,1),S[1,])
lines(seq(1,Mnage,1),S[50,],col='blue')
lines(seq(1,Mnage,1),S[(yearend-year1)*1/dt+1,],col='red')
legend("right", c("Initial","Timepoint 50","End"), col = c("black","blue","red"), lwd = 2)
title(c(cntry,"Susceptibles"))

for (i in 1:Mnage){
  lines(seq(1,Mnage,1),S[i,])
}


# EPI 
plot(time,total[,1],type='l')
plot(time,total[,'R'],col='red',type='l',ylab="subpop numbers of people",ylim=c(0,max(total[,'R'],total[,'I'],total[,'NI'])))
#lines(time,total[,'L'],col='blue')
lines(time,total[,'I'],col='cyan')
lines(time,total[,'NI'],col='green')
legend("right", c("R","L","I","NI"), col = c("red", "blue","cyan","green"), lwd = 2)

plot(time,total[,'new_I'],col='red',ylab="subpop numbers of people",type='l',ylim=c(0,max(total[,'new_I'],total[,'new_NI'])))
lines(time,total[,'new_NI'],col='blue')
legend("right", c("New Inf","New Non-Inf"), col = c("red", "blue"), lwd = 2)

plot(time,lambda)

# Proportions
prop<-matrix(0,length(total[,'S']),5)

prop[,1]<-total[,'S']/psize;prop[,2]<-total[,'I']/psize;prop[,3]<-total[,'R']/psize
prop[,4]<-total[,'NI']/psize;prop[,5]<-total[,'L']/psize;
colnames(prop)<-c("S","I","R","NI","L")

plot(times,prop[,"S"],type='l',ylim=c(0,max(prop[,"S"],prop[,"L"])));
lines(times,prop[,"L"],col='blue')
legend("topright", c("S","L"), col = c("black", "blue"), lwd = 2)
plot(times,prop[,"I"],col='red',type='l')
lines(times,prop[,"NI"],col='cyan')
lines(times,prop[,"R"],col='green')
legend("topright", c("I","NI","R"), col = c("red", "cyan","green"), lwd = 2)
#plot(rowSums(prop),type='l')