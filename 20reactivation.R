#load grid package

temp2<-as.data.frame(temp2)
vpl4<-ggplot(data=temp2,aes(x=yrs))+geom_line(colour="black",size=1.5,aes(y=m))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,100)+
  ylab("Proportion of all new cases (%)")+
  geom_ribbon(aes(ymin=l, ymax=u),alpha=0.3,colour=NA)+
  geom_line(colour="black",linetype=2,size=1.5,aes(y=mi))+
  geom_ribbon(aes(ymin=li, ymax=ui),alpha=0.3,colour=NA)+
  theme_bw()+
  theme(legend.position="none")
vpl4

temp3<-as.data.frame(temp3)
vpl5<-ggplot(data=temp3,aes(x=yrs))+geom_line(colour="red",size=1.5,aes(y=m))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,100)+
  ylab("Proportion of all new cases (%)")+
  geom_ribbon(aes(ymin=l, ymax=u, fill="red"),alpha=0.3,colour=NA)+
  geom_line(colour="red",linetype=2,size=1.5,aes(y=mi))+
  geom_ribbon(aes(ymin=li, ymax=ui, fill="red"),alpha=0.3,colour=NA)+
  theme_bw()+
  theme(legend.position="none")
vpl5

temp4<-as.data.frame(temp4)
vpl6<-ggplot(data=temp4,aes(x=yrs))+geom_line(colour="orange",size=1.5,aes(y=m))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,100)+
  ylab("Proportion of all new cases (%)")+
  geom_ribbon(aes(ymin=l, ymax=u, fill="orange"),alpha=0.3,colour=NA)+
  geom_line(colour="orange",linetype=2,size=1.5,aes(y=mi))+
  geom_ribbon(aes(ymin=li, ymax=ui, fill="orange"),alpha=0.3,colour=NA)+
  theme_bw()+
  theme(legend.position="none")
vpl6

temp5<-as.data.frame(temp5)
vpl7<-ggplot(data=temp5,aes(x=yrs))+geom_line(colour="green",size=1.5,aes(y=m))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,100)+
  ylab("Proportion of all new cases (%)")+
  geom_ribbon(aes(ymin=l, ymax=u, fill="green"),alpha=0.3,colour=NA)+
  geom_line(colour="green",linetype=2,size=1.5,aes(y=mi))+
  geom_ribbon(aes(ymin=li, ymax=ui, fill="green"),alpha=0.3,colour=NA)+
  theme_bw()+
  theme(legend.position="none")
vpl7

temp6<-as.data.frame(temp6)
vpl8<-ggplot(data=temp6,aes(x=yrs))+geom_line(colour="blue",size=1.5,aes(y=m))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,100)+
  ylab("Proportion of all new cases (%)")+
  geom_ribbon(aes(ymin=l, ymax=u, fill="blue"),alpha=0.3,colour=NA)+
  geom_line(colour="blue",linetype=2,size=1.5,aes(y=mi))+
  geom_ribbon(aes(ymin=li, ymax=ui, fill="blue"),alpha=0.3,colour=NA)+
  theme_bw()+
  theme(legend.position="none")
vpl8

multiplot(vpl4,vpl5,vpl6,vpl7,vpl8,cols=3)

par(mfrow=c(2,3))
plot(seq(2000,2050),temp2[91:151,1], ylab="Proportion of all new cases (%)",xlab="Year", ylim=c(0,100),type='l',col='orange')
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



### tbri and tbra

dfvx_0_r<-dfvx4[which(dfvx4[,"type"]%in%0),c("yrs","TBRatot","TBRa0-14","TBRa15-54","TBRa55-64","TBRa65+")]                                        
dfvx_0_r<-dfvx_0_r[which(dfvx_0_r[,"yrs"]%in%fityrs),]

#total  
temp2<-matrix(0,51,6)

dfvx_tot<-dfvx_0_r$TBRatot

dim(dfvx_tot)<-c((length(fityrs)),20)

for(i in 1:length(fityrs)){                                           
    temp2[i,]<-c(median(dfvx_tot[i,]),min(dfvx_tot[i,]),max(dfvx_tot[i,]),(100-median(dfvx_tot[i,])),(100-min(dfvx_tot[i,])),(100-max(dfvx_tot[i,])))   
  }
temp2<-cbind(fityrs,temp2)
colnames(temp2)<-c("yrs","m","l","u","mi","li","ui")

#kids
temp3<-matrix(0,51,6)

dfvx_tot<-dfvx_0_r$"TBRa0-14"

dim(dfvx_tot)<-c((length(fityrs)),20)

for(i in 1:length(fityrs)){                                           
  temp3[i,]<-c(median(dfvx_tot[i,]),min(dfvx_tot[i,]),max(dfvx_tot[i,]),(100-median(dfvx_tot[i,])),(100-min(dfvx_tot[i,])),(100-max(dfvx_tot[i,])))   
}
temp3<-cbind(fityrs,temp3)
colnames(temp3)<-c("yrs","m","l","u","mi","li","ui")


#adult
temp4<-matrix(0,51,6)

dfvx_tot<-dfvx_0_r$"TBRa15-54"

dim(dfvx_tot)<-c((length(fityrs)),20)

for(i in 1:length(fityrs)){                                           
  temp4[i,]<-c(median(dfvx_tot[i,]),min(dfvx_tot[i,]),max(dfvx_tot[i,]),(100-median(dfvx_tot[i,])),(100-min(dfvx_tot[i,])),(100-max(dfvx_tot[i,])))   
}
temp4<-cbind(fityrs,temp4)
colnames(temp4)<-c("yrs","m","l","u","mi","li","ui")



#older adult
temp5<-matrix(0,51,6)

dfvx_tot<-dfvx_0_r$"TBRa55-64"

dim(dfvx_tot)<-c((length(fityrs)),20)

for(i in 1:length(fityrs)){                                           
  temp5[i,]<-c(median(dfvx_tot[i,]),min(dfvx_tot[i,]),max(dfvx_tot[i,]),(100-median(dfvx_tot[i,])),(100-min(dfvx_tot[i,])),(100-max(dfvx_tot[i,])))   
}
temp5<-cbind(fityrs,temp5)
colnames(temp5)<-c("yrs","m","l","u","mi","li","ui")



#elderly
temp6<-matrix(0,51,6)

dfvx_tot<-dfvx_0_r$"TBRa65+"

dim(dfvx_tot)<-c((length(fityrs)),20)

for(i in 1:length(fityrs)){                                           
  temp6[i,]<-c(median(dfvx_tot[i,]),min(dfvx_tot[i,]),max(dfvx_tot[i,]),(100-median(dfvx_tot[i,])),(100-min(dfvx_tot[i,])),(100-max(dfvx_tot[i,])))   
}
temp6<-cbind(fityrs,temp6)
colnames(temp6)<-c("yrs","m","l","u","mi","li","ui")


