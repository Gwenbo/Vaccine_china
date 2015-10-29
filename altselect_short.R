### Faster method of doing alternative selection process ###

#call in libraries
library(plyr)

#reformat data

mydf2 <- cbind(xoutplot[xoutplot[,"year"]%in%2000,"run_count"],xoutplot[xoutplot[,"year"]%in%2000,"TBPb15+"],xoutplot[xoutplot[,"year"]%in%2010,"TBPb15+"],
               xoutplot[xoutplot[,"year"]%in%2000,"TBPb15-29"],xoutplot[xoutplot[,"year"]%in%2010,"TBPb15-29"],
               xoutplot[xoutplot[,"year"]%in%2000,"TBPb30-44"],xoutplot[xoutplot[,"year"]%in%2010,"TBPb30-44"],
               xoutplot[xoutplot[,"year"]%in%2000,"TBPb45-59"],xoutplot[xoutplot[,"year"]%in%2010,"TBPb45-59"],
               xoutplot[xoutplot[,"year"]%in%2000,"TBPb60+"],xoutplot[xoutplot[,"year"]%in%2010,"TBPb60+"],
               xoutplot[xoutplot[,"year"]%in%2010,"TBMtot"],xoutplot[xoutplot[,"year"]%in%2010,"TBM0-14"],
               xoutplot[xoutplot[,"year"]%in%2010,"TBM15-59"],xoutplot[xoutplot[,"year"]%in%2010,"TBM60+"],
               xoutplot[xoutplot[,"year"]%in%2010,"TBNtot"],xoutplot[xoutplot[,"year"]%in%2010,"TBN0-14"],
               xoutplot[xoutplot[,"year"]%in%2010,"TBN15-54"],xoutplot[xoutplot[,"year"]%in%2010,"TBN55-64"],
               xoutplot[xoutplot[,"year"]%in%2010,"TBN65+"],xoutplot[xoutplot[,"year"]%in%2010,"TBItot"])

write.table(mydf2,"xoutplot_reformatted.csv",sep=",",row.names=FALSE)


## factor increase in range
multi<-c(2,2)

#write fit data matrices

FitDataP<-matrix(c(178,92,119,213,596,116,59,73,133,346),nrow=1, ncol=10, byrow=TRUE)
colnames(FitDataP)<-c("Overall2000","1529years2000","3044years2000","4559years2000","60plusyears2000","Overall2010","1529years10","3044years10","4559years10","60plusyears10")

FitDataN<-matrix(c(63.91,2.72,64.62,104.36,143.07),nrow=1, ncol=5, byrow=TRUE)
colnames(FitDataN)<-c("OverallN","0-14 yearsN","15-54 years","55-64 years","≥65 years")

FitDataM<-matrix(c(4.69,0.29,1.91,15.69),nrow=1, ncol=4, byrow=TRUE)
colnames(FitDataM)<-c("OverallM","0-14 yearsM","15-59 years","≥60 years") 

FitDataI<-matrix(78)
colnames(FitDataI)<-"I"

FitData<-cbind(FitDataP, FitDataN, FitDataM, FitDataI)

#95% CIs

cip<-matrix(c(163,72,96,174,510,195,116,146,260,698,101,40,54,106,294,132,86,99,168,407),nrow=4, ncol=5, byrow=TRUE)
colnames(cip)<-c("Overall","1529years","3044years","4559years","60plusyears")
rownames(cip)<-c("lower2000","upper2000","lower2010","upper2010") 

##lower limit is the data, so assume mormally distrib with lower limit as 95% CI
cin<-matrix(c(63.91,2.72,64.62,104.36,143.07,79.89,3.40,80.78,130.45,178.84),nrow=2, ncol=5, byrow=TRUE)
colnames(cin)<-c("Overall","0-14 years","15-54 years","55-64 years","≥65 years")
rownames(cin)<-c("lower2010","upper2010")


mortality2010<- c(4.69,0.29,1.91,15.69)
mortality2010u<- c(4.84,0.32,2.10,17.26)
mortality2010u<- mortality2010 + ((mortality2010u-mortality2010)*10)
cim<-matrix(c(4.54,0.27,1.72,14.12,mortality2010u),nrow=2, ncol=4, byrow=TRUE)
#cim<-matrix(c(4.54,0.27,1.72,14.12,4.84,0.32,2.10,17.26),nrow=2, ncol=4, byrow=TRUE)
colnames(cim)<-c("Overall","0-14 years","15-59 years","≥60 years")
rownames(cim)<-c("lower2010","upper2010") 

cii<-c(68,88)

FitDataP<-as.data.frame(FitDataP)
cip<-as.data.frame(cip)
FitDataM<-as.data.frame(FitDataM)
cim<-as.data.frame(cim)
FitDataN<-as.data.frame(FitDataN)
cin<-as.data.frame(cin)
FitDataI<-as.data.frame(FitDataI)
cii<-as.data.frame(cii)
  
#output each to a vector
inbound<-matrix(0,500000,21)
colnames(inbound)<-c("Overall2000","Overall2010","1529years2000","1529years10","3044years2000","3044years10",
                     "4559years2000","4559years10","60plusyears2000","60plusyears10","OverallM","0-14 yearsM",
                     "15-59 years","≥60 years","OverallN","0-14 yearsN","15-54 years","55-64 years","≥65 years","I","total")

inbound[,1]<- mydf2[,2]>(FitData[,"Overall2000"]-(multi[1]*(FitData[,"Overall2000"]-cip$Overall[1]))) & mydf2[,2]<(FitData[,"Overall2000"]+(multi[2]*(cip$Overall[2]-FitData[,"Overall2000"])))
inbound[,2]<- mydf2[,3]>(FitData[,"Overall2010"]-(multi[1]*(FitData[,"Overall2010"]-cip$Overall[3]))) & mydf2[,3]<(FitData[,"Overall2010"]+(multi[2]*(cip$Overall[4]-FitData[,"Overall2010"])))
inbound[,3]<- mydf2[,4]>(FitData[,"1529years2000"]-(multi[1]*(FitData[,"1529years2000"]-cip$"1529years"[1]))) & mydf2[,4]<(FitData[,"1529years2000"]+(multi[2]*(cip$"1529years"[2]-FitData[,"1529years2000"])))
inbound[,4]<- mydf2[,5]>(FitData[,"1529years10"]-(multi[1]*(FitData[,"1529years10"]-cip$"1529years"[3]))) & mydf2[,5]<(FitData[,"1529years10"]+(multi[2]*(cip$"1529years"[4]-FitData[,"1529years10"])))
inbound[,5]<- mydf2[,6]>(FitData[,"3044years2000"]-(multi[1]*(FitData[,"3044years2000"]-cip$"3044years"[1]))) & mydf2[,6]<(FitData[,"3044years2000"]+(multi[2]*(cip$"3044years"[2]-FitData[,"3044years2000"])))
inbound[,6]<- mydf2[,7]>(FitData[,"3044years10"]-(multi[1]*(FitData[,"3044years10"]-cip$"3044years"[3]))) & mydf2[,7]<(FitData[,"3044years10"]+(multi[2]*(cip$"3044years"[4]-FitData[,"3044years10"])))
inbound[,7]<- mydf2[,8]>(FitData[,"4559years2000"]-(multi[1]*(FitData[,"4559years2000"]-cip$"4559years"[1]))) & mydf2[,8]<(FitData[,"4559years2000"]+(multi[2]*(cip$"4559years"[2]-FitData[,"4559years2000"])))
inbound[,8]<- mydf2[,9]>(FitData[,"4559years10"]-(multi[1]*(FitData[,"4559years10"]-cip$"4559years"[3]))) & mydf2[,9]<(FitData[,"4559years10"]+(multi[2]*(cip$"4559years"[4]-FitData[,"4559years10"])))
inbound[,9]<- mydf2[,10]>(FitData[,"60plusyears2000"]-(multi[1]*(FitData[,"60plusyears2000"]-cip$"60plusyears"[1]))) & mydf2[,10]<(FitData[,"60plusyears2000"]+(multi[2]*(cip$"60plusyears"[2]-FitData[,"60plusyears2000"])))
inbound[,10]<- mydf2[,11]>(FitData[,"60plusyears10"]-(multi[1]*(FitData[,"60plusyears10"]-cip$"60plusyears"[3]))) & mydf2[,11]<(FitData[,"60plusyears10"]+(multi[2]*(cip$"60plusyears"[4]-FitData[,"60plusyears10"])))
inbound[,11]<- mydf2[,12]>(FitData[,"OverallM"]-(multi[1]*(FitData[,"OverallM"]-cim$Overall[1]))) & mydf2[,12]<(FitData[,"OverallM"]+(multi[2]*(cim$Overall[2]-FitData[,"OverallM"])))       
inbound[,12]<- mydf2[,13]>(FitData[,"0-14 yearsM"]-(multi[1]*(FitData[,"0-14 yearsM"]-cim$"0-14 years"[1]))) & mydf2[,13]<(FitData[,"0-14 yearsM"]+(multi[2]*(cim$"0-14 years"[2]-FitData[,"0-14 yearsM"])))      
inbound[,13]<- mydf2[,14]>(FitData[,"15-59 years"]-(multi[1]*(FitData[,"15-59 years"]-cim$"15-59 years"[1]))) & mydf2[,14]<(FitData[,"15-59 years"]+(multi[2]*(cim$"15-59 years"[2]-FitData[,"15-59 years"])))
inbound[,14]<- mydf2[,15]>(FitData[,"≥60 years"]-(multi[1]*(FitData[,"≥60 years"]-cim$"≥60 years"[1]))) & mydf2[,15]<(FitData[,"≥60 years"]+(multi[2]*(cim$"≥60 years"[2]-FitData[,"≥60 years"])))
inbound[,15]<- mydf2[,16]>(FitData[,"OverallN"]-(multi[1]*(FitData[,"OverallN"]-cin$Overall[1]))) & mydf2[,16]<(FitData[,"OverallN"]+(multi[2]*(cin$Overall[2]-FitData[,"OverallN"])))    
inbound[,16]<- mydf2[,17]>(FitData[,"0-14 yearsN"]-(multi[1]*(FitData[,"0-14 yearsN"]-cin$"0-14 years"[1]))) & mydf2[,17]<(FitData[,"0-14 yearsN"]+(multi[2]*(cin$"0-14 years"[2]-FitData[,"0-14 yearsN"])))        
inbound[,17]<- mydf2[,18]>(FitData[,"15-54 years"]-(multi[1]*(FitData[,"15-54 years"]-cin$"15-54 years"[1]))) & mydf2[,18]<(FitData[,"15-54 years"]+(multi[2]*(cin$"15-54 years"[2]-FitData[,"15-54 years"])))
inbound[,18]<- mydf2[,19]>(FitData[,"55-64 years"]-(multi[1]*(FitData[,"55-64 years"]-cin$"55-64 years"[1]))) & mydf2[,19]<(FitData[,"55-64 years"]+(multi[2]*(cin$"55-64 years"[2]-FitData[,"55-64 years"])))
inbound[,19]<- mydf2[,20]>(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[1]))) & mydf2[,20]<(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[2])))      
inbound[,20]<- mydf2[,21]>(FitData[,"I"]-(multi[1]*(FitData[,"I"]-cii[1,]))) & mydf2[,21]<(FitData[,"I"]+(multi[2]*(cii[2,]-FitData[,"I"])))     
inbound[,21]<- rowSums(inbound[,1:20])        

#write.table(inbound,"inbound_20p_2.csv",sep=",",row.names=FALSE)

#calc number of data points each run fitting to
inbound<-as.data.frame(inbound)
freqfit<-count(inbound, 'total')
#write.table(freqfit,"freqfit_20p_1time.csv",sep=",",row.names=FALSE)

#sum columns to calc percent fitting to each data point
freqfitcol<-colSums(inbound)
freqfitcol_pc<-freqfitcol/500000*100

#count when total>=i
freqfitcol_pc<-c()

for (i in 0:20){
  inboundi<-inbound[inbound$"total"==i,]
  insize<-dim(inboundi)
  freqfitcoli<-colSums(inboundi)
  freqfitcoli_pc<-freqfitcoli/insize[1]*100
  freqfitcol_pc<-cbind(freqfitcol_pc,freqfitcoli_pc) 
}

freqfitcol_pc<-t(freqfitcol_pc)
write.table(freqfitcol_pc,"fitpc_all2.csv",sep=",",row.names=FALSE)




#keep if total>=10

inbound10<-inbound[inbound$"total">=14,]
insize<-dim(inbound10)
freqfitcol10<-colSums(inbound10)
freqfitcol10_pc<-freqfitcol10/insize[1]*100

freqfitcol10_pc

fit10pc_all<-cbind(fit10pc_all,freqfitcol10_pc)
fit10pc_all<-t(fit10pc_all)

colnames(fit10pc_all)<-c("1","1.5","2")

#write.table(fit10pc_all,"fit10pc_all.csv",sep=",",row.names=FALSE)


inbound12<-inbound[inbound$"total">=12,]

# plotting any fitting >=14
run_number<-seq(1,500000,1)
inbound<-cbind(inbound,run_number)
inbound10<-inbound[inbound$"total">=14,]

in10runs<-inbound10$"run_number"

xoutplotin<-xoutplot[xoutplot$"run_count"%in%in10runs,]

par(mfrow=c(2,2))
# plot notifications tot
plot(seq(fityrs),xoutplotin$TBNtot[1:(length(fityrs))], ylab="F10_Notifications/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
lines(seq(fityrs),xoutplotin$TBNtot[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,notif2010[1])
segments(11,notif2010l[1],11,notif2010u[1])

# plot prev tot
plot(seq(fityrs),xoutplotin$"TBPb15+"[1:(length(fityrs))], ylab="F10_Prev/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBPb15+"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(1,prevalence2000[1])
points(11,prevalence2010[1])
segments(1,prevalence2000l[1],1,prevalence2000u[1])
segments(11,prevalence2010l[1],11,prevalence2010u[1])

# plot mort tot
plot(seq(fityrs),xoutplotin$"TBMtot"[1:(length(fityrs))], ylab="F10_Mort/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBMtot"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,mortality2010[1])
segments(11,mortality2010l[1],11,mortality2010u[1])


# plot inc tot
plot(seq(fityrs),xoutplotin$"TBItot"[1:(length(fityrs))], ylab="F10_Inc/100,000pop/yr",xlab="Year", ylim=c(0,600),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBItot"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,incidence2010[1])
segments(11,incidence2010l[1],11,incidence2010u[1])


## mort by age

# plot mort tot
plot(seq(fityrs),xoutplotin$"TBMtot"[1:(length(fityrs))], ylab="F10_Mort/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBMtot"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,mortality2010[1])
segments(11,mortality2010l[1],11,mortality2010u[1])

plot(seq(fityrs),xoutplotin$"TBM0-14"[1:(length(fityrs))], ylab="F10_Mort014/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBM0-14"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,mortality2010[2])
segments(11,mortality2010l[2],11,mortality2010u[2])

plot(seq(fityrs),xoutplotin$"TBM15-59"[1:(length(fityrs))], ylab="F10_Mort1559/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBM15-59"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,mortality2010[3])
segments(11,mortality2010l[3],11,mortality2010u[3])

plot(seq(fityrs),xoutplotin$"TBM60+"[1:(length(fityrs))], ylab="F10_Mort60+/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBM60+"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,mortality2010[4])
segments(11,mortality2010l[4],11,mortality2010u[4])


# plot prev by age 
par(mfrow=c(2,3))
plot(seq(fityrs),xoutplotin$"TBPb15+"[1:(length(fityrs))], ylab="F10_Prev/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBPb15+"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,prevalence2010[1])
segments(11,prevalence2010l[1],11,prevalence2010u[1])
points(1,prevalence2000[1])
segments(1,prevalence2000l[1],1,prevalence2000u[1])


plot(seq(fityrs),xoutplotin$"TBPb15-29"[1:(length(fityrs))], ylab="F10_Prev014/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBPb15-29"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,prevalence2010[2])
segments(11,prevalence2010l[2],11,prevalence2010u[2])
points(1,prevalence2000[2])
segments(1,prevalence2000l[2],1,prevalence2000u[2])

plot(seq(fityrs),xoutplotin$"TBPb30-44"[1:(length(fityrs))], ylab="F10_Prev1559/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBPb30-44"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,prevalence2010[3])
segments(11,prevalence2010l[3],11,prevalence2010u[3])
points(1,prevalence2000[3])
segments(1,prevalence2000l[3],1,prevalence2000u[3])


plot(seq(fityrs),xoutplotin$"TBPb45-59"[1:(length(fityrs))], ylab="F10_Prev60+/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBPb45-59"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,prevalence2010[4])
segments(11,prevalence2010l[4],11,prevalence2010u[4])
points(1,prevalence2000[4])
segments(1,prevalence2000l[4],1,prevalence2000u[4])


plot(seq(fityrs),xoutplotin$"TBPb60+"[1:(length(fityrs))], ylab="F10_Prev60+/100,000pop/yr",xlab="Year", ylim=c(0,1000),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBPb60+"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,prevalence2010[5])
segments(11,prevalence2010l[5],11,prevalence2010u[5])
points(1,prevalence2000[5])
segments(1,prevalence2000l[5],1,prevalence2000u[5])

# plot notif tot
par(mfrow=c(2,3))

plot(seq(fityrs),xoutplotin$"TBNtot"[1:(length(fityrs))], ylab="F10_Notif/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBNtot"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,notif2010[1])
segments(11,notif2010l[1],11,notif2010u[1])

plot(seq(fityrs),xoutplotin$"TBN0-14"[1:(length(fityrs))], ylab="F10_Notif014/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBN0-14"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,notif2010[2])
segments(11,notif2010l[2],11,notif2010u[2])

plot(seq(fityrs),xoutplotin$"TBN15-54"[1:(length(fityrs))], ylab="F10_Notif1554/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBN15-54"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,notif2010[3])
segments(11,notif2010l[3],11,notif2010u[3])

plot(seq(fityrs),xoutplotin$"TBN55-64"[1:(length(fityrs))], ylab="F10_Notif5564/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBN55-64"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,notif2010[4])
segments(11,notif2010l[4],11,notif2010u[4])


plot(seq(fityrs),xoutplotin$"TBN65+"[1:(length(fityrs))], ylab="F10_Notif65+/100,000pop/yr",xlab="Year", ylim=c(0,400),type='l',col='orange')
for (i in 2:length(in10runs)){
  lines(seq(fityrs),xoutplotin$"TBN65+"[(((i-1)*(length(fityrs)))+1):(i*(length(fityrs)))],type='l', col='orange')
}
points(11,notif2010[5])
segments(11,notif2010l[5],11,notif2010u[5])



## plotting parameters of runs >=14

parain<-para[para$"run_number"%in%in10runs,]


par(mfrow=c(3,3))
parain<-as.data.frame(parain)
#para_narrow<-as.data.frame(para_narrow)

for (i in 1:(length(nm)-1))
{
  nam<-paste(pararange[i,1]," posterior",sep='')
  #nam2<-as.data.frame(pararange[i,1])
  tpar<-as.numeric(parain[,(i+2)])
  #tpar_narrow<-as.numeric(para_narrow[t_narrow,(i+2)])
  assign(nam,tpar)
  plot(density(tpar),xlim=c(as.numeric(pararange[i,2]),as.numeric(pararange[i,3])),main=nam, col="red") # plot the density of the posterior
  lines(density(para[,i+2]))  # plot the density of the prior (a flat line (ish as density smooths things) as it was uniform)
  #lines(density(tpar_narrow), col="blue") # plot the density of the posterior
  #lines(density(para_narrow[,i+2])) 
}




# inbound<- which(mydf2[,2]>(FitData[,"Overall2000"]-(multi[1]*(FitData[,"Overall2000"]-cip$Overall[1]))) & mydf2[,2]<(FitData[,"Overall2000"]+(multi[2]*(cip$Overall[2]-FitData[,"Overall2000"])))
#                      & mydf2[,3]>(FitData[,"Overall2010"]-(multi[1]*(FitData[,"Overall2010"]-cip$Overall[3]))) & mydf2[,3]<(FitData[,"Overall2010"]+(multi[2]*(cip$Overall[4]-FitData[,"Overall2010"])))
#                      & mydf2[,4]>(FitData[,"1529years2000"]-(multi[1]*(FitData[,"1529years2000"]-cip$"1529years"[1]))) & mydf2[,4]<(FitData[,"1529years2000"]+(multi[2]*(cip$"1529years"[2]-FitData[,"1529years2000"])))
#                      & mydf2[,5]>(FitData[,"1529years10"]-(multi[1]*(FitData[,"1529years10"]-cip$"1529years"[3]))) & mydf2[,5]<(FitData[,"1529years10"]+(multi[2]*(cip$"1529years"[4]-FitData[,"1529years10"])))
#                      & mydf2[,6]>(FitData[,"3044years2000"]-(multi[1]*(FitData[,"3044years2000"]-cip$"3044years"[1]))) & mydf2[,6]<(FitData[,"3044years2000"]+(multi[2]*(cip$"3044years"[2]-FitData[,"3044years2000"])))
#                      & mydf2[,7]>(FitData[,"3044years10"]-(multi[1]*(FitData[,"3044years10"]-cip$"3044years"[3]))) & mydf2[,7]<(FitData[,"3044years10"]+(multi[2]*(cip$"3044years"[4]-FitData[,"3044years10"])))
#                      & mydf2[,8]>(FitData[,"4559years2000"]-(multi[1]*(FitData[,"4559years2000"]-cip$"4559years"[1]))) & mydf2[,8]<(FitData[,"4559years2000"]+(multi[2]*(cip$"4559years"[2]-FitData[,"4559years2000"])))
#                      & mydf2[,9]>(FitData[,"4559years10"]-(multi[1]*(FitData[,"4559years10"]-cip$"4559years"[3]))) & mydf2[,9]<(FitData[,"4559years10"]+(multi[2]*(cip$"4559years"[4]-FitData[,"4559years10"])))
#                      & mydf2[,10]>(FitData[,"60plusyears2000"]-(multi[1]*(FitData[,"60plusyears2000"]-cip$"60plusyears"[1]))) & mydf2[,10]<(FitData[,"60plusyears2000"]+(multi[2]*(cip$"60plusyears"[2]-FitData[,"60plusyears2000"])))
#                      & mydf2[,11]>(FitData[,"60plusyears10"]-(multi[1]*(FitData[,"60plusyears10"]-cip$"60plusyears"[3]))) & mydf2[,11]<(FitData[,"60plusyears10"]+(multi[2]*(cip$"60plusyears"[4]-FitData[,"60plusyears10"])))
#                      & mydf2[,12]>(FitData[,"OverallM"]-(multi[1]*(FitData[,"OverallM"]-cim$Overall[1]))) & mydf2[,12]<(FitData[,"OverallM"]+(multi[2]*(cim$Overall[2]-FitData[,"OverallM"])))       
#                      & mydf2[,13]>(FitData[,"0-14 yearsM"]-(multi[1]*(FitData[,"0-14 yearsM"]-cim$"0-14 years"[1]))) & mydf2[,13]<(FitData[,"0-14 yearsM"]+(multi[2]*(cim$"0-14 years"[2]-FitData[,"0-14 yearsM"])))      
#                      & mydf2[,14]>(FitData[,"15-59 years"]-(multi[1]*(FitData[,"15-59 years"]-cim$"15-59 years"[1]))) & mydf2[,14]<(FitData[,"15-59 years"]+(multi[2]*(cim$"15-59 years"[2]-FitData[,"15-59 years"])))
#                      & mydf2[,15]>(FitData[,"≥60 years"]-(multi[1]*(FitData[,"≥60 years"]-cim$"≥60 years"[1]))) & mydf2[,15]<(FitData[,"≥60 years"]+(multi[2]*(cim$"≥60 years"[2]-FitData[,"≥60 years"])))
#                      & mydf2[,16]>(FitData[,"OverallN"]-(multi[1]*(FitData[,"OverallN"]-cin$Overall[1]))) & mydf2[,16]<(FitData[,"OverallN"]+(multi[2]*(cin$Overall[2]-FitData[,"OverallN"])))    
#                      & mydf2[,17]>(FitData[,"0-14 yearsN"]-(multi[1]*(FitData[,"0-14 yearsN"]-cin$"0-14 years"[1]))) & mydf2[,17]<(FitData[,"0-14 yearsN"]+(multi[2]*(cin$"0-14 years"[2]-FitData[,"0-14 yearsN"])))        
#                      & mydf2[,18]>(FitData[,"15-54 years"]-(multi[1]*(FitData[,"15-54 years"]-cin$"15-54 years"[1]))) & mydf2[,18]<(FitData[,"15-54 years"]+(multi[2]*(cin$"15-54 years"[2]-FitData[,"15-54 years"])))
#                      & mydf2[,19]>(FitData[,"55-64 years"]-(multi[1]*(FitData[,"55-64 years"]-cin$"55-64 years"[1]))) & mydf2[,19]<(FitData[,"55-64 years"]+(multi[2]*(cin$"55-64 years"[2]-FitData[,"55-64 years"])))
#                      & mydf2[,20]>(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[1]))) & mydf2[,20]<(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[2])))      
#                      & mydf2[,21]>(FitData[,"I"]-(multi[1]*(FitData[,"I"]-cii[1,]))) & mydf2[,21]<(FitData[,"I"]+(multi[2]*(cii[2,]-FitData[,"I"])))     
# )


## check that both methods give you the same runs
cbind(t,t2)