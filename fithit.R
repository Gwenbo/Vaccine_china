### Fit hit - calc number of data ranges the model is within 

#call in libraries
library(plyr)

#reformat data

mydf2 <- cbind(xout[xout[,"year"]%in%2000,"TBPb15+"],xout[xout[,"year"]%in%2010,"TBPb15+"],
               xout[xout[,"year"]%in%2000,"TBPb15-29"],xout[xout[,"year"]%in%2010,"TBPb15-29"],
               xout[xout[,"year"]%in%2000,"TBPb30-44"],xout[xout[,"year"]%in%2010,"TBPb30-44"],
               xout[xout[,"year"]%in%2000,"TBPb45-59"],xout[xout[,"year"]%in%2010,"TBPb45-59"],
               xout[xout[,"year"]%in%2000,"TBPb60+"],xout[xout[,"year"]%in%2010,"TBPb60+"],
               xout[xout[,"year"]%in%2010,"TBMtot"],xout[xout[,"year"]%in%2010,"TBM0-14"],
               xout[xout[,"year"]%in%2010,"TBM15-59"],xout[xout[,"year"]%in%2010,"TBM60+"],
               xout[xout[,"year"]%in%2010,"TBNtot"],xout[xout[,"year"]%in%2010,"TBN0-14"],
               xout[xout[,"year"]%in%2010,"TBN15-54"],xout[xout[,"year"]%in%2010,"TBN55-64"],
               xout[xout[,"year"]%in%2010,"TBN65+"],xout[xout[,"year"]%in%2010,"TBItot"])


## factor increase in range
multi<-c(1,1)

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
inbound<-matrix(0,2,21)
colnames(inbound)<-c("Overall2000","Overall2010","1529years2000","1529years10","3044years2000","3044years10",
                     "4559years2000","4559years10","60plusyears2000","60plusyears10","OverallM","0-14 yearsM",
                     "15-59 years","≥60 years","OverallN","0-14 yearsN","15-54 years","55-64 years","≥65 years","I","total")

inbound[,1]<- mydf2[,1]>(FitData[,"Overall2000"]-(multi[1]*(FitData[,"Overall2000"]-cip$Overall[1]))) & mydf2[,1]<(FitData[,"Overall2000"]+(multi[2]*(cip$Overall[2]-FitData[,"Overall2000"])))
inbound[,2]<- mydf2[,2]>(FitData[,"Overall2010"]-(multi[1]*(FitData[,"Overall2010"]-cip$Overall[3]))) & mydf2[,2]<(FitData[,"Overall2010"]+(multi[2]*(cip$Overall[4]-FitData[,"Overall2010"])))
inbound[,3]<- mydf2[,3]>(FitData[,"1529years2000"]-(multi[1]*(FitData[,"1529years2000"]-cip$"1529years"[1]))) & mydf2[,3]<(FitData[,"1529years2000"]+(multi[2]*(cip$"1529years"[2]-FitData[,"1529years2000"])))
inbound[,4]<- mydf2[,4]>(FitData[,"1529years10"]-(multi[1]*(FitData[,"1529years10"]-cip$"1529years"[3]))) & mydf2[,4]<(FitData[,"1529years10"]+(multi[2]*(cip$"1529years"[4]-FitData[,"1529years10"])))
inbound[,5]<- mydf2[,5]>(FitData[,"3044years2000"]-(multi[1]*(FitData[,"3044years2000"]-cip$"3044years"[1]))) & mydf2[,5]<(FitData[,"3044years2000"]+(multi[2]*(cip$"3044years"[2]-FitData[,"3044years2000"])))
inbound[,6]<- mydf2[,6]>(FitData[,"3044years10"]-(multi[1]*(FitData[,"3044years10"]-cip$"3044years"[3]))) & mydf2[,6]<(FitData[,"3044years10"]+(multi[2]*(cip$"3044years"[4]-FitData[,"3044years10"])))
inbound[,7]<- mydf2[,7]>(FitData[,"4559years2000"]-(multi[1]*(FitData[,"4559years2000"]-cip$"4559years"[1]))) & mydf2[,7]<(FitData[,"4559years2000"]+(multi[2]*(cip$"4559years"[2]-FitData[,"4559years2000"])))
inbound[,8]<- mydf2[,8]>(FitData[,"4559years10"]-(multi[1]*(FitData[,"4559years10"]-cip$"4559years"[3]))) & mydf2[,8]<(FitData[,"4559years10"]+(multi[2]*(cip$"4559years"[4]-FitData[,"4559years10"])))
inbound[,9]<- mydf2[,9]>(FitData[,"60plusyears2000"]-(multi[1]*(FitData[,"60plusyears2000"]-cip$"60plusyears"[1]))) & mydf2[,9]<(FitData[,"60plusyears2000"]+(multi[2]*(cip$"60plusyears"[2]-FitData[,"60plusyears2000"])))
inbound[,10]<- mydf2[,10]>(FitData[,"60plusyears10"]-(multi[1]*(FitData[,"60plusyears10"]-cip$"60plusyears"[3]))) & mydf2[,10]<(FitData[,"60plusyears10"]+(multi[2]*(cip$"60plusyears"[4]-FitData[,"60plusyears10"])))
inbound[,11]<- mydf2[,11]>(FitData[,"OverallM"]-(multi[1]*(FitData[,"OverallM"]-cim$Overall[1]))) & mydf2[,11]<(FitData[,"OverallM"]+(multi[2]*(cim$Overall[2]-FitData[,"OverallM"])))       
inbound[,12]<- mydf2[,12]>(FitData[,"0-14 yearsM"]-(multi[1]*(FitData[,"0-14 yearsM"]-cim$"0-14 years"[1]))) & mydf2[,12]<(FitData[,"0-14 yearsM"]+(multi[2]*(cim$"0-14 years"[2]-FitData[,"0-14 yearsM"])))      
inbound[,13]<- mydf2[,13]>(FitData[,"15-59 years"]-(multi[1]*(FitData[,"15-59 years"]-cim$"15-59 years"[1]))) & mydf2[,13]<(FitData[,"15-59 years"]+(multi[2]*(cim$"15-59 years"[2]-FitData[,"15-59 years"])))
inbound[,14]<- mydf2[,14]>(FitData[,"≥60 years"]-(multi[1]*(FitData[,"≥60 years"]-cim$"≥60 years"[1]))) & mydf2[,14]<(FitData[,"≥60 years"]+(multi[2]*(cim$"≥60 years"[2]-FitData[,"≥60 years"])))
inbound[,15]<- mydf2[,15]>(FitData[,"OverallN"]-(multi[1]*(FitData[,"OverallN"]-cin$Overall[1]))) & mydf2[,15]<(FitData[,"OverallN"]+(multi[2]*(cin$Overall[2]-FitData[,"OverallN"])))    
inbound[,16]<- mydf2[,16]>(FitData[,"0-14 yearsN"]-(multi[1]*(FitData[,"0-14 yearsN"]-cin$"0-14 years"[1]))) & mydf2[,16]<(FitData[,"0-14 yearsN"]+(multi[2]*(cin$"0-14 years"[2]-FitData[,"0-14 yearsN"])))        
inbound[,17]<- mydf2[,17]>(FitData[,"15-54 years"]-(multi[1]*(FitData[,"15-54 years"]-cin$"15-54 years"[1]))) & mydf2[,17]<(FitData[,"15-54 years"]+(multi[2]*(cin$"15-54 years"[2]-FitData[,"15-54 years"])))
inbound[,18]<- mydf2[,18]>(FitData[,"55-64 years"]-(multi[1]*(FitData[,"55-64 years"]-cin$"55-64 years"[1]))) & mydf2[,18]<(FitData[,"55-64 years"]+(multi[2]*(cin$"55-64 years"[2]-FitData[,"55-64 years"])))
inbound[,19]<- mydf2[,19]>(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[1]))) & mydf2[,19]<(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[2])))      
inbound[,20]<- mydf2[,20]>(FitData[,"I"]-(multi[1]*(FitData[,"I"]-cii[1,]))) & mydf2[,20]<(FitData[,"I"]+(multi[2]*(cii[2,]-FitData[,"I"])))     
#calc number of data points the run fitting to
inbound[,21]<- rowSums(inbound[,1:20])        

#only want first number (second one is emmpty to keep as a data frame instead of vector)
fithit<-inbound[1,21]

