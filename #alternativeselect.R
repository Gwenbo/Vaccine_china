## code for only keeping runs that are within 'multi' multiplier of the data CIs ###

#multiplier for lower and upper bounds of CIs
multi<-c(2,2)
inbound<-c()
fittrack<-matrix(0,(n_p*numjobs),2)
colnames(fittrack)<-c("run_count","fitcount")
FitDataP<-as.data.frame(FitDataP)
cip<-as.data.frame(cip)
FitDataM<-as.data.frame(FitDataM)
cim<-as.data.frame(cim)
FitDataN<-as.data.frame(FitDataN)
cin<-as.data.frame(cin)
FitDataI<-as.data.frame(FitDataI)
cii<-as.data.frame(cii)

for (jj in 141800:(n_p*numjobs)){

  fitcount<-0
  yr2000<-which((xoutplot[,"year"]%in%2000)&(xoutplot[,"run_count"]%in%jj))
  #yr2000<-which((xoutplot[,"year"]%in%2000)&(xoutplot[,"run_count"]%in%78089))
  yr2010<-which((xoutplot[,"year"]%in%2010)&(xoutplot[,"run_count"]%in%jj))
  #yr2010<-which((xoutplot[,"year"]%in%2000)&(xoutplot[,"run_count"]%in%78089))
  
  ##prevalence  
      #overall P 2000
      if ((xoutplot[yr2000,"TBPb15+"]>(FitData[,"Overall2000"]-(multi[1]*(FitData[,"Overall2000"]-cip$Overall[1])))) & (xoutplot[yr2000,"TBPb15+"]<(FitData[,"Overall2000"]+(multi[2]*(cip$Overall[2]-FitData[,"Overall2000"]))))) {fitcount<-fitcount+1}
      #overall P 2010
      if ((xoutplot[yr2010,"TBPb15+"]>(FitData[,"Overall2010"]-(multi[1]*(FitData[,"Overall2010"]-cip$Overall[3])))) & (xoutplot[yr2010,"TBPb15+"]<(FitData[,"Overall2010"]+(multi[2]*(cip$Overall[4]-FitData[,"Overall2010"]))))) {fitcount<-fitcount+1}
      #15-29 P 2000
      if (xoutplot[yr2000,"TBPb15-29"]>(FitData[,"1529years2000"]-(multi[1]*(FitData[,"1529years2000"]-cip$"1529years"[1]))) & xoutplot[yr2000,"TBPb15-29"]<(FitData[,"1529years2000"]+(multi[2]*(cip$"1529years"[2]-FitData[,"1529years2000"])))) {fitcount<-fitcount+1}
      #15-29 P 2010
      if (xoutplot[yr2010,"TBPb15-29"]>(FitData[,"1529years10"]-(multi[1]*(FitData[,"1529years10"]-cip$"1529years"[3])))  & xoutplot[yr2010,"TBPb15-29"]<(FitData[,"1529years10"]+(multi[2]*(cip$"1529years"[4]-FitData[,"1529years10"])))) {fitcount<-fitcount+1}
      #30-44 P 2000
      if (xoutplot[yr2000,"TBPb30-44"]>(FitData[,"3044years2000"]-(multi[1]*(FitData[,"3044years2000"]-cip$"3044years"[1]))) & xoutplot[yr2000,"TBPb30-44"]<(FitData[,"3044years2000"]+(multi[2]*(cip$"3044years"[2]-FitData[,"3044years2000"])))) {fitcount<-fitcount+1}
      #30-44 P 2010
      if (xoutplot[yr2010,"TBPb30-44"]>(FitData[,"3044years10"]-(multi[1]*(FitData[,"3044years10"]-cip$"3044years"[3]))) & xoutplot[yr2010,"TBPb30-44"]<(FitData[,"3044years10"]+(multi[2]*(cip$"3044years"[4]-FitData[,"3044years10"])))) {fitcount<-fitcount+1}
      #4559 P 2000
      if (xoutplot[yr2000,"TBPb45-59"]>(FitData[,"4559years2000"]-(multi[1]*(FitData[,"4559years2000"]-cip$"4559years"[1]))) & xoutplot[yr2000,"TBPb45-59"]<(FitData[,"4559years2000"]+(multi[2]*(cip$"4559years"[2]-FitData[,"4559years2000"])))) {fitcount<-fitcount+1}
      #4559 P 2010
      if (xoutplot[yr2010,"TBPb45-59"]>(FitData[,"4559years10"]-(multi[1]*(FitData[,"4559years10"]-cip$"4559years"[3]))) & xoutplot[yr2010,"TBPb45-59"]<(FitData[,"4559years10"]+(multi[2]*(cip$"4559years"[4]-FitData[,"4559years10"])))) {fitcount<-fitcount+1}
      #60plusyears P 2000
      if (xoutplot[yr2000,"TBPb60+"]>(FitData[,"60plusyears2000"]-(multi[1]*(FitData[,"60plusyears2000"]-cip$"60plusyears"[1]))) & xoutplot[yr2000,"TBPb60+"]< (FitData[,"60plusyears2000"]+(multi[2]*(cip$"60plusyears"[2]-FitData[,"60plusyears2000"])))) {fitcount<-fitcount+1}
      #60plusyears P 2010
      if (xoutplot[yr2010,"TBPb60+"]>(FitData[,"60plusyears10"]-(multi[1]*(FitData[,"60plusyears10"]-cip$"60plusyears"[3]))) & xoutplot[yr2010,"TBPb60+"]<(FitData[,"60plusyears10"]+(multi[2]*(cip$"60plusyears"[3]-FitData[,"60plusyears10"])))) {fitcount<-fitcount+1}
  
  
  ##mortality
      #overall M 2010
      if (xoutplot[yr2010,"TBMtot"]>(FitData[,"OverallM"]-(multi[1]*(FitData[,"OverallM"]-cim$Overall[1]))) & xoutplot[yr2010,"TBMtot"]<(FitData[,"OverallM"]+(multi[2]*(cim$Overall[2]-FitData[,"OverallM"])))) {fitcount<-fitcount+1}
       #"0-14 years" M 2010
      if (xoutplot[yr2010,"TBM0-14"]>(FitData[,"0-14 yearsM"]-(multi[1]*(FitData[,"0-14 yearsM"]-cim$"0-14 years"[1]))) & xoutplot[yr2010,"TBM0-14"]<(FitData[,"0-14 yearsM"]+(multi[2]*(cim$"0-14 years"[2]-FitData[,"0-14 yearsM"])))) {fitcount<-fitcount+1}
      #15-59 years M 2010
      if (xoutplot[yr2010,"TBM15-59"]>(FitData[,"15-59 years"]-(multi[1]*(FitData[,"15-59 years"]-cim$"15-59 years"[1]))) & xoutplot[yr2010,"TBM15-59"]<(FitData[,"15-59 years"]+(multi[2]*(cim$"15-59 years"[2]-FitData[,"15-59 years"])))) {fitcount<-fitcount+1}
       #60plusyears M 2010
      if (xoutplot[yr2010,"TBM60+"]>(FitData[,"≥60 years"]-(multi[1]*(FitData[,"≥60 years"]-cim$"≥60 years"[1]))) & xoutplot[yr2010,"TBM60+"]<(FitData[,"≥60 years"]+(multi[2]*(cim$"≥60 years"[2]-FitData[,"≥60 years"])))) {fitcount<-fitcount+1}
      
  
  ## notifications

      #overall N 2010
      if (xoutplot[yr2010,"TBNtot"]>(FitData[,"OverallN"]-(multi[1]*(FitData[,"OverallN"]-cin$Overall[1]))) & xoutplot[yr2010,"TBNtot"]<(FitData[,"OverallN"]+(multi[2]*(cin$Overall[2]-FitData[,"OverallN"])))) {fitcount<-fitcount+1}
      #"0-14 years" N 2010
      if (xoutplot[yr2010,"TBN0-14"]>(FitData[,"0-14 yearsN"]-(multi[1]*(FitData[,"0-14 yearsN"]-cin$"0-14 years"[1]))) & xoutplot[yr2010,"TBN0-14"]<(FitData[,"0-14 yearsN"]+(multi[2]*(cin$"0-14 years"[2]-FitData[,"0-14 yearsN"])))) {fitcount<-fitcount+1}
      #15-54 years N 2010
      if (xoutplot[yr2010,"TBN15-54"]>(FitData[,"15-54 years"]-(multi[1]*(FitData[,"15-54 years"]-cin$"15-54 years"[1]))) & xoutplot[yr2010,"TBN15-54"]<(FitData[,"15-54 years"]+(multi[2]*(cin$"15-54 years"[2]-FitData[,"15-54 years"])))) {fitcount<-fitcount+1}
       #55-64 years N 2010
      if (xoutplot[yr2010,"TBN55-64"]>(FitData[,"55-64 years"]-(multi[1]*(FitData[,"55-64 years"]-cin$"55-64 years"[1]))) & xoutplot[yr2010,"TBN55-64"]<(FitData[,"55-64 years"]+(multi[2]*(cin$"55-64 years"[2]-FitData[,"55-64 years"])))) {fitcount<-fitcount+1}
      #60plusyears N 2010
      if (xoutplot[yr2010,"TBN65+"]>(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[1]))) & xoutplot[yr2010,"TBN65+"]<(FitData[,"≥65 years"]-(multi[1]*(FitData[,"≥65 years"]-cin$"≥65 years"[1])))) {fitcount<-fitcount+1}
      
  ## Incidence 
      #overall I 2010
      if (xoutplot[yr2010,"TBItot"]> (FitData[,"I"]-(multi[1]*(FitData[,"I"]-cii[1,]))) & xoutplot[yr2010,"TBItot"]<(FitData[,"I"]+(multi[2]*(cii[2,]-FitData[,"I"])))) {fitcount<-fitcount+1}
 
  
  ### Keep run number if meets certain criterion
    print(jj)
    fittrack[jj,1]<-jj
    fittrack[jj,2]<-fitcount
    if (fitcount==20){inbound<-cbind(inbound,jj)}
  
}

inbound

write.table(inbound,"inbound_0.csv",sep=",",row.names=FALSE)
write.table(fittrack,"fittrack_0.csv",sep=",",row.names=FALSE)

# inbound<-read.table("inbound_0.csv",sep=",")
# fittrack<-read.table("fittrack_0.csv",sep=",",head=TRUE)



