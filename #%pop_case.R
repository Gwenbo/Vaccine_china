### % population and cases per age group ###

PcPop<-matrix(0,2,6)
popsizeall<-sum(psize[301:302])
casesall<-sum(new_actv[301:302,])
PcPop[1,1]<-sum(psize014[301:302])/popsizeall*100
PcPop[1,2]<-sum(psize1524[301:302])/popsizeall*100
PcPop[1,3]<-sum(psize2554[301:302])/popsizeall*100
PcPop[1,4]<-sum(psize5564[301:302])/popsizeall*100
PcPop[1,5]<-((sum(psize5574[301:302]))-(sum(psize5564[301:302])))/popsizeall*100
PcPop[1,6]<-sum(psize75plus[301:302])/popsizeall*100


PcPop[2,1]<-sum(new_actv[301:302,1:15])/casesall*100
PcPop[2,2]<-sum(new_actv[301:302,16:25])/casesall*100
PcPop[2,3]<-sum(new_actv[301:302,26:55])/casesall*100
PcPop[2,4]<-sum(new_actv[301:302,56:65])/casesall*100
PcPop[2,5]<-sum(new_actv[301:302,66:75])/casesall*100
PcPop[2,6]<-sum(new_actv[301:302,76:Mnage])/casesall*100

colnames(PcPop)<-c("0-14","15-24","25-54","55-64","65-74","75+")
rownames(PcPop)<-c("% population", "%cases")

setwd("Outputs")
write.table(PcPop,'Percent pop_cases per agegrp.csv',sep=",",row.names=TRUE,col.names=TRUE)
setwd(home)