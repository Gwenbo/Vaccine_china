### VACCINE PLOTS of 20 top likelihood runs ###

fityrs<-seq(2000,2050,1)
halfyr<-rep(times,times = ((combn*(typen-1)+1)*20))
yrs<-rep(year,times = ((combn*(typen-1)+1)*20))
dfvx2<-cbind(dfvx,halfyr,yrs)
dfvx3<-as.data.frame(dfvx2)

run<-rep(top_L, each=22046)
dfvx4<-cbind(run,dfvx3)
dfvx_sm<-dfvx4[which(dfvx4[,"yrs"]%in%fityrs), c("run","type","vxint","yrs","TBPbtot", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]


### manipluate dfvx to have data frame for each outcome so is more manageable
vacc_p<-dfvx_sm[,"TBPbtot"]
dim(vacc_p)<-c((length(fityrs)),((combn*(typen-1)+1)*20))

ci<-matrix(0,(combn*(typen-1)+1)*length(fityrs),6)
colnames(ci)<-c("type","vxint","yr","m","l","u")
type<-c(rep(0,(length(fityrs))),rep((seq(2,7,1)),each=(length(fityrs)*combn)))
vxint<-c((rep(0,(length(fityrs)))),rep((rep((seq(1,combn,1)), each=(length(fityrs)))),(typen-1)))
ci[,1]<-type
ci[,2]<-vxint
ci[,3]<-fityrs

#calculate cis and median

dfvx_0<-dfvx_sm[which(dfvx_sm[,"type"]%in%0),]                                        
                                        
for(i in 2:typen){                                        
dfvx_S<-dfvx_sm[which(dfvx_sm[,"type"]%in%i),]

for(j in 1:combn){
dfvx_m<-dfvx_S[which(dfvx_S[,"vxint"]%in%j),]

dfvx_inc<-dfvx_m$TBItot

dim(dfvx_inc)<-c((length(fityrs)),20)

model_mm=apply(dfvx_inc,1,function(x) quantile(x,probs=c(0.5))) 
model_um=apply(mall_xout[,top_L],1,function(x) quantile(x,probs=c(0.975))) 
model_lm=apply(mall_xout[,top_L],1,function(x) quantile(x,probs=c(0.025))) 
}
}



###dfvx's are missing row names after 419478 - is this a problem?


vpl1

#will need to generate model_p for 2000-2050
model_p<-cbind(seq(2000,2010,1),model_mp, model_lp,model_up)
colnames(model_p)<-c("year","m","l","u")
model_p<-as.data.frame(model_p)
pall_xout<-as.data.frame(pall_xout)
vpl1<-ggplot(data=model_p,aes(x=year))+geom_line(colour="black",aes(y=m))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,300)+
  ylab("Prevalence Rate of Bacteriologically Confirmed TB (/100,000pop)")+
  geom_line(aes(y=l),col="green")+
  geom_line(aes(y=u),col="green")+
  geom_point(data=FitDataP,aes(x=2000,y=FitDataP$Overall2000,group=1),colour="red",size=3)+
  geom_point(data=FitDataP,aes(x=2010,y=FitDataP$Overall2010,group=2),colour="red",size=3)
#need to add in shading instead of green lines
vpl1<-vpl1+geom_errorbar(aes(x=2000,ymin=cip$Overall[1], ymax=cip$Overall[2]),width=0.5,colour="red")
vpl1<-vpl1+geom_errorbar(aes(x=2010,ymin=cip$Overall[3], ymax=cip$Overall[4]),width=0.5,colour="red")
#add in vaccine data
#vpl1<-vpl1+geom_line(data=#insert vaccine data frame#,aes(y=#insert vaccine data frame column#),colour="blue")

vpl1

