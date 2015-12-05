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
# vacc_p<-dfvx_sm[,"TBPbtot"]
# dim(vacc_p)<-c((length(fityrs)),((combn*(typen-1)+1)*20))

# calc median and cis of incidence of the top 20 runs for baseline and each vaccine scenario
ci<-matrix(0,(combn*(typen-1)+1)*length(fityrs),6)
colnames(ci)<-c("type","vxint","yr","m","l","u")
type<-c(rep(0,(length(fityrs))),rep((seq(2,7,1)),each=(length(fityrs)*combn)))
vxint<-c((rep(0,(length(fityrs)))),rep((rep((seq(1,combn,1)), each=(length(fityrs)))),(typen-1)))
ci[,1]<-type
ci[,2]<-vxint
ci[,3]<-fityrs

#calculate cis and median

dfvx_0<-dfvx_sm[which(dfvx_sm[,"type"]%in%0),]                                        
dfvx_inc0<-dfvx_0$TBItot
dim(dfvx_inc0)<-c((length(fityrs)),20)
ci[1:(length(fityrs)),4]=apply(dfvx_inc0,1,function(x) quantile(x,probs=c(0.5))) 
ci[1:(length(fityrs)),5]=apply(dfvx_inc0,1,function(x) quantile(x,probs=c(0.025))) 
ci[1:(length(fityrs)),6]=apply(dfvx_inc0,1,function(x) quantile(x,probs=c(0.975))) 

count<-0

for(i in 2:typen){                                        
dfvx_S<-dfvx_sm[which(dfvx_sm[,"type"]%in%i),]

for(j in 1:combn){
count<-count+1
dfvx_m<-dfvx_S[which(dfvx_S[,"vxint"]%in%j),]

dfvx_inc<-dfvx_m$TBItot

dim(dfvx_inc)<-c((length(fityrs)),20)

ci[((count*(length(fityrs)))+1):((count+1)*(length(fityrs))),4]=apply(dfvx_inc,1,function(x) quantile(x,probs=c(0.5))) 
ci[((count*(length(fityrs)))+1):((count+1)*(length(fityrs))),5]=apply(dfvx_inc,1,function(x) quantile(x,probs=c(0.025))) 
ci[((count*(length(fityrs)))+1):((count+1)*(length(fityrs))),6]=apply(dfvx_inc,1,function(x) quantile(x,probs=c(0.975))) 

}
}


scen<-rep((seq(0,(combn*(typen-1)),1)), each=length(fityrs))
ci<-cbind(ci,scen)

###dfvx's are missing row names after 419478 - is this a problem?

## plots all model runs all at once
vpl1

#will need to generate model_p for 2000-2050
model_p<-cbind(seq(2000,2050,1),model_mp, model_lp,model_up)
colnames(model_p)<-c("year","m","l","u")
ci<-as.data.frame(ci)
pall_xout<-as.data.frame(pall_xout)
vpl1<-ggplot(data=ci,aes(x=yr))+geom_line(colour="black",aes(y=m, group=scen))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,150)+
  ylab("Incidence Rate(/100,000pop)")+
  geom_line(aes(y=l, group=scen),col="green")+
  geom_line(aes(y=u, group=scen),col="green")+
  geom_point(data=FitDataI,aes(x=2010,y=FitDataI$I),colour="red",size=3)
#need to add in shading instead of green lines
vpl1<-vpl1+geom_errorbar(aes(x=2010,ymin=cii[1,1], ymax=cii[2,1]),width=0.5,colour="red")
#add in vaccine data
#vpl1<-vpl1+geom_line(data=#insert vaccine data frame#,aes(y=#insert vaccine data frame column#),colour="blue")

vpl1


### GRAPH TO SHOW THAT even with best vaccine we DONT HIT 2050 GOALS
vpl2<-ggplot(data=ci[ci$scen%in%c(0,36),],aes(x=yr))+geom_line(colour="black",aes(y=m, group=scen))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,150)+
  ylab("Incidence Rate(/100,000pop)")+
#  geom_line(aes(y=l, group=scen),col="green")+
 # geom_line(aes(y=u, group=scen),col="green")+
  geom_ribbon(aes(ymin=l, ymax=u,group=scen,fill=scen),alpha=0.3,colour=NA)+
  theme_bw()+
  theme(legend.position="none")
  


### calculate reduction
100*((dfvx[151,15]-dfvx[(y1[-1]+150),15]))/(dfvx[151,15])

ci2050<-ci[ci$yr%in%2050,]

red2050<-matrix(0,72,8)
colnames(red2050)<-c("Vaccine type","Age target","VE","Duration","Coverage","Median","Lower" ,"Upper")


for (i in 1:72){
  red2050[i,6]<-100*(ci2050[1,4]-ci2050[(i+1),4])/(ci2050[1,4])
  red2050[i,7]<-100*(ci2050[1,5]-ci2050[(i+1),5])/(ci2050[1,5])
  red2050[i,8]<-100*(ci2050[1,6]-ci2050[(i+1),6])/(ci2050[1,6])
  
}

red2050<-as.data.frame(red2050)
red2050[,1]<-rep((c("Pre-infection", "Post-infection","Mixed")),each=12)
red2050[,2]<-rep((c("elderly", "adolescents")),each=36)
red2050[,3]<-rep((c("40%", "60%","80%")),each=2)
red2050[,4]<-rep((c("10yrs","20yrs")),each=1)
red2050[,5]<-rep((c("30%", "70%")),each=6)



#bar chart of % reduction
vpl3<-ggplot(data=red2050,aes(x=XXXX))+geom_bar(aes(y=m, group=scen))+ 
  xlim(2000,2050)+
  xlab("Years")+
  ylim(0,150)+
  ylab("Incidence Rate(/100,000pop)")+
  geom_line(aes(y=l, group=scen),col="green")+
  geom_line(aes(y=u, group=scen),col="green")+
  geom_point(data=FitDataI,aes(x=2010,y=FitDataI$I),colour="red",size=3)
#need to add in shading instead of green lines
vpl3<-vpl3+geom_errorbar(aes(x=2010,ymin=cii[1,1], ymax=cii[2,1]),width=0.5,colour="red")

vpl3
