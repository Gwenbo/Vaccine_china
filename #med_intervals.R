### OUTCOME and CREDIBLE INTERVAL PLOTS ###

library(ggplot2)
library(data.table)
### need xoutplot for 2000-2050 for the baseline. Dont need to do for all 1m, just for best 20, then change below to not have top_L as have already selected them.

setwd(clusteroutput)

smallxout<-c()
fityrs<-seq(2000,2050,1)
xoutplot_top<-matrix(0,(20*(length(fityrs))),19)
colnames(xoutplot_top)<-c("run","type","vxint","year","TBPbtot", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")
xoutplot_top<-as.data.frame(xoutplot_top)

top_L_a<-top_L[c(-3,-11)]
top_L_b<-top_L[c(3,11)]
top_L_a <-data.frame(ID = top_L_a)
top_L_b <-data.frame(ID = top_L_b)

top_L_a_split <- t(sapply(top_L_a$ID, function(x) substring(x, first=c(1,4), last=c(3,6))))
top_L_b_split <- t(sapply(top_L_b$ID, function(x) substring(x, first=c(1,3), last=c(2,5))))

tla<-cbind(top_L_a, top_L_a_split)
tlb<-cbind(top_L_b, top_L_b_split)
top_L_20<-rbind(tla,tlb)
top_L_20<-as.matrix(top_L_20)
top_L_20<-as.numeric(top_L_20)
dim(top_L_20)<-c(20,3)

for (jj in 1:20){
  
  mm<-top_L_20[jj,2]
  nn<-top_L_20[jj,3]
  run<-top_L_20[jj,1]
  sxout<-as.data.frame(fread(paste("xout_",(mm+1301),".csv",sep='')),check.names=TRUE)
  smallxout<-sxout[(((302*(nn-1)+1)):(302*nn)),]
  smallxout<-cbind(run,smallxout)
    
  ##matrix of run outputs for plotting (starts at 2000-10 for now, but will need from 1990-2050 later)
  xoutplot_top[((1+((jj-1)*(length(fityrs)))):(jj*(length(fityrs)))),]<-smallxout[which(smallxout[,"year"]%in%fityrs),c("run","type","vxint","year","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
    
}

write.table(xoutplot_top,"xoutplot_top20_base.csv",sep=",",row.names=FALSE)


##select just one outcome, and format it to be the correct dimension (each column is a unique run and each row is a year)
## For a model output which is a time-course use the following to calculate a median or CI etc (just set prob to what you want)




## all ages (or 15+ for prev)
mall_xout<-xoutplot_top[,"TBMtot"]
dim(mall_xout)<-c((length(fityrs)),(n_p*numjobs))
model_mm=apply(mall_xout[,top_L],1,function(x) quantile(x,probs=c(0.5))) 
model_um=apply(mall_xout[,top_L],1,function(x) quantile(x,probs=c(0.975))) 
model_lm=apply(mall_xout[,top_L],1,function(x) quantile(x,probs=c(0.025))) 




model_mm
model_um
model_lm

pall_xout<-xoutplot_top[,"TBPb15+"]
dim(pall_xout)<-c((length(fityrs)),(n_p*numjobs))
model_mp=apply(pall_xout[,top_L],1,function(x) quantile(x,probs=c(0.5))) 
model_up=apply(pall_xout[,top_L],1,function(x) quantile(x,probs=c(0.975))) 
model_lp=apply(pall_xout[,top_L],1,function(x) quantile(x,probs=c(0.025))) 

nall_xout<-xoutplot_top[,"TBNtot"]
dim(nall_xout)<-c((length(fityrs)),(n_p*numjobs))
model_mn=apply(nall_xout[,top_L],1,function(x) quantile(x,probs=c(0.5))) 
model_un=apply(nall_xout[,top_L],1,function(x) quantile(x,probs=c(0.975))) 
model_ln=apply(nall_xout[,top_L],1,function(x) quantile(x,probs=c(0.025))) 

iall_xout<-xoutplot_top[,"TBItot"]
dim(iall_xout)<-c((length(fityrs)),(n_p*numjobs))
model_mi=apply(iall_xout[,top_L],1,function(x) quantile(x,probs=c(0.5))) 
model_ui=apply(iall_xout[,top_L],1,function(x) quantile(x,probs=c(0.975))) 
model_li=apply(iall_xout[,top_L],1,function(x) quantile(x,probs=c(0.025))) 


##Plotting

# ###over time, need xout?? NO as need to plot the fitted ones.
par(mfrow=c(1,1))

#ggplot needs data frame 
xout<-as.data.frame(xout)
FitDataP<-as.data.frame(FitDataP)
cip<-as.data.frame(cip)
FitDataM<-as.data.frame(FitDataM)
cim<-as.data.frame(cim)
FitDataN<-as.data.frame(FitDataN)
cin<-as.data.frame(cin)
FitDataI<-as.data.frame(FitDataI)
cii<-as.data.frame(cii)



model_p<-cbind(seq(2000,2010,1),model_mp, model_lp,model_up)
colnames(model_p)<-c("year","m","l","u")
model_p<-as.data.frame(model_p)
pall_xout<-as.data.frame(pall_xout)
pl4<-ggplot(data=model_p,aes(x=year))+geom_line(colour="black",aes(y=m))+ 
  xlim(2000,2010)+
  xlab("Years")+
  ylim(0,300)+
  ylab("Prevalence Rate of Bacteriologically Confirmed TB (/100,000pop)")+
  geom_line(aes(y=l),col="green")+
  geom_line(aes(y=u),col="green")+
  geom_point(data=FitDataP,aes(x=2000,y=FitDataP$Overall2000,group=1),colour="red",size=3)+
  geom_point(data=FitDataP,aes(x=2010,y=FitDataP$Overall2010,group=2),colour="red",size=3)
#needs fixing: geom_line(xoutplot$TBPbtot[which.max(L[,3]),],col="blue")
pl4<-pl4+geom_errorbar(aes(x=2000,ymin=cip$Overall[1], ymax=cip$Overall[2]),width=0.5,colour="red")
pl4<-pl4+geom_errorbar(aes(x=2010,ymin=cip$Overall[3], ymax=cip$Overall[4]),width=0.5,colour="red")

pl4

model_m<-cbind(seq(2000,2010,1),model_mm, model_lm,model_um)
colnames(model_m)<-c("year","m","l","u")
model_m<-as.data.frame(model_m)
pl5<-ggplot(data=model_m,aes(x=year))+geom_line(colour="black",aes(y=m))+ 
  xlim(2000,2010)+
  xlab("Years")+
  ylim(0,20)+
  ylab("Mortality Rate Overall (/100,000pop)")+
  geom_line(aes(y=l),col="green")+
  geom_line(aes(y=u),col="green")+
  geom_point(data=FitDataM,aes(x=2010,y=FitDataM$Overall,group=1),colour="red",size=3)
#geom_line(xoutplot$TBMbtot[which.max(L[,3]),],col="blue")
pl5<-pl5+geom_errorbar(aes(x=2010,ymin=cim$Overall[1], ymax=cim$Overall[2]),width=0.5,colour="red")

pl5

model_n<-cbind(seq(2000,2010,1),model_mn, model_ln,model_un)
colnames(model_n)<-c("year","m","l","u")
model_n<-as.data.frame(model_n)
pl6<-ggplot(data=model_n,aes(x=year))+geom_line(colour="black",aes(y=m))+ 
  xlim(2000,2010)+
  xlab("Years")+
  ylim(0,300)+
  ylab("Notification Rate Overall (/100,000pop)")+
  geom_line(aes(y=l),col="green")+
  geom_line(aes(y=u),col="green")+
  geom_point(data=FitDataN,aes(x=2010,y=FitDataN$Overall,group=1),colour="red",size=3)
#geom_line(xoutplot$TBNtot[which.max(L[,3]),],col="blue")
pl6<-pl6+geom_errorbar(aes(x=2010,ymin=cin$Overall[1], ymax=cin$Overall[2]),width=0.5,colour="red")

pl6


model_i<-cbind(seq(2000,2010,1),model_mi, model_li,model_ui)
colnames(model_i)<-c("year","m","l","u")
model_i<-as.data.frame(model_i)
pl7<-ggplot(data=model_i,aes(x=year))+geom_line(colour="black",aes(y=m))+ 
  xlim(2000,2010)+
  xlab("Years")+
  ylim(0,300)+
  ylab("Incidence Rate Overall (/100,000pop)")+
  geom_line(aes(y=l),col="green")+
  geom_line(aes(y=u),col="green")+
  geom_point(data=FitDataI,aes(x=2010,y=FitDataI[,1],group=1),colour="red",size=3)
#geom_line(xoutplot$TBNtot[which.max(L[,3]),],col="blue")
pl7<-pl7+geom_errorbar(aes(x=2010,ymin=cii[1,1], ymax=cii[2,1]),width=0.5,colour="red")

pl7





par(mfrow=c(2,2))
pl4
pl5
pl6
pl7

colnames(pall_xout)<-seq(1,500000,1)
rownames(pall_xout)<-seq(2000,2010,1)

matplot(model[,unique_t], type="l",col="grey",ylim=c(0,30))     # plot resampled runs as grey lines
points(dat,col="red")                  # plot the data as red dots
lines(model[,which.max(L)],col="red")   # plot the best fit as a red line
lines(model_m,col="black",lty=2)        # plot the median and 95% CI
lines(model_u,col="black")      
lines(model_l,col="black")    

# lines(xout$TBPbtot[,which.max(L)],col="red")   # plot the best fit as a red line
# lines(model_m,col="black",lty=2)        # plot the median and 95% CI
# lines(model_u,col="black")      
# lines(model_l,col="black")    

#xout[(((yearend-year1+1)*2*kkk*nmbr)-(2*(yearend-year1+1)-1)):(2*(yearend-year1+1)*kkk*nmbr),]<-cbind(Xn,times,year,nn,count,kkk)

# Structure with psize row1, hiv- row2, hiv+ row3
# blankPanel<-grid.rect(gp=gpar(col="white")) 
# # save in plots folder
# setwd(home);setwd("Output/A.plots")
# pdf(paste(cntry,"fits.pdf"))
# grid.arrange(pp,blankPanel,qq11,qq2,qq3,qq4, ncol=2)
# dev.off()


