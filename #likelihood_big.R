#### Calc of likelihoods following on from parasamp.R or cluster_china.R  ####
home<-"/Users/Rebecca/Vaccine_china" 
output<-"/Users/Rebecca/Vaccine_china/Outputs/Cluster_outputs" 

###insert date of cluster run
clusteroutput<-"/Users/Rebecca/Vaccine_china/Outputs/Cluster_outputs/150903"
setwd(home)

library(ggplot2)
library(data.table)
suppressWarnings(library('gdata')) ## For drop.levels function


#make sure have read in the para range
setwd(home);setwd("Data")
pararange <- as.matrix(drop.levels(read.csv('pararanges.csv',header=TRUE,check.names=F)))       # Number of parameters same for all countries - CHECK with new model
setwd(home)

#number of runs in each job
n_p<-1000
#number of jobs
numjobs<-200

#what is the likelihood of that parameter given the data (i.e. the prev etc that we're trying to fit to)

#first need to call in data to fit to

FitDataP<-matrix(c(178,92,119,213,596,116,59,73,133,346),nrow=1, ncol=10, byrow=TRUE)
colnames(FitDataP)<-c("Overall2000","1529years2000","3044years2000","4559years2000","60plusyears2000","Overall2010","1529years10","3044years10","4559years10","60plusyears10")

FitDataN<-matrix(c(63.91,2.72,64.62,104.36,143.07),nrow=1, ncol=5, byrow=TRUE)
colnames(FitDataN)<-c("Overall","0-14 years","15-54 years","55-64 years","≥65 years")

FitDataM<-matrix(c(4.69,0.29,1.91,15.69),nrow=1, ncol=4, byrow=TRUE)
colnames(FitDataM)<-c("Overall","0-14 years","15-59 years","≥60 years") 

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


#calc variance of data from CIs assuming normal distrib (therefore can use 1.96sd=95% CIs)
### sd from lower and upper is different so not normally distrib, so how use this??
sdpl<-(FitDataP[,1:5]-cip[1,])/1.96
sdpu<-(cip[2,]-FitDataP[,1:5])/1.96
sdp<-(sdpl+sdpu)/2
sdp2l<-(FitDataP[,6:10]-cip[3,])/1.96
sdp2u<-(cip[4,]-FitDataP[,6:10])/1.96
sdp2<-(sdp2l+sdp2u)/2

sdnl<-(FitDataN[1,]-cin[1,])/1.96
sdnu<-(cin[2,]-FitDataN[1,])/1.96
sdn<-(sdnl+sdnu)/2

sdml<-(FitDataM[1,]-cim[1,])/1.96
sdmu<-(cim[2,]-FitDataM[1,])/1.96
sdm<-(sdml+sdmu)/2

sdi<-(FitDataI-cii[1])/1.96

var<-c((sdp^2),(sdp2^2),(sdn^2),(sdm^2),(sdi^2))

### bind together cluster parameter sets 
setwd(clusteroutput)
xout<-c()
para<-c()

for (uu in 1:numjobs){
  print(uu)
  paranxt<-fread(paste("paraout_China_",uu,".csv",sep=''))
  para<-rbind(para,paranxt)
  print(uu)  
}

# for (uu in 1:numjobs){
#   print(uu)
#   xoutnxt<-fread(paste("xout_",uu,".csv",sep=''))
#   xout<-rbind(xout,xoutnxt)
#   print(dim(xout)) 
# }

write.table(para,"para_clustermerge.csv",sep=",",row.names=FALSE)


setwd(home)


## then calc likelihood of that parameter given the data (i.e. the prev etc that we're trying to fit to)

## Calc L without making xout one big data frame

setwd(clusteroutput)

# ## TEMPORARY - to replace first line of random parameters with results of manual fit
# manufit<-c()
# clusterdoc<-c()
# manufit<-read.csv("xoutmanufit_1.csv",check.names=FALSE)
# manufit<-as.data.frame(manufit)
# clusterdoc<-read.csv("xout_1.csv",check.names=FALSE)
# clusterdoc<-as.data.frame(clusterdoc)
# clusterdoc[1:302,]<-manufit[1:302,]
# write.csv(clusterdoc,"xout_1.csv",row.names=FALSE)
# 


smallxout<-c()
fityrs<-seq(2000,2010,1)
xoutplot<-matrix(0,(n_p*numjobs*(length(fityrs))),20)
colnames(xoutplot)<-c("job","fit","type","vxint","year","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")
xoutplot<-as.data.frame(xoutplot)
L<-matrix(0,(n_p*numjobs),3)
colnames(L)<-c("job","fit","L")
LP<-rep(0,(n_p*numjobs))
LN<-rep(0,(n_p*numjobs))
LM<-rep(0,(n_p*numjobs))
LI<-rep(0,(n_p*numjobs))


for (jj in 1:numjobs){
  
  smallxout<-as.data.frame(fread(paste("xout_",jj,".csv",sep='')),check.names=TRUE)
  
  ##matrix of run outputs for plotting (starts at 2000-10 for now, but will need from 1990-2050 later)
  xoutplot[((1+((jj-1)*n_p*(length(fityrs)))):(jj*n_p*(length(fityrs)))),]<-smallxout[which(smallxout[,"year"]%in%fityrs),c("job","fit","type","vxint","year","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
  
  
  #xoutplot<-subset(smallxout, year>=2000 & year<=2010)
  
  ##matrix of run outputs for calc of Likelihood
  new00<-smallxout[which(smallxout[,"year"]%in%2000),c("job","fit","type","vxint","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+")]
  colnames(new00)<-c("job","fit","type","vxint","TBPb15+2000", "TBPb15-29_2000", "TBPb30-44_2000", "TBPb45-59_2000", "TBPb60+_2000")
  new10<-smallxout[which(smallxout[,"year"]%in%2010),c("TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
  neww<-cbind(new00,new10)
  neww<-neww[,5:24]
  
  L[((jj-1)*1000+1):(jj*1000),1]<-jj
  
  for (i in 1:n_p){
    #calc likelihoods
    L[(i+((jj-1)*1000)),2]<-i
    L[(i+((jj-1)*1000)),3] <- -1/(sum((-0.5*log(2*pi*var))-((((neww[i,])-FitData)^2)/(2*var))))
    #breakdown of likelihood
    LP[(i+((jj-1)*1000))] <- -1/(sum((-0.5*log(2*pi*var[1:10]))-((((neww[i,1:10])-FitData[1:10])^2)/(2*var[1:10]))))
    LN[(i+((jj-1)*1000))] <- -1/(sum((-0.5*log(2*pi*var[11:15]))-((((neww[i,11:15])-FitData[11:15])^2)/(2*var[11:15]))))
    LM[(i+((jj-1)*1000))] <- -1/(sum((-0.5*log(2*pi*var[16:19]))-((((neww[i,16:19])-FitData[16:19])^2)/(2*var[16:19]))))
    LI[(i+((jj-1)*1000))] <- -1/(sum((-0.5*log(2*pi*var[20]))-((((neww[i,20])-FitData[20])^2)/(2*var[20]))))
    
    #identify negs and NAs and change Likelihood to zero so they are not selected 
    test<-matrix(1,1,20)
    #if data are NA, missing will be FALSE
    missing<-complete.cases(L[(i+((jj-1)*1000)),3])
    #if NA, will be left as 1s, if is not NA (i.e. missing==TRUE, then check whether negatives and if not negatives replace with zeros)
    if(missing==TRUE) {test[1,]<-ifelse(neww[i,]<0,test[1,]==1,test[1,]==0)}
    #add up the negs test across the output variables
    negs<-rowSums(test)
    #negs true if outputs are negative or NA, false if outputs are all positive
    negs<-ifelse(negs > 0,"TRUE","FALSE")
    negs<-as.logical(negs)
    if (negs==TRUE) {L[(i+((jj-1)*1000)),3]<-0} 
  }
  print(jj)
}

#at the moment the first xout file has job 0 as inlcudes manual fit, so need to replace 0 with 1
xoutplot[1:(n_p*(length(fityrs))),1]<-rep(1,(n_p*(length(fityrs))))

#need to add column to be able to identify which links to which value of L
run_count<-rep(seq(1,(n_p*numjobs),1),each=length(fityrs))
xoutplot<-cbind(run_count, xoutplot)

write.table(xoutplot,"xout_clustermerge0010.csv",sep=",",row.names=FALSE)


## Output which runs are the highest likelihood runs
top_L<-order(L[,3],decreasing=TRUE)
top_L<-top_L[1:10]
top_L

top_LM<-order(LM,decreasing=TRUE)
top_LM<-top_LM[1:10]
top_LM

top_LP<-order(LP,decreasing=TRUE)
top_LP<-top_LP[1:10]
top_LP

top_LN<-order(LN,decreasing=TRUE)
top_LN<-top_LN[1:10]
top_LN

top_LI<-order(LI,decreasing=TRUE)
top_LI<-top_LI[1:10]
top_LI

### Sample the runs with a weight based upon the calculated likelihood of that run's parameters

library(gdata)

N_resamp<-200000 # number of samples to take

#### kick out everything with negative model outputs??  if so drop those where negative, and in sample need to have seq length N_p minus those deleted###

#resample with weights based upon likelihodds
# t will be a vector of the indices of the resampled parameter sets
t<-sample(seq(1:(n_p*numjobs)),N_resamp,replace=TRUE,prob=L[,3])
tM<-sample(seq(1:(n_p*numjobs)),N_resamp,replace=TRUE,prob=LM)
# This just pulls out the unique values of t
unique_t<-unique(t)
table(t)


#keep only the xoutplot for the unique resampled runs
unique_xout<-xoutplot[which(xoutplot[,"run_count"]%in%unique_t),]

#select just one outcome, and format it to be the correct dimension (each column is a run and each row is a year)
mall_xout<-xoutplot[,"TBMtot"]
dim(mall_xout)<-c((length(fityrs)),100000)

pall_xout<-xoutplot[,"TBPb15+"]
dim(pall_xout)<-c((length(fityrs)),100000)

## For a model output which is a time-course use the following to calculate a median or CI etc (just set prob to what you want)
model_m=apply(mall_xout[,t],1,function(x) quantile(x,probs=c(0.5))) 
model_u=apply(mall_xout[,t],1,function(x) quantile(x,probs=c(0.975))) 
model_l=apply(mall_xout[,t],1,function(x) quantile(x,probs=c(0.025))) 

model_mp=apply(pall_xout[,t],1,function(x) quantile(x,probs=c(0.5))) 
model_up=apply(pall_xout[,t],1,function(x) quantile(x,probs=c(0.975))) 
model_lp=apply(pall_xout[,t],1,function(x) quantile(x,probs=c(0.025))) 

model_m
model_u
model_l





# 
# # Then a crude plot would be 
# 
# plot(temp[,1],ylim=c(0,0.5),col="blue")
# points(temp[,2],col="red")
# points(temp[,3],col="green")
# 
# lines(P_m,lty=2)
# lines(P_l)
# lines(P_u)



##Plotting

#prior vs posterior plots

for (i in 1:(length(nm)-1))
{
  nam<-paste(pararange[i,1]," posterior",sep='')
  tpar<-as.numeric(para[t,i])
  assign(nam,tpar)
  plot(density(tpar),xlim=c(as.numeric(pararange[i,2]),as.numeric(pararange[i,3])),main=nam, col="red") # plot the density of the posterior
  lines(density(para[,i]))  # plot the density of the prior (a flat line (ish as density smooths things) as it was uniform)
}


## Scatter plots

#prev 2010 vs parameters

for (i in 1:(length(nm)-1))
{
  #   nam<-paste(pararange[i,1]," vs Prevalence rate 2000",sep='')
  #   plot(para[,i],neww[,1],ylab="Prevalence rate (2000)",xlab=paste(pararange[i,1]),type='p', main=nam)
  #   
  #   nam<-paste(pararange[i,1]," vs Prevalence rate 2010",sep='')
  #   plot(para[,i],neww[,6],ylab="Prevalence rate (2010)",xlab=paste(pararange[i,1]), type='p', main=nam)
  # 
  #   nam<-paste(pararange[i,1]," vs Notification rate 2010",sep='')
  #   plot(para[,i],neww[,11],ylab="Notification rate (2010)",xlab=paste(pararange[i,1]), type='p', main=nam)
  #   
  nam<-paste(pararange[i,1]," vs Mortality rate 2010",sep='')
  plot(para[,i],neww[,16],ylab="Mortality rate (2010)",xlab=paste(pararange[i,1]), type='p', main=nam)
  
  #   nam<-paste(pararange[i,1]," vs Incidence rate 2010",sep='')
  #   plot(para[,i],neww[,20],ylab="Incidence rate (2010)",xlab=paste(pararange[i,1]), type='p', main=nam)
  #   
}


# ###over time, need xout?? NO as need to plot the fitted ones.
par(mfrow=c(1,1))

#ggplot needs data frame 
xout<-as.data.frame(xout)
FitDataP<-as.data.frame(FitDataP)
cip<-as.data.frame(cip)


#group cant just be fint once appended, which.max L doesnt work with this
pl<-ggplot(data=xoutplot,aes(x=year,y=TBPbtot,group=fit))+geom_line(colour="black",aes(y=TBPbtot))+
  xlim(2000,2010)+
  xlab("Years")+
  ylim(0,500)+
  ylab("Prevalence Rate of Bacteriologically Confirmed TB (/100,000pop)")+
  geom_point(data=FitDataP,aes(x=2000,y=FitDataP$Overall2000,group=1),colour="red",size=3)+
  geom_point(data=FitDataP,aes(x=2010,y=FitDataP$Overall2010,group=2),colour="red",size=3)+
  geom_line(xoutplot$TBPbtot[which.max(L[,3]),],col="red")
pl<-pl+geom_errorbar(aes(x=2000,ymin=cip$Overall[1], ymax=cip$Overall[2]),width=0.5,colour="red")
pl<-pl+geom_errorbar(aes(x=2010,ymin=cip$Overall[3], ymax=cip$Overall[4]),width=0.5,colour="red")

pl
##need to use neww???? no coz neww is 2000 and 2010 for each run only. 
##Or select xout more carefully as is every 151??
# t is the run number, so need to select all rows of xout where fit= a value of unique t

xoutfit<-xout[which(xout[,"fit"]==unique_t)]
xoutfit<-xout[which(xout$fit==unique_t)]
xoutfit<-subset(xout,xout$fit==unique_t)
unique_t<-sort(unique_t)

pl2<-ggplot(data=xout[xout$fit%in%unique_t,],aes(x=year,y=TBPbtot,group=fit))+geom_line(colour="black")+
  xlim(1995,2015)+
  xlab("Years")+
  ylim(0,500)+
  ylab("Prevalence Rate of Bacteriologically Confirmed TB (/100,000pop)")+
  geom_point(data=FitDataP,aes(x=2000,y=FitDataP$Overall2000,group=1),colour="red",size=5)+
  geom_point(data=FitDataP,aes(x=2010,y=FitDataP$Overall2010,group=2),colour="red",size=5)+
  geom_line(data=xout[xout$fit==which.max(L[,3]),],col="red")

pl2

par(mfrow=c(1,3))
pl
pl2
pl3

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




