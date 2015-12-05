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
#colnames(var)<-c("TBPb15+2000","TBPb15-29_2000","TBPb30-44_2000","TBPb45-59_2000","TBPb60+_2000","TBPb15+","TBPb15-29","TBPb30-44","TBPb45-59","TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")
#neww<-read.csv("neww.csv")

### bind together cluster parameter sets and outputs

#insert number of cluster jobs below
setwd(clusteroutput)
numjobs<-100
xout<-matrix(0,(numjobs*302000),100)
colnames(xout)<-c(colnames(Xn),"timestep","year","type","vxint","fit","job") #timestep is for params where given by timestep, year is for where output is summary of annual
para<-c()
for (uu in 1:numjobs){
    xout[((((uu-1)*302000)+1):(uu*302000)),]<-fread(paste("xout_",uu,".csv",sep=''))
    print(uu)
    }

write.table(xout,"xout_clustermerge.csv",sep=",",row.names=FALSE)

for (uu in 1:numjobs){
  print(uu)
  paranxt<-fread(paste("paraout_China_",uu,".csv",sep=''))
  para<-rbind(para,paranxt)
  print(uu)  
  }

write.table(para,"para_clustermerge.csv",sep=",",row.names=FALSE)


setwd(home)

### For use when testing model on cluster
# setwd(output)
# xout<-read.csv("xoutmanufit_1.csv", header=TRUE, check.names=FALSE)
# xout<-as.data.frame(xout)
# 
# #graphs to check what run by cluster looks ok when chekcing against original fit
# setwd(home)
# source('#graphs_cluster_check.R')

# have model outcomes to be fitted to in easily readible format 
new00<-xout[which(xout[,"year"]%in%2000),c("job","fit","type","vxint","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+")]
colnames(new00)<-c("job","fit","type","vxint","TBPb15+2000", "TBPb15-29_2000", "TBPb30-44_2000", "TBPb45-59_2000", "TBPb60+_2000")
new10<-xout[which(xout[,"year"]%in%2010),c("TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
neww<-cbind(new00,new10)

#for testing one run
year<-c(seq(year1,yearend,1),rep(0,((1/dt)*(yearend-year1+1)-(yearend-year1+1))))
xout<-cbind(Xn,year)
new00<-xout[which(xout[,"year"]%in%2000),c("TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+")]
colnames(new00)<-c("TBPb15+2000", "TBPb15-29_2000", "TBPb30-44_2000", "TBPb45-59_2000", "TBPb60+_2000")
new10<-xout[which(xout[,"year"]%in%2010),c("TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
neww<-cbind(new00,new10)

View(neww)
#neww<-xout[which(xout[,"year"]%in%c(2000,2010)),c("type","vxint","fit","year","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
#colnames(neww)<-NULL

neww<-neww[,5:24]
setwd(output)
write.table(neww,"neww.csv",sep=",",row.names=FALSE)
setwd(home)


## then calc likelihood of that parameter given the data (i.e. the prev etc that we're trying to fit to)

#likelihood calc
#  L<-rep(0,n_p)
# for (i in 1:n_p){
#    L[i] <- prod(((2*pi*var)^(-1/2))*exp(-(1/(2*var))*(((neww[i,])-FitData)^2)))
#  }

#had to switch to log likelihood as likelihood becomes too small beyond approx 17 terms and just gives zero, so need to use log likelihood as adds rather than multiplies
###HOW DOES USING LOG LIKELIHOOD affect how you use these numbers? +ive vs negative log likelihood??
### HAVE NEGATIVE values, what do about this???

# L<-rep(0,n_p)
# for (i in 1:n_p){
#   L[i] <- sum((-0.5*log(2*pi))-(0.5*log(var))-((((neww[i,])-FitData)^2)/(2*var)))
# }

#the below works for if just one data frame, but not when many from cluster
L<-rep(0,n_p)
for (i in 1:n_p){
  L[i] <- sum((-0.5*log(2*pi*var))-((((neww[i,])-FitData)^2)/(2*var)))
}
L

## trying to calc L without making xout one big data frame
L<-rep(0,(n_p*numjobs))
       
for (jj in 1:numjobs){
       
##have to calc neww for each !!  and in i loop need neww to be the neww for that loop (need ot save of just write over each loop?)
      xout<-as.data.frame(fread(paste("xout_",jj,".csv",sep='') ))
      new00<-xout[which(xout[,"year"]%in%2000),c("job","fit","type","vxint","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+")]
      colnames(new00)<-c("job","fit","type","vxint","TBPb15+2000", "TBPb15-29_2000", "TBPb30-44_2000", "TBPb45-59_2000", "TBPb60+_2000")
      new10<-xout[which(xout[,"year"]%in%2010),c("TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
      neww<-cbind(new00,new10)
      neww<-neww[,5:24]

        for (i in 1:n_p){
          check[i]<-(i+((jj-1)*1000))
          L[(i+((jj-1)*1000))] <- sum((-0.5*log(2*pi*var))-((((neww[i,])-FitData)^2)/(2*var)))
          LLL[i]<- sum((-0.5*log(2*pi*var))-((((neww[i,])-FitData)^2)/(2*var)))
        }
      print(jj)
    }


## for checking whether a particular likelihood is dominating 
LP<-rep(0,n_p)
for (i in 1:n_p){
  LP[i] <- sum((-0.5*log(2*pi*var[1:10]))-((((neww[i,1:10])-FitData[1:10])^2)/(2*var[1:10])))
}

LN<-rep(0,n_p)
for (i in 1:n_p){
  LN[i] <- sum((-0.5*log(2*pi*var[11:15]))-((((neww[i,11:15])-FitData[11:15])^2)/(2*var[11:15])))
}

LM<-rep(0,n_p)
for (i in 1:n_p){
  LM[i] <- sum((-0.5*log(2*pi*var[16:19]))-((((neww[i,16:19])-FitData[16:19])^2)/(2*var[16:19])))
}

LI<-rep(0,n_p)
for (i in 1:n_p){
  LI[i] <- sum((-0.5*log(2*pi*var[20]))-((((neww[i,20])-FitData[20])^2)/(2*var[20])))
}

# ## for checking likelihood of manufit run vs the best run of that batch
# LP[1]
# LP[max]
# LN[1]
# LN[max]
# LM[1]
# LM[max]
# LI[1]
# LI[max]


#need to do this so that the most liekly ones are the biggest number, as log likelihood is best when is the clostest to zero, so times b -1 to make it a positive value, then 1/L to make the best ones the biggest numbers so that works for weighted sampling
L<-(-1/L)

#retunr vector indicating which rows are complete (i.e. no NaNs)
missing<-as.vector(complete.cases(L))
table(missing)
## make those that didnt work zeros so they wont be selected

for(i in 1:length(L))
{
if (missing[i]==FALSE) {L[i]<-0}
}
L

## matirx to indicate  THE NEGATIVEs - if the value of negs is >0 then that parameter set has given at least one negative output
#set up matrix
test<-matrix(1,length(L),20)

#change those without negatives into zeros and turn NAs into zeros so that doesnt stop the if else negs code working
for(kkk in 1:length(L)){
  test[kkk,]<-ifelse(neww[kkk,]<0,test[kkk,]==1,test[kkk,]==0)
  if (missing[kkk]==FALSE) {test[kkk,]<-0}
}

#add up the negs test across the output variables
negs<-rowSums(test)
table(negs)
#negs true if outputs are negative, false if outputs are all positive or NA
negs<-ifelse(negs > 0,"TRUE","FALSE")
negs<-as.logical(negs)


for(i in 1:n_p)
  { 
  if (negs[i]==TRUE) {L[i]<-0} 
  }
L


### Sample the runs with a weight based upon the calculated likelihood of that run's parameters

library(gdata)

N_resamp<-100000 # number of samples to take

#### kick out everything with negative model outputs??  if so drop those where negative, and in sample need to have seq length N_p minus those deleted###

#resample with weights based upon likelihodds
# t will be a vector of the indices of the resampled parameter sets
t<-sample(seq(1:n_p),N_resamp,replace=TRUE,prob=L)
# This just pulls out the unique values of t
unique_t<-unique(t)
table(t)

## For a model output which is a time-course you can use the following to calculate a median or CI etc (just set prob to what you want)
model_m=apply(neww[t,],1,function(x) quantile(x,probs=c(0.5))) 
model_u=apply(neww[t,],1,function(x) quantile(x,probs=c(0.975))) 
model_l=apply(neww[t,],1,function(x) quantile(x,probs=c(0.025))) 


#plotting

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



pl<-ggplot(data=xout,aes(x=year,y=TBPbtot,group=fit))+geom_line(colour="black",aes(y=TBPbtot))+
      xlim(1995,2015)+
      xlab("Years")+
      ylim(0,500)+
      ylab("Prevalence Rate of Bacteriologically Confirmed TB (/100,000pop)")+
      geom_point(data=FitDataP,aes(x=2000,y=FitDataP$Overall2000,group=1),colour="red",size=3)+
      geom_point(data=FitDataP,aes(x=2010,y=FitDataP$Overall2010,group=2),colour="red",size=3)+
      geom_line(xout$TBPbtot[which.max(L),],col="red")
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
      geom_line(data=xout[xout$fit==which.max(L),],col="red")

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



