

#what is the likelihood of that parameter given the data (i.e. the prev etc that we're trying to fit to)

#first need to call in data to fit to

FitDataP<-matrix(c(178,92,119,213,596,116,59,73,133,346),nrow=1, ncol=10, byrow=TRUE)
colnames(FitDataP)<-c("Overall(15+)2000","15-29 years2000","30-44 years2000","45-59 years2000","≥60 years2000","Overall(15+)","15-29 years","30-44 years","45-59 years","≥60 years")

FitDataN<-matrix(c(63.91,2.72,64.62,104.36,143.07),nrow=1, ncol=5, byrow=TRUE)
colnames(FitDataN)<-c("Overall","0-14 years","15-54 years","55-64 years","≥65 years")

FitDataM<-matrix(c(4.69,0.29,1.91,15.69),nrow=1, ncol=4, byrow=TRUE)
colnames(FitDataM)<-c("Overall","0-14 years","15-59 years","≥60 years") 

FitDataI<-matrix(78)

FitData<-cbind(FitDataP, FitDataN, FitDataM, FitDataI)

#variance of data from 95% CIs

cip<-matrix(c(163,72,96,174,510,195,116,146,260,698,101,40,54,106,294,132,86,99,168,407),nrow=4, ncol=5, byrow=TRUE)
colnames(cip)<-c("Overall(15+)","15-29 years","30-44 years","45-59 years","≥60 years")
rownames(cip)<-c("lower2000","upper2000","lower2010","upper2010") 

##lower limit is the data, so assume mormally distrib with lower limit as 95% CI
cin<-matrix(c(63.91,2.72,64.62,104.36,143.07,79.89,3.40,80.78,130.45,178.84),nrow=2, ncol=5, byrow=TRUE)
colnames(cin)<-c("Overall","0-14 years","15-54 years","55-64 years","≥65 years")
rownames(cin)<-c("lower2010","upper2010")
                 
cim<-matrix(c(4.54,0.27,1.72,14.12,4.84,0.32,2.10,17.26),nrow=2, ncol=4, byrow=TRUE)
colnames(cim)<-c("Overall","0-14 years","15-59 years","≥60 years")
rownames(cim)<-c("lower2010","upper2010") 

cii<-c(68,88)



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

# var<-mat.or.vec(1,20)
# var[1,]<-sdp^2
# var[2,]<-sdp2^2
# var[3,]<-sdn^2
# var[4,1:4]<-sdm^2
# var[5,1]<-sdi^2
# rownames(var)<-c("Prev2000","Prev2010","Notif2010","Mort2010","Inc2010") 

# have model outcomes to be fitted to in easily readible format 
new00<-xout[which(xout[,"year"]%in%2000),c("type","vxint","fit","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+")]
colnames(new00)<-c("type","vxint","fit","TBPb15+2000", "TBPb15-29_2000", "TBPb30-44_2000", "TBPb45-59_2000", "TBPb60+_2000")
new10<-xout[which(xout[,"year"]%in%2010),c("TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
neww<-cbind(new00,new10)
View(neww)
#neww<-xout[which(xout[,"year"]%in%c(2000,2010)),c("type","vxint","fit","year","TBPb15+", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBPb60+","TBNtot","TBN0-14","TBN15-54","TBN55-64","TBN65+","TBMtot","TBM0-14","TBM15-59","TBM60+","TBItot")]
#colnames(neww)<-NULL
neww<-neww[,4:23]

## then calc likelihood of that parameter given the data (i.e. the prev etc that we're trying to fit to)

#likelihood calc
# L<-rep(0,n_p)
# for (i in 1:n_p){
#   L[i] <- prod(((2*pi*var)^(-1/2))*exp(-(1/(2*var))*(((neww[i,])-FitData)^2)))
# }

#had to switch to log likelihood as likelihood becomes too small beyond approx 17 terms and just gives zero, so need to use log likelihood as adds rather than multiplies
###HOW DOES USING LOG LIKELIHOOD affect how you use these numbers? +ive vs negative log likelihood??
### HAVE NEGATIVE values, what do about this???

L<-rep(0,n_p)
for (i in 1:n_p){
  L[i] <- sum((-0.5*log(2*pi))-(0.5*log(var))-((((neww[i,])-FitData)^2)/(2*var)))
}

L



### Sample the runs with a weight based upon the calculated likelihood of that run's parameters

library(gdata)

N_resamp<-10000 # number of samples to take

#### kick out everything with negative model outputs??  if so drop those where negative, and in sample need to have seq length N_p minus those deleted###

#resample with weights based upon likelihodds
# t will be a vector of the indices of the resampled parameter sets
t<-sample(seq(1:n_p),N_resamp,replace=TRUE,prob=L/sum(L))
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
  nam<-paste(pararange[i,1],"_posterior",sep='')
  tpar<-as.numeric(para[t,i])
  assign(nam,tpar)
  plot(density(tpar),xlim=c(as.numeric(pararange[i,2]),as.numeric(pararange[i,3]))) # plot the density of the posterior
  lines(density(para[,i]))  # plot the density of the prior (a flat line (ish as density smooths things) as it was uniform)
}


# par(mfrow=c(1,1))
# 
# matplot(neww[,1], type="l",col="grey",ylim=c(0,30))     # plot all 1000 outputs as grey lines
# points(FitData[,1],col="red")                  # plot the data as red dots
# lines(model[,which.max(L)],col="red")   # plot the best fit as a red line
# lines(model_m,col="black",lty=2)        # plot the median and 95% CI
# lines(model_u,col="black")      
# lines(model_l,col="black")    

