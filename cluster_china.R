#### Run model on cluster ####

# Will be run as batches of 1000 on cluster as takes about an hour or so to run this
# Will run model on cluster, then clac likelihood, resample etc on computer

#### Baseline model fitting #####

#setting home working drive - C=0 if on laptop, 1 if on cluster
C=1
if (C == 0){home<-"/Users/Rebecca/Vaccine_china"}
if (C == 1){home<-"/home/lsh355020/China/"}
setwd(home)

#call in data and prep Cfunction
source('#DataGrab.R')
setwd(home)
source('CFunctions.R')
cntry<-"China"
nm<-c(pararange[,1],"p0") # The parameter ranges

### Generate Parameter Sets ###

##number of parameter sets - will increase later
n_p<-3

#cluster job/task number
if (C==0){job<-0}
if (C==1){job<-as.numeric(Sys.getenv("SGE_TASK_ID"))}

##generate matrix for storing parameter sets (later will want to store these as the file that you call for the model)

randparam<-mat.or.vec(n_p,(length(nm)+1))
colnames(randparam)<-c('set',nm)
#number the param sets
#randparam[1:n_p,1]<-rep(job,n_p)
randparam[1:n_p,1]<-seq(1,n_p,1)


#generate random numbers within param ranges - will need to change if decide some not going to be uniform prior
for (dd in 1:(length(nm)-1)){
  randparam[1:n_p,dd+1]<-runif(n_p,as.numeric(pararange[dd,2]),as.numeric(pararange[dd,3]))
}

## setting limits on age wise parameters to ensure are bigger/smaller than for adults

for (dd in 1:n_p){
  # to replace pelderly with a randomly selected number that is equal to or higher than padult
  randparam[dd,"pelderly"]<-runif(1,randparam[dd,"padult"],as.numeric(pararange[9,3]))
  
  # to replace felderly with a randomly selected number that is equal to or lower than fadult
  randparam[dd,"felderly"]<-runif(1,as.numeric(pararange[17,2]),randparam[dd,"fadult"])
 
  # to replace uiscaleE and uiscaleC with a randomly selected number that is equal to or higher than uiscaleA
  randparam[dd,"uiscaleE"]<-runif(1,randparam[dd,"uiscaleA"],as.numeric(pararange[24,3]))
  randparam[dd,"uiscaleC"]<-runif(1,randparam[dd,"uiscaleA"],as.numeric(pararange[12,3]))
  
  # to replace nelderly with a randomly selected number that is equal to or lower than n. (should i limit upper range??)
  randparam[dd,"nelderly"]<-runif(1,as.numeric(pararange[21,2]),randparam[dd,"n"])
  
  # to replace relderly with a randomly selected number that is equal to or higher than radult
  randparam[dd,"relderly"]<-runif(1,randparam[dd,"radult"],as.numeric(pararange[13,3]))

  # to replace velderly with a randomly selected number that is equal to or higher than vadult
  randparam[dd,"velderly"]<-runif(1,randparam[dd,"vadult"],as.numeric(pararange[22,3]))
  
  # to replace CDRscaleE with a randomly selected number that is equal to or lower than CDRscale
  randparam[dd,"CDRscaleE"]<-runif(1,as.numeric(pararange[26,2]),randparam[dd,"CDRscale"])
  
  }


head(randparam)
tail(randparam)

#enter value for P0
randparam[1:n_p,(length(nm)+1)]<-rep(1267142,n_p) ##insert p0 value here (1267142 from manual model fit???)##

#randparam needs to be saved as paraout_China.csv in data file
setwd(home);setwd("Data")
write.table(randparam,paste("paraout_",cntry,"_",job,".csv",sep=''),sep=",",row.names=FALSE) #sep within paste tells how the elements should be separated (baseline is to assume space), and in write.table sep is to say how the data are separated


#read parameters back in without numbering
setwd(home);setwd("Data")
#can be slow, if doing evry time change to not read back in
para<-read.csv(paste("paraout_",cntry,"_",job,".csv",sep=''))[-1]
setwd(home)

#timestep, start and end year
dt<-(1/2)
year1<-1900
yearend<-2050

typen<-0 #temporary
year<-c(seq(year1,yearend,1),rep(0,((1/dt)*(yearend-year1+1)-(yearend-year1+1))))

count<-1 #remove once doing vacc scenarios
nn<-1 #remove once doing vacc scenarios
nmbr<-count*nn #use this once doing vaccine scenarios
xout<-mat.or.vec(((yearend-year1+1)*(1/dt)*n_p*(typen+1)),109) #(94+run number+vacc type+vacc effic+10*TBRa/i) would need changing if extra outputs added


# Simulate data 

for (kkk in 1:n_p)
{
  for (i in 1:(length(nm)+1)){assign(nm[i],as.numeric(para[kkk,i]))} # Assign the parameters to the correct values
  neta2<-neta # this parameter needs extra assigning for some annoying reason! 
  # Run the model with these parameters  
  TIME<-system.time(Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,dt,c(0.02,0.02,0.8,0.07)),c(year1,yearend),0,C))   
  xout[((((yearend-year1+1)*(1/dt)*kkk*nmbr)-((1/dt)*(yearend-year1+1)-1)):((1/dt)*(yearend-year1+1)*kkk*nmbr)),]<-cbind(Xn,times,year,(rep(nn,length(times))),(rep(count,length(times))),(rep(kkk,length(times))))
   
}

head(xout)
#Adding cluster job number to xout
JOB<-rep(job,n_p)
JOB<-as.data.frame(JOB)
xout<-as.data.frame(xout)
xout<-cbind(xout,JOB)
colnames(xout)<-c(colnames(Xn),"timestep","year","type","vxint","fit","job") #timestep is for params where given by timestep, year is for where output is summary of annual
    
head(xout)
tail(xout)
#write results to csv file on the cluster
setwd(home);setwd("Output")
getwd()
write.table(xout,paste("xout","_",job,".csv",sep=""),sep=",",row.names=FALSE)
            