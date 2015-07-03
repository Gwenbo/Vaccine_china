#### Baseline model fitting #####

home<-"/Users/Rebecca/Vaccine_china" # Could also set up and output folder
setwd(home)
source('#DataGrab.R')
cntry<-"China"
nm<-c(pararange[,1],"p0") # The parameter ranges

### Generate Parameter Sets ###

##number of parameter sets - will increase later
n_p<-3

##generate matrix for storing parameter sets (later will want to store these as the file that you call for the model)

randparam<-mat.or.vec(n_p,(length(nm)+1))
colnames(randparam)<-c('set',nm)
#number the param sets
randparam[1:n_p,1]<-seq(1,n_p,1)

#generate random numbers within param ranges - will need to change if decide some not going to be uniform prior
for (dd in 1:(length(nm)-1)){
  randparam[1:n_p,dd+1]<-runif(n_p,as.numeric(pararange[dd,2]),as.numeric(pararange[dd,3]))
  }

#enter value for P0
randparam[1:n_p,(length(nm)+1)]<-rep(1267142,n_p) ##insert p0 value here (1267142 from manual model fit???)##
  
#randparam needs to be saves as paraout_China.csv in data file
setwd(home);setwd("Data")
write.table(randparam,paste("paraout_",cntry,".csv",sep=''),sep=",",row.names=FALSE) #sep within paste tells how the elements should be separated (baseline is to assume space), and in write.table sep is to say how the data are separated


#read parameters back in without numbering
setwd(home);setwd("Data")
#can be slow, if doing evry time change to not read back in
para<-read.csv(paste("paraout_",cntry,".csv",sep=''))[-1]
setwd(home)

typen<-0 #temporary
year<-c(seq(1900,2050,1),rep(0,151))

count<-1 #remove once doing vacc scenarios
nn<-1 #remove once doing vacc scenarios
nmbr<-count*nn #use this once doing vaccine scenarios
xout<-mat.or.vec(((yearend-year1+1)*2*n_p*(typen+1)),99) #(94+run number+vacc type+vacc effic) would need changing if extra outputs added
colnames(xout)<-c(colnames(X),"timestep","year","type","vxint","fit") #timestep is for params where given by timestep, year is for where output is summary of annual

# Simulate data 
for (kkk in 1:n_p)
    {
    print(kkk)
    for (i in 1:length(nm)){assign(nm[i],as.numeric(para[kkk,i]))} # Assign the parameters to the correct values
    neta2<-neta # this parameter needs extra assigning for some annoying reason! 
    
    # Run the model with these parameters  
    Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,0)   
    xout[(((yearend-year1+1)*2*kkk*nmbr)-(2*(yearend-year1+1)-1)):(2*(yearend-year1+1)*kkk*nmbr),]<-cbind(Xn,times,year,nn,count,kkk)
}


#newo<-ddply(eee,.(Year,fit),summarise,psize=Psize,tbi=100000*(negcases)/Psize,tbm=100000*(negdeaths)/Psize,tbih=100000*(poscases)/Psize,tbmh=100000*(posdeaths)/Psize)
#newo<-ddply(eee,.(Year,fit),summarise,psize=Psize,tbi=100000*(negcases)/Psize,tbm=100000*(negdeaths)/Psize,tbih=100000*(poscases)/Psize,tbmh=100000*(posdeaths)/Psize)

