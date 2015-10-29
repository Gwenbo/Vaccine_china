## calling in model for ABC-MCMC ###

MCMCmodel<-function(para){
  
  #para<-para
  #para<-inipa
  para<-as.data.frame(para)
  #call in data sets and Cfunction
  C=0
  if (C == 0){home<-"/Users/Rebecca/Vaccine_china"}
  if (C == 1){home<-"/home/lsh355020/China/"}
  setwd(home)
  
  #call in data and prep Cfunction
  source('#DataGrab_nopara.R')
  #source('#DataGrab.R')
  
  setwd(home)
  source('CFunctions.R')
  cntry<-"China"
  nm<-c(pararange[,1],"p0") # The parameter ranges
  dt<-0.5  #timestep
  
  #run the model
  for (i in 1:(length(nm)+1)){assign(nm[i],as.numeric(para[i,]))} # Assign the parameters to the correct values
  neta2<-neta # this parameter needs extra assigning for some annoying reason! 
  # Run the model with these parameters  
  Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,dt,c(0.02,0.02,0.8,0.07)),c(year1,yearend),0,C)
  #print(Xn)
  
  # timestep is in cfunction as "times", and make vector of years to be able to select correct yr
  year<-c(seq(year1,yearend,1),rep(0,((1/dt)*(yearend-year1+1)-(yearend-year1+1))))
  xout<-cbind(Xn,year)
  
  #compare model to data ranges and calc number of hits (fithit)
  source("fithit.R")
  print(fithit)
  ###### Aceept or reject? Adjust fit hit threshold as required
  AR <- c(0,0)
  
  if(fithit >=8) AR <- c(1,0) 
  
  
  #return summary statistic of the number of data points the model is hittting
  return(AR)
}


