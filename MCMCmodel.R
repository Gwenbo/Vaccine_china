## calling in model for ABC-MCMC ###

MCMCmodel<-function(para){
  
#   inipa
#   #para<-para
#   para<-inipa
  para<-as.data.frame(para)
  para
  print(para)
  
  #call in data sets and Cfunction
  
  #### check that elderly/infant parameters are bigger/smaller than adult (as appropriate)
  
  if ((para[9,1]>=para[10,1]) & (para[11,1]>=para[21,1]) & (para[12,1]>=para[23,1]) & (para[13,1]>=para[14,1]) & (para[16,1]>=para[17,1]) & (para[18,1]<=para[22,1]) & (para[19,1]>=para[26,1]) & (para[23,1]<=para[24,1])) {paracheck<-1} else {paracheck<-0}
  print(paracheck)
  
  if (paracheck==0){AR <- c(0,0)
                    hittrack<-c(hittrack,99)}
  else {
  
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
  head(Xn)
  
  # timestep is in cfunction as "times", and make vector of years to be able to select correct yr
  year<-c(seq(year1,yearend,1),rep(0,((1/dt)*(yearend-year1+1)-(yearend-year1+1))))
  xout<-cbind(Xn,year)
  head(xout)
  print(xout[xout[,"year"]%in%2000,"TBPbtot"])
  #need to assign to global otherwise the fit hit script cant 'see' it
  assign('xout',xout,envir=.GlobalEnv)
  
  #compare model to data ranges and calc number of hits (fithit)
  source("fithit.R")
    
  fithit
  hittrack<-c(hittrack,fithit)
  hittrack<-c(hittrack,fithit)
  
  
  ###### Aceept or reject? Adjust fit hit threshold as required. Keep = 1
  AR <- c(0,0)
  print (fithit)
  print (hittrack)
  
  if(fithit >=hits) AR <- c(1,0) 
  
  #end of ifelse for if parameters are right sizes
  }
  
  print("AR")
  print(AR)
  #return summary statistic of the number of data points the model is hittting
  assign('hittrack',hittrack,envir=.GlobalEnv)
  
  return(AR)
}


