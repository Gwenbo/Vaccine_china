## Vaccine matrix generation
# Vaccine type is an input: Vx1 = infant, Vx2 = preexposure, Vx3 = postexposure
# Efficacy (eff) and Duration (D) are separate inputs
# Gives theta = proportion per time step of that age that move into the vaccine category (=eff*cov)
# And d = matrix of ages vs times, 1 at (i,j) if at time j all those of age i have to leave the vaccine category

# Year of introduction
yrintro = 2025

# Mid point of year - for infant 6month old vaccine (need to have some death before vaccine)
# dont need midyear for adult/elderly
#midyear <- round((1/dt)/2)

times<-seq(year1,yearend,dt)
steps<-length(times)

# Generate matrix of exiting vaccine strata
#dV1<-matrix(0,steps,Mnage); 
dV2<-matrix(0,steps,Mnage); 
#dV3<-matrix(0,steps,Mnage);

# Generate matrix of entering vaccine strata
#thetaV1<-matrix(0,steps,Mnage);
thetaV2<-matrix(0,steps,Mnage); 
thetaV2a<-matrix(0,steps,Mnage); 
thetaV2m<-matrix(0,steps,Mnage);
thetablank<-matrix(0,steps,Mnage);
#thetaV3<-matrix(0,steps,Mnage);

# If introduce before the end of the simulation
if (yrintro < yearend){
 
  # index for the start of the year timestep and halfway
  startyr <- seq((yrintro-year1)*(1/dt)+1,(yearend-year1)*(1/dt)+1,(1/dt))
  #midyr <- startyr + midyear
  
  # MASS CAMPAIGNS: When are the mass campaigns? //Every 10 years from intro, unless duration (D) longer//

#   if (D > 10){
#     masscampyr<-seq((yrintro-year1)*(1/dt)+1,(yearend-year1)*(1/dt)+1,D*(1/dt))
#   } else {
#     masscampyr<-seq((yrintro-year1)*(1/dt)+1,(yearend-year1)*(1/dt)+1,10*(1/dt))
#   }
#   
  # in elderly model switch to mass only in first 3 years after yrintro
  masscampyr<-seq((yrintro-year1)*(1/dt)+1,((yrintro+2)-year1)*(1/dt)+1,(1/dt))
  
  
  # Number of timepoints
  for (i in 1:steps){
    # If any step is midyear then assign infant coverage to those aged 1 or less (j=1)
#     if (any(midyr==i)){
#       thetaV1[i,1]<-infantcov 
#     }
    # If any step is the start of the year then vaccinate 55yos
    if (any(startyr==i)){
      thetaV2a[i,56]<-coverage # Because of this at the start of the year theta[i,j]*X[i-1,j-1]. As the j-1 have just hit 10yo and are immediately vaccinated
    }
    # If a masscampyear (start of then) then get 56-64yos. Campaign over 3 yrs, so divide adult coverage by the 3 yrs of the campaign
    if (any(masscampyr==i)){
      thetaV2m[i,57:65]<-coverage/3
    }
  }
  
  # Type 2 is both adults and mass campaigns
  thetaV2<-thetaV2a+thetaV2m
  # Type 3 is a combination
  #thetaV3<-thetaV1+thetaV2
  
  if (D < 100){ ## D <- 100 if lifelong protection. Then none leave vaccine category. 
    for (i in 1:steps){
      # Remove from the category those that are of the age duration of vaccine + 6months after duration of protection has expired
#       if (any((midyr+D*(1/dt)-1)==i)){
#         dV1[i,D+1]<-1
#       }
      # Remove from the category those that were vaccinated at age 55. minus signs remove the ones during the duration of protection so only remove people from steps after end of duration
      if (any((startyr[c(-1:-D)]-1)==i)){
        dV2[i,(56+D+1)]<-1
        #dV3[i,D+1]<-1
      }
      #vaccine waning of 5% per year throughout duration of protection
      if (any((startyr[c(1:length(startyr))])==i)){
        dV2[i,(c(57:(57+D-1)))]<-0.05
      }
      
      # Adult - remove those mass vaccinated = anyone aged 11+D and over (couldn't have been vaccinated any other way)
      # dont need mass campaign as vaccine does not last past age 75yrs
#       if (any((masscampyr+D*(1/dt)-1)==i)){
#         dV2[i,(11+D):Mnage]<-1
        #dV3[i,(11+D):Mnage]<-1
      #}
    }
  }
}

# Depending on type, theta is theta# (coverage) times efficacy, d is dv#
if (vaccine == 1){
  theta<-thetaV1*eff
  thetaH<-matrix(0,steps,Mnage); # NO HIV positive babies
  d<-dV1
} else if (vaccine == 2){
  theta<-thetaV2*eff
  thetaS<-theta
  thetaL<-thetablank
  thetaR<-thetablank
  # HIV efficacy multiplied in main (as need yearly ART average)
  #thetaH<-theta
  d<-dV2
  #print(c('sumtheta',sum(theta)))
} else if (vaccine == 3){
  theta<-thetaV2*eff
  thetaS<-theta
  thetaL<-theta
  thetaR<-theta
  d<-dV2
}

# For checking output
#assign('thetaV1',thetaV1,envir=.GlobalEnv);
assign('thetaV2a',thetaV2a,envir=.GlobalEnv);
assign('thetaV2m',thetaV2m,envir=.GlobalEnv);