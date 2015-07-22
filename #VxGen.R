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

times<-seq(year1,(yearend+(1-dt)),dt)
steps<-length(times)

# Generate matrix of exiting vaccine strata
#dV1<-matrix(0,steps,Mnage); 
dV2<-matrix(0,steps,Mnage); 
#dV3<-matrix(0,steps,Mnage);
dV4<-matrix(0,steps,Mnage); 

# Generate matrix of entering vaccine strata
#thetaV1<-matrix(0,steps,Mnage);
thetaV2<-matrix(0,steps,Mnage); 
thetaV2a<-matrix(0,steps,Mnage); 
thetaV2m<-matrix(0,steps,Mnage);
thetablank<-matrix(0,steps,Mnage);
#thetaV3<-matrix(0,steps,Mnage);
thetaV4<-matrix(0,steps,Mnage); 
thetaV4a<-matrix(0,steps,Mnage); 
thetaV4m<-matrix(0,steps,Mnage);

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
    ###elderly vaccine
    # If any step is the start of the year then vaccinate 60yos
    if (any(startyr==i)){
      thetaV2a[i,61]<-coverage # Because of this at the start of the year theta[i,j]*X[i-1,j-1]. As the j-1 have just hit 10yo and are immediately vaccinated
    }
    # If a masscampyear (start of then) then get 61-64yos (was 56-64 when vacc att 55, but reduced number of yrs to align with ages think wuold vac. Campaign over 3 yrs, so divide adult coverage by the 3 yrs of the campaign
    if (any(masscampyr==i)){
      thetaV2m[i,62:65]<-coverage/3
    }
    
    ### ado/adolescent vaccine (V4 because V3 is the combo)
    if (any(startyr==i)){
      thetaV4a[i,16]<-coverage # Because of this at the start of the year theta[i,j]*X[i-1,j-1]. As the j-1 have just hit 10yo and are immediately vaccinated
    }
    # If a masscampyear (start of then) then get 16-24yos. Campaign over 3 yrs, so divide adult coverage by the 3 yrs of the campaign
    if (any(masscampyr==i)){
      thetaV4m[i,17:25]<-coverage/3
    }
    
  }
  
  # Type 2 is both older adults and mass campaigns
  thetaV2<-thetaV2a+thetaV2m
  # Type 4 is young ado/adults and mass campaign
  thetaV4<-thetaV4a+thetaV4m
# Type 3 is a combination of ado and elderly
  #thetaV3<-thetaV4+thetaV2


  
  if (D < 100){ ## D <- 100 if lifelong protection. Then none leave vaccine category. 
  
 if (D==20){  
    
    for (i in 1:steps){
      
      #elderly: dV2 - vaccine immunosenescent waning of 5% per year throughout duration of protection and normal distribution around 20 yrs (s.d. 2yrs) for normal waning
      #ado/adult: dv4 - vaccine waning gaussian throughout duration of protection peaking at 20yrs
      #if (any((startyr[c(1:length(startyr))])==i)){  
        for (nnn in 1:26){
        if (i==startyr[nnn]) {k<-nnn
        print(k)
        dV2[i,]<-oldwane[k,]
        dV4[i,]<-yngwane[k,]
        }
            }
       #   } 
      }
      }
      
    # Remove from the category those that are of the age duration of vaccine + 6months after duration of protection has expired
    #       if (any((midyr+D*(1/dt)-1)==i)){
    #         dV1[i,D+1]<-1
    #       }
    #       # Remove from the category those that were vaccinated at age 55. minus signs remove the ones during the duration of protection so only remove people from steps after end of duration
    #       if (any((startyr[c(-1:-D)]-1)==i)){
    #         dV2[i,(56+D+1)]<-1
    #         #dV3[i,D+1]<-1
    #}
    
      ##ado/adult wane as gaussian
      #need to replace number with a matrix/vector of amt waning at each age
#       if (any((startyr[c(1:length(startyr))])==i)){
#         dV4[i,]<-
#       }
      
      
      # Adult - remove those mass vaccinated = anyone aged 11+D and over (couldn't have been vaccinated any other way)
      # dont need mass campaign as vaccine does not last past age 75yrs
#       if (any((masscampyr+D*(1/dt)-1)==i)){
#         dV2[i,(11+D):Mnage]<-1
        #dV3[i,(11+D):Mnage]<-1
      #}
    #}
  

if (D==10){  
  
  for (i in 1:steps){
    
    #elderly: dV2 - vaccine immunosenescent waning of 5% per year throughout duration of protection and normal distribution around 10 yrs (s.d. 2yrs) for normal waning
    #ado/adult: dv4 - vaccine waning gaussian throughout duration of protection peaking at 10yrs
    #if (any((startyr[c(1:length(startyr))])==i)){  
    for (nnn in 1:26){
      if (i==startyr[nnn]) {k<-nnn
                            print(k)
                            dV2[i,]<-oldwane10[k,]
                            dV4[i,]<-yngwane10[k,]
      }
 }
}
}
}
# Depending on type, theta is theta# (coverage) times efficacy, d is dv#
#vacc1=kids
#vacc2=elderly pre-infection
#vacc3=elderly latent only
#vacc4=elderly mixed effects (pre or post infection, not active)
#vacc5=ado/adult pre-infection
#vacc6=ado/adult latent only
#vacc7=ado/adult mixed effects (pre or post infection, not active)



if (vaccine == 1){
  theta<-thetaV1*eff
  #thetaH<-matrix(0,steps,Mnage); # NO HIV positive babies
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
  thetaS<-thetablank
  thetaL<-theta
  thetaR<-theta
  d<-dV2
} else if (vaccine == 4){
  theta<-thetaV2*eff
  thetaS<-theta
  thetaL<-theta
  thetaR<-theta
  d<-dV2 
#ado/adult
} else if (vaccine == 5){
  theta<-thetaV4*eff
  thetaS<-theta
  thetaL<-thetablank
  thetaR<-thetablank
  d<-dV4
} else if (vaccine == 6){
  theta<-thetaV4*eff
  thetaS<-thetablank
  thetaL<-theta
  thetaR<-theta
  d<-dV4
} else if (vaccine == 7){
  theta<-thetaV4*eff
  thetaS<-theta
  thetaL<-theta
  thetaR<-theta
  d<-dV4
}

# For checking output
#assign('thetaV1',thetaV1,envir=.GlobalEnv);
assign('thetaV2a',thetaV2a,envir=.GlobalEnv);
assign('thetaV2m',thetaV2m,envir=.GlobalEnv);
assign('thetaV4a',thetaV2a,envir=.GlobalEnv);
assign('thetaV4m',thetaV2m,envir=.GlobalEnv)

}


