#### All functions needed 

#### MAIN Function to generate model output
# Takes in country, vaccine details, fit parameters, initial values, time for simulation, whether to plot, cluster or not
FitGo <- function(cntry,Vx,Fit,InitV,TimeScale,Plot,C){
  # Cntry <- country name; # Vx <- vector of vaccine properties. c(vxtype, efficacy, duration). If blank then no vaccine (for fit)
  # Fit <- 6 fit parameters
  # InitV <- vector of initial values: Run number, timestep, proportion TB initially (either 1 or four = ? )
  # TimeScale <- c(year1, yearend)
  # Plot <- whether to plot or not (1 = yes)
  
  # C is the cluster parameter -> C==0 if on laptop
  if (C == 0){setwd("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel/")}
  if (C == 1){setwd("/users/eidegkni/Documents/vaccine")}
  
  # Paranames in Fit and InitV
  FitV<-c('psz1900','rmort','neta','rmortTB','CDRscale','alpha')
  InitialV<-c('run','dt','prop') 
  
  #### Run model with these input parameters
  # Assign parameters to values inputted
  if(length(InitV)==3){for(i in 1:length(InitV)){assign(InitialV[i],InitV[i],envir = .GlobalEnv)}
  } else {for(i in 1:2){assign(InitialV[i],as.numeric(InitV[i]),envir = .GlobalEnv)};assign(InitialV[3],as.numeric(InitV[3:6]),envir = .GlobalEnv)}
  assign('cntry',cntry,envir = .GlobalEnv)
  assign('year1',TimeScale[1],envir = .GlobalEnv); assign('yearend',TimeScale[2],envir = .GlobalEnv)
  ## Parameters that are kept in but don't use in this version: different progression proportions from Stover
  g<-x; gH<-xH;
  hchild<-fchild; hadult<-fadult; hH<-fH
  # Timesteps with inputted start end and timestep
  times<-seq(year1,yearend,dt);steps<-length(times);
  
  # Country data (may be overruled by FIT values)
  source("#CountryCalc.R")
  for(i in 1:length(Fit)){assign(FitV[i],Fit[i],envir = .GlobalEnv)} # Only assign those variables that want to 
  #print(c("neta",neta))
  # FIT: rmortTB = proportion multiply TB mortality by. Range=[-1,1].
  if(length(Fit)>3){if (rmortTB<0){ui<-rmortTB*(ui)+ui; uni<-rmortTB*(uni)+uni;
  } else {ui<-rmortTB*(1-ui)+ui; uni<-rmortTB*(1-uni)+uni}
  }
  # If don't assign CDRscale set it to 1 (i.e. use data)
  if(length(Fit)<5){CDRscale <- 1}
  
  ## Generate Vaccine specific data using above eff and D if specified in input
  if(length(Vx)>1){
    assign('vaccine',Vx[1],envir = .GlobalEnv); assign('eff',Vx[2],envir = .GlobalEnv); assign('D',Vx[3],envir = .GlobalEnv)
    source("#VxGen.R")
  } else {d<-matrix(0,steps,Mnage); 
          theta<-matrix(0,steps,Mnage);
          vaccine<-0;thetaH<-theta;
          eff<-0;Dur<-0;
          }
  
  
  ## Initialise
  source("#Init.R")
  
  ## To check with fitting vector
  #yyy<-c(pchild,padult,pH,v,vH,x,xH,fchild,fadult,fH,w,n,nH,r,rH,e,g,gH,hchild,hadult,hH,LEHIV,LEART,effH,effHa,rmort,neta,rmortTB,CDRscale,alpha)
  #if(sum(xxx-yyy)>0){print(c("AT",which(xxx-yyy>0),"yyy",yyy,"error with para"))}else{print("para match")}
  
  # # # # # # RUN 
  for (k in year1:(yearend)){
    #print(k)
    
    #### •••••••••••••••••• Yearly parameters #••••••••••••••••••
    # Only start marking years after 2009
    if (k <= 2009){ yr <- 2009 } else { yr <- k }
    #### MORTALITY. Runs from age 1 to age 85
    # FIT: Rmort multiplies background death rates. Range [-1,1]
    if (rmort < 0) {u<-rmort*(mort[1+yr-2009,2:86]) + mort[1+yr-2009,2:86];names(u)<-NULL;#print(c("neg","u",u,"rmort",rmort)) 
    } else {u<-rmort*(1-(mort[1+yr-2009,2:86])) + mort[1+yr-2009,2:86];names(u)<-NULL;#print(c("u",u,"rmort",rmort))
    }
    #### HIV MORTALITY (weighted by ART coverage)
    uH <- u + 1/( (1 - art[1+yr-2009]) * LEHIV + art[1+yr-2009] * LEART)
    
    #### HIV Incidence
    # Take the same value before 2009, otherwise the year's values
    if (k == year1){ ind <- 1 } else { ind <- ( (1/dt) * (k - year1) ) }
    # Population size in each of the groups (15-49yos, same but HIV+)
    psize1549<-sum(S[ind,15:49],L[ind,15:49],R[ind,15:49],I[ind,15:49],NI[ind,15:49],Sv[ind,15:49],Lv[ind,15:49],Rv[ind,15:49])
  
    #print(c(psize1549,psizeH1549,psize1549))
    # Proportion who become HIV+ is then weighted to only be in the 15-49yo pop that is hiv-
    hiv<-hivI/(100*(psize1549/(psizeH1549+psize1549)))
    
    #### HIV specific parameters - yearly ART variation and vH (could go earlier)
    # FIT: Alpha multiplies the progression rates for HIV+s. Range [-1,1] 
    if (length(Fit) > 5){if (alpha < 0) {pH2 <- alpha*pH + pH; vH2 <- alpha*vH + vH; rH2 <- alpha*rH + rH
    } else {pH2 <- alpha*(1 - pH) + pH; vH2 <- alpha*(1 - vH) + vH; rH2 <- alpha*(1 - rH) + rH
    }} else {pH2<-pH; vH2<-vH; rH2<-rH}
    
    # Calculate the ART weighted averages for progression
    pHA<-pH2*(1-art[1+yr-2009]) + art[1+yr-2009] * 0.35 * pH2 
    vHA<-vH2*(1-art[1+yr-2009]) + art[1+yr-2009] * 0.35 * vH2 
    rHA<-rH2*(1-art[1+yr-2009]) + art[1+yr-2009] * 0.35 * rH2 
    xHA<-xH*(1-art[1+yr-2009]) + art[1+yr-2009] * 0.35 * xH
    gHA<-gH*(1-art[1+yr-2009]) + art[1+yr-2009] * 0.35 * gH
    
    # And for mortality 
    uiHA<-(1-art[1+yr-2009])*uiH + art[1+yr-2009] * 0.25 * uiH 
    uniHA<-(1-art[1+yr-2009])*uniH + art[1+yr-2009] * 0.25 * uniH 
    
    # And vaccine efficacy
    effHA<-effH*(1-art[1+yr-2009]) + art[1+yr-2009] * effHa
    thetaH<-thetaH*effHA
    
    # print(c('ui',ui,'uiH',uiH,'uiHA',uiHA,'uniHA',uniHA))
    # print(c('pH',pH,'pHA',pHA,'vHa',vHA,'xHA',xHA,'rHA',rHA,'art',art[20],'alpha',alpha))
    
    #### CDR & TREATMENT SUCCESS (a proportion of cases that are found and successfully treated)
    # FIT: CDRscale multiplies the cdr value for both HIV+s and HIV-s
    CDR<-CDRscale*cdr[1+yr-2009];CDRH<-CDRscale*cdrH[1+yr-2009];
    CoT<-suctt[1+yr-2009];CoTH<-sucttH[1+yr-2009];
    #print(c(yr,'CDRH',CDRH,'CoT',CoT,'CoTH',CoTH,1+yr-2009))
    
    #### BIRTHS 
    # Need to have 2009 birth RATE pre-2009 else won't get curve 
    if (k < 2009) { br<-bb[1]/(Popsize[1,cntry])
                    if (k == year1){B<-round(br*psize[1]); bv<-c(bv,B)
                    } else { B<-round(br*psize[((k-year1)*(1/dt))]); bv<-c(bv,B);}
    } else { B<-bb[1+yr-2009] }
    #print(c("BIRTHS",br,B,psize[((k-year1)*(1/dt))],((k-year1)*(1/dt))))
    
    ####•••••••••••••••••• END OF YEARLY PARAMETERS
    
    #### Start of model run
    if (k > year1){ # Start of year 1 is the initial condition
      ## These are those where the time step is the first of the year
      i = ((1/dt)*(k-year1)+1) # Time zero is first row
      start <- 1 # whether first ts of year. All year dependent parameters must be last years.
      # FIT: Takes in the neta defined as an input
      lambda[i-1] <- (1-exp(-(neta) * z * ((sum(I[i-1,],IH[i-1,]))/(psize[i-1]))))
      
      ####•••••••••••••••••• TB model ••••••••••••••••••
      # Age 1, first time step of the year, all births occur
      
      j = 1; S[i,j]<-B     
      
      S[i,2:14] = S[i-1,1:13] - (u[1:13]+lambda[i-1])*S[i-1,1:13]*dt 
      L[i,2:14] = L[i-1,1:13] + lambda[i-1]*(1 - pchild)*(S[i-1,1:13] + g*R[i-1,1:13])*dt - (v + lambda[i-1]*pchild*x + u[1:13])*L[i-1,1:13]*dt 
      
      new_I[i,2:14] = lambda[i-1]*pchild*fchild*(S[i-1,1:13] + g*R[i-1,1:13])*dt + (v + lambda[i-1]*pchild*x)*fchild*L[i-1,1:13]*dt + r*hchild*R[i-1,1:13]*dt + w*NI[i-1,1:13]*dt
      new_NI[i,2:14] = lambda[i-1]*pchild*(1 - fchild)*(S[i-1,1:13] + g*R[i-1,1:13])*dt + (v + lambda[i-1]*pchild*x)*(1 - fchild)*L[i-1,1:13]*dt + r*(1 - hchild)*R[i-1,1:13]*dt
      
      R[i,2:14] = R[i-1,1:13] + n*(I[i-1,1:13] + NI[i-1,1:13])*dt + CDR*CoT*(new_I[i,2:14] + e*new_NI[i,2:14]) - (r + g*lambda[i-1] + u[1:13])*R[i-1,1:13]*dt 
      I[i,2:14] = I[i-1,1:13] + (1 - CDR*CoT)*(new_I[i,2:14]) - (n + u[1:13] + ui)*I[i-1,1:13]*dt
      NI[i,2:14] = NI[i-1,1:13] + (1 - CDR*CoT)*(e*new_NI[i,2:14]) - (n + u[1:13] + uni + w)*NI[i-1,1:13]*dt
      
      #if(I[i,2] < I[i-1,1]){print(c(i,I[i,2],I[i-1,1],"stop",(n + u[1:13] + ui),"cdr",CDR,CoT))}
      
      S[i,15:Mnage] = S[i-1,14:(Mnage-1)] - (u[14:(Mnage-1)]+lambda[i-1])*S[i-1,14:(Mnage-1)]-hiv[14:(Mnage-1)]*S[i-1,14:(Mnage-1)]*dt 
      L[i,15:Mnage] = L[i-1,14:(Mnage-1)] + lambda[i-1]*(1 - padult)*(S[i-1,14:(Mnage-1)] + g*R[i-1,14:(Mnage-1)])*dt - (v + lambda[i-1]*padult*x + u[14:(Mnage-1)] + hiv[14:(Mnage-1)])*L[i-1,14:(Mnage-1)]*dt 
      
      new_I[i,15:Mnage] = lambda[i-1]*padult*fadult*(S[i-1,14:(Mnage-1)] + g*R[i-1,14:(Mnage-1)])*dt + (v + lambda[i-1]*padult*x)*fadult*L[i-1,14:(Mnage-1)]*dt + r*hadult*R[i-1,14:(Mnage-1)]*dt + w*NI[i-1,14:(Mnage-1)]*dt
      new_NI[i,15:Mnage] = lambda[i-1]*padult*(1 - fadult)*(S[i-1,14:(Mnage-1)] + g*R[i-1,14:(Mnage-1)])*dt + (v + lambda[i-1]*padult*x)*(1 - fadult)*L[i-1,14:(Mnage-1)]*dt + r*(1 - hadult)*R[i-1,14:(Mnage-1)]*dt  
      
      #for use in calc proportion reactiviation vs new. Check the f=*hadult term, what does g= ???
      new_react[i,15:Mnage]=  g*R[i-1,14:(Mnage-1)])*dt + v*L[i-1,14:(Mnage-1)]*dt + r*R[i-1,14:(Mnage-1)]*dt
      
      
      R[i,15:Mnage] = R[i-1,14:(Mnage-1)] + n*(I[i-1,14:(Mnage-1)] + NI[i-1,14:(Mnage-1)])*dt + CDR*CoT*(new_I[i,15:Mnage] + e*new_NI[i,15:Mnage]) - (r + g*lambda[i-1] + u[14:(Mnage-1)] + hiv[14:(Mnage-1)] )*R[i-1,14:(Mnage-1)]*dt 
      I[i,15:Mnage] = I[i-1,14:(Mnage-1)] + (1 - CDR*CoT)*(new_I[i,15:Mnage]) - (n + u[14:(Mnage-1)] + ui + hiv[14:(Mnage-1)])*I[i-1,14:(Mnage-1)]*dt
      NI[i,15:Mnage] = NI[i-1,14:(Mnage-1)] + (1 - CDR*CoT)*(e*new_NI[i,15:Mnage]) - (n + u[14:(Mnage-1)] + uni + w + hiv[14:(Mnage-1)])*NI[i-1,14:(Mnage-1)]*dt                    
      
      ####•••••••••••••••••••• TB HIV model •••••••••••••••••
      
      SH[i,15:Mnage] = SH[i-1,14:(Mnage-1)] - (uH[14:(Mnage-1)] + lambda[i-1])*SH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*S[i-1,14:(Mnage-1)]*dt 
      LH[i,15:Mnage] = LH[i-1,14:(Mnage-1)] + lambda[i-1]*(1 - pHA)*(SH[i-1,14:(Mnage-1)] + gHA*RH[i-1,14:(Mnage-1)])*dt - (vHA + lambda[i-1]*pHA*xHA + uH[14:(Mnage-1)])*LH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*L[i-1,14:(Mnage-1)]*dt 
      
      new_IH[i,15:Mnage] = lambda[i-1]*pHA*fH*(SH[i-1,14:(Mnage-1)] + gHA*RH[i-1,14:(Mnage-1)])*dt + (vHA + lambda[i-1]*pHA*xHA)*fH*LH[i-1,14:(Mnage-1)]*dt + rHA*hH*RH[i-1,14:(Mnage-1)]*dt + w*NIH[i-1,14:(Mnage-1)]*dt
      new_NIH[i,15:Mnage] = lambda[i-1]*pHA*(1 - fH)*(SH[i-1,14:(Mnage-1)] + gHA*RH[i-1,14:(Mnage-1)])*dt + (vHA + lambda[i-1]*pHA*xHA)*(1 - fH)*LH[i-1,14:(Mnage-1)]*dt + rHA*(1 - hH)*RH[i-1,14:(Mnage-1)]*dt  
      
      RH[i,15:Mnage] = RH[i-1,14:(Mnage-1)] + nH*(IH[i-1,14:(Mnage-1)] + NIH[i-1,14:(Mnage-1)])*dt + CDRH*CoTH*(new_IH[i-1,14:(Mnage-1)] + e*new_NIH[i-1,14:(Mnage-1)]) - (rHA + gHA*lambda[i-1] + uH[14:(Mnage-1)])*RH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*R[i-1,14:(Mnage-1)]*dt
      IH[i,15:Mnage] = IH[i-1,14:(Mnage-1)] + (1 - CDRH*CoTH)*(new_IH[i-1,14:(Mnage-1)]) - (nH + uH[14:(Mnage-1)] + uiHA)*IH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*I[i-1,14:(Mnage-1)]*dt
      NIH[i,15:Mnage] = NIH[i-1,14:(Mnage-1)] + (1 - CDRH*CoTH)*(e*new_NIH[i-1,14:(Mnage-1)]) - (nH + uH[14:(Mnage-1)] + uniHA + w)*NIH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*NI[i-1,14:(Mnage-1)]*dt                
      
      ####•••••••••••••••••••••••••••••••••••••••••••••••••••••••• VACCINE STRATA AGING + INFECTION •••••••••••••••••••••••••
      ## Others have aged so need these to be reset to zero
      Sv[i,1] = 0;Lv[i,1] = 0;Rv[i,1] = 0;SvH[i,1] = 0;LvH[i,1] = 0;RvH[i,1] = 0;
      
      Sv[i,2:14] = Sv[i-1,1:13] - (u[1:13]+lambda[i-1])*Sv[i-1,1:13]*dt 
      Lv[i,2:14] = Lv[i-1,1:13] - (u[1:13])*Lv[i-1,1:13]*dt + lambda[i-1]*(Sv[i-1,1:13] + g*Rv[i-1,1:13])*dt
      Rv[i,2:14] = Rv[i-1,1:13] - (u[1:13]+g*lambda[i-1])*Rv[i-1,1:13]*dt 
      
      Sv[i,15:Mnage] = Sv[i-1,14:(Mnage-1)] - (u[14:(Mnage-1)] + hiv[14:(Mnage-1)])*Sv[i-1,14:(Mnage-1)]*dt - lambda[i-1]*(Sv[i-1,14:(Mnage-1)])*dt
      Lv[i,15:Mnage] = Lv[i-1,14:(Mnage-1)] - (u[14:(Mnage-1)] + hiv[14:(Mnage-1)])*Lv[i-1,14:(Mnage-1)]*dt + lambda[i-1]*(Sv[i-1,14:(Mnage-1)] + g*Rv[i-1,14:(Mnage-1)])*dt
      Rv[i,15:Mnage] = Rv[i-1,14:(Mnage-1)] - (u[14:(Mnage-1)] + hiv[14:(Mnage-1)])*Rv[i-1,14:(Mnage-1)]*dt - lambda[i-1]*(g*Rv[i-1,14:(Mnage-1)])*dt
      
      SvH[i,15:Mnage] = SvH[i-1,14:(Mnage-1)] + hiv[14:(Mnage-1)]*Sv[i-1,14:(Mnage-1)]*dt - (uH[14:(Mnage-1)])*SvH[i-1,14:(Mnage-1)]*dt - lambda[i-1]*(SvH[i-1,14:(Mnage-1)])*dt
      LvH[i,15:Mnage] = LvH[i-1,14:(Mnage-1)] + hiv[14:(Mnage-1)]*Lv[i-1,14:(Mnage-1)]*dt - (uH[14:(Mnage-1)])*LvH[i-1,14:(Mnage-1)]*dt + lambda[i-1]*(SvH[i-1,14:(Mnage-1)] + gHA*RvH[i-1,14:(Mnage-1)])*dt
      RvH[i,15:Mnage] = RvH[i-1,14:(Mnage-1)] + hiv[14:(Mnage-1)]*Rv[i-1,14:(Mnage-1)]*dt - (uH[14:(Mnage-1)])*RvH[i-1,14:(Mnage-1)]*dt - lambda[i-1]*(gHA*RvH[i-1,14:(Mnage-1)])*dt          
      
      ###•••••••••••••••••• Vaccine coverage and duration ••••••••••••••••
      # NUMBER OF VACCINES (column 1=infant, 2=10yos, 3=mass)
      if(vaccine == 0){VX[i,1:3]<-0}
      if(vaccine == 1){VX[i,1]<-sum(thetaV1[i,]*(S[i,]+L[i,]+R[i,]))}
      if(vaccine == 2){VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2a[i,]*(SH[i,]+LH[i,]+RH[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2m[i,]*(SH[i,]+LH[i,]+RH[i,]))}
      if(vaccine == 3){VX[i,1]<-sum(thetaV1[i,]*(S[i,]+L[i,]+R[i,])); VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2a[i,]*(SH[i,]+LH[i,]+RH[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2m[i,]*(SH[i,]+LH[i,]+RH[i,]))}
      
      ##•••••••••••••••••• Vaccination campaign: age everyone and then implement (both vaccination and return)
      S2 = S[i,] + Sv[i,]*(d[i,]*(1-theta[i,])) - theta[i,]*S[i,]
      L2 = L[i,] + Lv[i,]*(d[i,]*(1-theta[i,])) - theta[i,]*L[i,]
      R2 = R[i,] + Rv[i,]*(d[i,]*(1-theta[i,])) - theta[i,]*R[i,]
      
      SH2 = SH[i,] + SvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*SH[i,]
      LH2 = LH[i,] + LvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*LH[i,]
      RH2 = RH[i,] + RvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*RH[i,]
      
      Sv2 = Sv[i,] - Sv[i,]*(d[i,]*(1-theta[i,])) + theta[i,]*S[i,]
      Lv2 = Lv[i,] - Lv[i,]*(d[i,]*(1-theta[i,])) + theta[i,]*L[i,]
      Rv2 = Rv[i,] - Rv[i,]*(d[i,]*(1-theta[i,])) + theta[i,]*R[i,]
      
      SvH2 = SvH[i,] - SvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*SH[i,]
      LvH2 = LvH[i,] - LvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*LH[i,]
      RvH2 = RvH[i,] - RvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*RH[i,]
      
      S[i,]<-S2;L[i,]<-L2;R[i,]<-R2;       SH[i,]<-SH2;LH[i,]<-LH2;RH[i,]<-RH2;
      Sv[i,]<-Sv2;Lv[i,]<-Lv2;Rv[i,]<-Rv2; SvH[i,]<-SvH2;LvH[i,]<-LvH2;RvH[i,]<-RvH2;
      
      ####•••••••••••••••••• Economic Output ••••••••••••••••••
      #### Output for cost-effectiveness 
      ## POPULATION SIZE
      psize[i]<-sum(S[i,],L[i,],R[i,],I[i,],NI[i,],Sv[i,],Lv[i,],Rv[i,])
      #print(c("PSIZE",i,psize[i]))
      #what is ind??? also, was written as 15-49 for 15-49, but isnt age 0 j=1, so should be 16:50 for age 15-49???
      #ages needed to fit to incidence and population size
      psize014[i]<-sum(S[ind,1:15],L[ind,1:15],R[ind,1:15],I[ind,1:15],NI[ind,1:15],Sv[ind,1:15],Lv[ind,1:15],Rv[ind,1:15])
      psize1554[i]<-sum(S[ind,16:55],L[ind,16:55],R[ind,16:55],I[ind,16:55],NI[ind,16:55],Sv[ind,16:55],Lv[ind,16:55],Rv[ind,16:55])
      psize5564[i]<-sum(S[ind,56:65],L[ind,56:65],R[ind,56:65],I[ind,56:65],NI[ind,56:65],Sv[ind,56:65],Lv[ind,56:65],Rv[ind,56:65])
      psize65plus[i]<-sum(S[ind,66:Mnage],L[ind,66:Mnage],R[ind,66:Mnage],I[ind,66:Mnage],NI[ind,66:Mnage],Sv[ind,66:Mnage],Lv[ind,66:Mnage],Rv[ind,66:Mnage])
      #ages needed to fit to mort  and prevalence as have different groupings
      psize1559[i]<-sum(S[ind,16:60],L[ind,16:60],R[ind,16:60],I[ind,16:60],NI[ind,16:60],Sv[ind,16:60],Lv[ind,16:60],Rv[ind,16:60])
      psize1529[i]<-sum(S[ind,16:30],L[ind,16:30],R[ind,16:30],I[ind,16:30],NI[ind,16:30],Sv[ind,16:30],Lv[ind,16:30],Rv[ind,16:30])
      psize3044[i]<-sum(S[ind,31:45],L[ind,31:45],R[ind,31:45],I[ind,31:45],NI[ind,31:45],Sv[ind,31:45],Lv[ind,31:45],Rv[ind,31:45])
      psize4459[i]<-sum(S[ind,45:60],L[ind,45:60],R[ind,45:60],I[ind,45:60],NI[ind,45:60],Sv[ind,45:60],Lv[ind,45:60],Rv[ind,45:60])
      psize60plus[i]<-sum(S[ind,61:Mnage],L[ind,61:Mnage],R[ind,61:Mnage],I[ind,61:Mnage],NI[ind,61:Mnage],Sv[ind,61:Mnage],Lv[ind,61:Mnage],Rv[ind,61:Mnage])
      
      ## number vaccinated
      #need to set up matrix for psizevacc to be recorded in to**  not needed as in econout???
      #psizevacc[i]<-sum(Sv[i,],Lv[i,],Rv[i,])
      #nmbvacc<-psizevacc[i]-psizevacc[i-1]
    
    
      ## Death markers
      # Number of TB deaths in HIV-, in HIV+, all form HIV deaths
      TBDeaths[i,]=dt*((ui)*I[i-1,]+(uni)*NI[i-1,]);
      TBDeathsH[i,]=dt*(uiHA*IH[i-1,]+(uniHA)*NIH[i-1,]);
      AllDeathsH[i,]=dt*((uH+uiHA)*IH[i-1,]+(uH+uniHA)*NIH[i-1,]);
      # Age deaths HIV-, HIV+
      ADeaths[i,]=dt*(u*S[i-1,]+u*L[i-1,]+(u+ui)*I[i-1,]+(u+uni)*NI[i-1,]+u*R[i-1,]+u*Sv[i-1,]+u*Lv[i-1,]+u*Rv[i-1,])
      ADeathsH[i,]=dt*(uH*SH[i-1,]+uH*LH[i-1,]+(uH+uiHA)*IH[i-1,]+(uH+uniHA)*NIH[i-1,]+uH*RH[i-1,]+u*SvH[i-1,]+u*LvH[i-1,]+u*RvH[i-1,])
      # Deaths matrix holds all death indices
      # Columns: Number deaths HIV-, av. age HIV death, number HIV+ deaths, av age HIV+ death, av. age death
      Deaths[i,1]=sum(ADeaths[i,]);   Deaths[i,2]=sum(ADeaths[i,]*seq(1:Mnage))/sum(ADeaths[i,])
      Deaths[i,3]=sum(ADeathsH[i,]);  Deaths[i,4]=sum(ADeathsH[i,]*seq(1:Mnage))/sum(ADeathsH[i,])
      Deaths[i,5]=sum((ADeathsH[i,]+ADeaths[i,])*seq(1:Mnage))/sum(ADeathsH[i,]+ADeaths[i,])
      
      ## NUMBER ON TREATMENT & NUMBER SUCCESSFULLY TREATED
      # TBRx columns: HIV- detected, successfully treated, HIV+ detected, successfully treated
      TBRx[i,1]=CDR*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]));    TBRx[i,2]=CDR*(CoT)*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]))
      TBRx[i,3]=CDRH*(sum(new_IH[i-1,])+e*sum(new_NIH[i-1,]));  TBRx[i,4]=CDRH*(CoTH)*(sum(new_IH[i-1,])+e*sum(new_NI[i-1,]))
      
      if (i==length(seq(year1,yearend,dt))){ # LAST TIME STEP
        # First is for whole population, second for HIV positives only... 
        Out<-cbind(Deaths,TBRx,psize,rowSums(S),rowSums(L),rowSums(R),rowSums(I),rowSums(NI),rowSums(Sv),rowSums(Lv),rowSums(Rv),rowSums(SvH),rowSums(LvH),rowSums(RvH),rowSums(theta),rowSums(thetaH),rowSums(d),VX)
        nms<-c("Deaths","DAge","DeathsHIV","DHAge","AllDAge","Rx","SucT","RxH","SucTH","Psz","S","L","R","I","NI","Sv","Lv","Rv","SvH","LvH","RvH","theta","thetaH","d","VaccDTP3","Vacc10","VaccMass")
        Out<-as.data.frame(Out);colnames(Out)<-nms
        
        #### FOR CE OUTPUT
        yrcount<-seq(1,(yearend-year1)*(1/dt)+1,(1/dt))
        EconOut<-matrix(0,length(yrcount)-1,9)
        EconOut[,1]<-seq(year1,yearend-1)
        nns<-c("Year","HIV-cases","HIV+cases","TBDeaths","AvAgeD","VaccDTP3","Vacc10","VaccMass","Treatments")
        EconOut<-as.data.frame(EconOut); colnames(EconOut)<-nns
        
        hbcOut<-matrix(0,length(yrcount)-1,6)
        hbcOut[,1]<-seq(year1,yearend-1)
        nns2<-c("Year","Psize","negcases","poscases","negdeaths","posdeaths")
        hbcOut<-as.data.frame(hbcOut); colnames(hbcOut)<-nns2
        
        # Calculate that years average
        
        for (i in 1:(length(yrcount)-1)){
          #gives first and last timestep of a year??? no as first timestep of year runs. so why need???
          i1<-yrcount[i]; i2<-yrcount[i+1]-1
          EconOut[i,"AvAgeD"]=sum(colSums(ADeathsH[i1:i2,]+ADeaths[i1:i2,])*seq(1:Mnage))/sum(ADeathsH[i1:i2,]+ADeaths[i1:i2,])
          EconOut[i,"TBDeaths"]=sum(TBDeaths[i1:i2,]+TBDeathsH[i1:i2,])
          EconOut[i,"HIV-cases"]=sum(new_I[i1:i2,]+new_NI[i1:i2,])
          EconOut[i,"HIV+cases"]=sum(new_IH[i1:i2,]+new_NIH[i1:i2,])
          EconOut[i,"VaccDTP3"]=sum(VX[i1:i2,1])
          EconOut[i,"Vacc10"]=sum(VX[i1:i2,2])
          EconOut[i,"VaccMass"]=sum(VX[i1:i2,3])
          EconOut[i,"Treatments"]=sum(TBRx[i1:i2,1]+TBRx[i1:i2,3])
          
          hbcOut[i,"Psize"] = mean(psize[i1:i2])
          hbcOut[i,"negcases"] = sum(new_I[i1:i2,]+new_NI[i1:i2,])
          hbcOut[i,"poscases"] = sum(new_IH[i1:i2,]+new_NIH[i1:i2,])
          hbcOut[i,"negdeaths"] =sum(TBDeaths[i1:i2,])
          hbcOut[i,"posdeaths"] = sum(TBDeathsH[i1:i2,])
        }
      } 
    } ####•••••••••••••••••• END OF START OF YEAR RUNS
    
    ####•••••••••••••••••• MIDDLE OF YEAR RUNS
    if (k < yearend){ 
      for (i in (2+(1/dt)*(k-year1)):((1/dt)*(k-year1)+1/dt)){
        start <- 0 # Not the start of the year
        lambda[i-1] <- (1 - exp(-(neta) * z * ((sum(I[i-1,],IH[i-1,]))/(psize[i-1]))))
        
        ####•••••••••••••••••• TB model ••••••••••••••••••
        ## If the time step is not the first of the year 
        
        S[i,1:14] = S[i-1,1:14] - (u[1:14]+lambda[i-1])*S[i-1,1:14]*dt 
        L[i,1:14] = L[i-1,1:14] + lambda[i-1]*(1 - pchild)*(S[i-1,1:14] + g*R[i-1,1:14])*dt - (v + lambda[i-1]*pchild*x + u[1:14])*L[i-1,1:14]*dt
        
        new_I[i,1:14] = lambda[i-1]*pchild*fchild*(S[i-1,1:14] + g*R[i-1,1:14])*dt + (v + lambda[i-1]*pchild*x)*fchild*L[i-1,1:14]*dt + r*hchild*R[i-1,1:14]*dt + w*NI[i-1,1:14]*dt
        new_NI[i,1:14] = lambda[i-1]*pchild*(1 - fchild)*(S[i-1,1:14] + g*R[i-1,1:14])*dt + (v + lambda[i-1]*pchild*x)*(1 - fchild)*L[i-1,1:14]*dt + r*(1 - hchild)*R[i-1,1:14]*dt
        
        R[i,1:14] = R[i-1,1:14] + n*(I[i-1,1:14] + NI[i-1,1:14])*dt + CDR*CoT*(new_I[i,1:14] + e*new_NI[i,1:14]) - (r + g*lambda[i-1] + u[1:14])*R[i-1,1:14]*dt
        I[i,1:14] = I[i-1,1:14] + (1 - CDR*CoT)*(new_I[i,1:14]) - (n + u[1:14] + ui)*I[i-1,1:14]*dt
        NI[i,1:14] = NI[i-1,1:14] + (1 - CDR*CoT)*(e*new_NI[i,1:14]) - (n + u[1:14] + uni + w)*NI[i-1,1:14]*dt
        
        S[i,15:Mnage] = S[i-1,15:Mnage] - (u[15:Mnage]+lambda[i-1])*S[i-1,15:Mnage]*dt - hiv[15:Mnage]*S[i-1,15:Mnage]*dt 
        L[i,15:Mnage] = L[i-1,15:Mnage] + lambda[i-1]*(1 - padult)*(S[i-1,15:Mnage] + g*R[i-1,15:Mnage])*dt - (v + lambda[i-1]*padult*x + u[15:Mnage] + hiv[15:Mnage])*L[i-1,15:Mnage]*dt
        
        new_I[i,15:Mnage] = lambda[i-1]*padult*fadult*(S[i-1,15:Mnage] + g*R[i-1,15:Mnage])*dt + (v + lambda[i-1]*padult*x)*fadult*L[i-1,15:Mnage]*dt + r*hadult*R[i-1,15:Mnage]*dt + w*NI[i-1,15:Mnage]*dt
        new_NI[i,15:Mnage] = lambda[i-1]*padult*(1 - fadult)*(S[i-1,15:Mnage] + g*R[i-1,15:Mnage])*dt + (v + lambda[i-1]*padult*x)*(1 - fadult)*L[i-1,15:Mnage]*dt + r*(1 - hadult)*R[i-1,15:Mnage]*dt  
        
        R[i,15:Mnage] = R[i-1,15:Mnage] + n*(I[i-1,15:Mnage] + NI[i-1,15:Mnage])*dt + CDR*CoT*(new_I[i,15:Mnage] + e*new_NI[i,15:Mnage]) - (r + g*lambda[i-1] + u[15:Mnage] + hiv[15:Mnage])*R[i-1,15:Mnage]*dt 
        I[i,15:Mnage] = I[i-1,15:Mnage] + (1 - CDR*CoT)*(new_I[i,15:Mnage]) - (n + u[15:Mnage] + ui + hiv[15:Mnage])*I[i-1,15:Mnage]*dt
        NI[i,15:Mnage] = NI[i-1,15:Mnage] + (1 - CDR*CoT)*(e*new_NI[i,15:Mnage]) - (n + u[15:Mnage] + uni + w + hiv[15:Mnage])*NI[i-1,15:Mnage]*dt                    
        
        ####•••••••••••••••••••• TB HIV model •••••••••••••••••
        SH[i,15:Mnage] = SH[i-1,15:Mnage] - (uH[15:Mnage] + lambda[i-1])*SH[i-1,15:Mnage]*dt + hiv[15:Mnage]*S[i-1,15:Mnage]*dt 
        LH[i,15:Mnage] = LH[i-1,15:Mnage] + lambda[i-1]*(1 - pHA)*(SH[i-1,15:Mnage] + gHA*RH[i-1,15:Mnage])*dt - (vHA + lambda[i-1]*pHA*xHA + uH[15:Mnage])*LH[i-1,15:Mnage]*dt + hiv[15:Mnage]*L[i-1,15:Mnage]*dt 
        
        new_IH[i,15:Mnage] = lambda[i-1]*pHA*fH*(SH[i-1,15:Mnage] + gHA*RH[i-1,15:Mnage])*dt + (vHA + lambda[i-1]*pHA*xHA)*fH*LH[i-1,15:Mnage]*dt + rHA*hH*RH[i-1,15:Mnage]*dt + w*NIH[i-1,15:Mnage]*dt
        new_NIH[i,15:Mnage] = lambda[i-1]*pHA*(1 - fH)*(SH[i-1,15:Mnage] + gHA*RH[i-1,15:Mnage])*dt + (vHA + lambda[i-1]*pHA*xHA)*(1 - fH)*LH[i-1,15:Mnage]*dt + rHA*(1 - hH)*RH[i-1,15:Mnage]*dt  
        
        RH[i,15:Mnage] = RH[i-1,15:Mnage] + nH*(IH[i-1,15:Mnage] + NIH[i-1,15:Mnage])*dt + CDRH*CoTH*(new_IH[i-1,15:Mnage] + e*new_NIH[i-1,15:Mnage]) - (rHA + gHA*lambda[i-1] + uH[15:Mnage])*RH[i-1,15:Mnage]*dt + hiv[15:Mnage]*R[i-1,15:Mnage]*dt
        IH[i,15:Mnage] = IH[i-1,15:Mnage] + (1 - CDRH*CoTH)*(new_IH[i-1,15:Mnage]) - (nH + uH[15:Mnage] + uiHA)*IH[i-1,15:Mnage]*dt + hiv[15:Mnage]*I[i-1,15:Mnage]*dt
        NIH[i,15:Mnage] = NIH[i-1,15:Mnage] + (1 - CDRH*CoTH)*(e*new_NIH[i-1,15:Mnage]) - (nH + uH[15:Mnage] + uniHA + w)*NIH[i-1,15:Mnage]*dt + hiv[15:Mnage]*NI[i-1,15:Mnage]*dt                
        
        #### •••••••••••••••••••• VACCINE AGING •••••••••••••••••••••••••
        
        Sv[i,1:14] = Sv[i-1,1:14] - (u[1:14])*Sv[i-1,1:14]*dt - lambda[i-1]*(Sv[i-1,1:14])*dt 
        Lv[i,1:14] = Lv[i-1,1:14] - (u[1:14])*Lv[i-1,1:14]*dt + lambda[i-1]*(Sv[i-1,1:14] + g*Rv[i-1,1:14])*dt 
        Rv[i,1:14] = Rv[i-1,1:14] - (u[1:14])*Rv[i-1,1:14]*dt - lambda[i-1]*(g*Rv[i-1,1:14])*dt
        
        Sv[i,15:Mnage] = Sv[i-1,15:Mnage] - (u[15:Mnage] + hiv[15:Mnage])*Sv[i-1,15:Mnage]*dt - lambda[i-1]*(Sv[i-1,15:Mnage])*dt 
        Lv[i,15:Mnage] = Lv[i-1,15:Mnage] - (u[15:Mnage] + hiv[15:Mnage])*Lv[i-1,15:Mnage]*dt + lambda[i-1]*(Sv[i-1,15:Mnage] + g*Rv[i-1,15:Mnage])*dt
        Rv[i,15:Mnage] = Rv[i-1,15:Mnage] - (u[15:Mnage] + hiv[15:Mnage])*Rv[i-1,15:Mnage]*dt - lambda[i-1]*(g*Rv[i-1,15:Mnage])*dt
        
        SvH[i,15:Mnage] = SvH[i-1,15:Mnage] + hiv[15:Mnage]*Sv[i-1,15:Mnage]*dt - (uH[15:Mnage])*SvH[i-1,15:Mnage]*dt - lambda[i-1]*(SvH[i-1,15:Mnage])*dt 
        LvH[i,15:Mnage] = LvH[i-1,15:Mnage] + hiv[15:Mnage]*Lv[i-1,15:Mnage]*dt - (uH[15:Mnage])*LvH[i-1,15:Mnage]*dt + lambda[i-1]*(SvH[i-1,15:Mnage] + gHA*RvH[i-1,15:Mnage])*dt 
        RvH[i,15:Mnage] = RvH[i-1,15:Mnage] + hiv[15:Mnage]*Rv[i-1,15:Mnage]*dt - (uH[15:Mnage])*RvH[i-1,15:Mnage]*dt - lambda[i-1]*(gHA*RvH[i-1,15:Mnage])*dt 
        
        ###•••••••••••••••••• Vaccine coverage and duration ••••••••••••••••
        # NUMBER OF VACCINES
        if(vaccine == 0){VX[i,1:3]<-0}
        if(vaccine == 1){VX[i,1]<-sum(thetaV1[i,]*(S[i,]+L[i,]+R[i,]))}
        if(vaccine == 2){VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2a[i,]*(SH[i,]+LH[i,]+RH[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2m[i,]*(SH[i,]+LH[i,]+RH[i,]))}
        if(vaccine == 3){VX[i,1]<-sum(thetaV1[i,]*(S[i,]+L[i,]+R[i,])); VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2a[i,]*(SH[i,]+LH[i,]+RH[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2m[i,]*(SH[i,]+LH[i,]+RH[i,]))}
        
        ####•••••••••••••••••• Vaccination and Removal of protection
        
        S2 = S[i,] + Sv[i,]*(d[i,]*(1-theta[i,])) - theta[i,]*S[i,]
        L2 = L[i,] + Lv[i,]*(d[i,]*(1-theta[i,])) - theta[i,]*L[i,]
        R2 = R[i,] + Rv[i,]*(d[i,]*(1-theta[i,])) - theta[i,]*R[i,]
        
        SH2 = SH[i,] + SvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*SH[i,]
        LH2 = LH[i,] + LvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*LH[i,]
        RH2 = RH[i,] + RvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*RH[i,]
        
        Sv2 = Sv[i,] - Sv[i,]*(d[i,]*(1-theta[i,])) + theta[i,]*S[i,]
        Lv2 = Lv[i,] - Lv[i,]*(d[i,]*(1-theta[i,])) + theta[i,]*L[i,]
        Rv2 = Rv[i,] - Rv[i,]*(d[i,]*(1-theta[i,])) + theta[i,]*R[i,]
        
        SvH2 = SvH[i,] - SvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*SH[i,]
        LvH2 = LvH[i,] - LvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*LH[i,]
        RvH2 = RvH[i,] - RvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*RH[i,]
        
        S[i,]<-S2;L[i,]<-L2;R[i,]<-R2;       SH[i,]<-SH2;LH[i,]<-LH2;RH[i,]<-RH2;
        Sv[i,]<-Sv2;Lv[i,]<-Lv2;Rv[i,]<-Rv2; SvH[i,]<-SvH2;LvH[i,]<-LvH2;RvH[i,]<-RvH2;
        
        
        ####•••••••••••••••••• Economic Output ••••••••••••••••••
        #### Output for cost-effectiveness 
        ## POPULATION SIZE 
        #are vectors set up for psize elsewhere???
        psize[i]<-sum(S[i,],L[i,],R[i,],I[i,],NI[i,],Sv[i,],Lv[i,],Rv[i,])
        #print(c("PSIZE",i,psize[i]))
        #see earlier query on ages???
        #ages needed to fit to incidence and population size
        psize014[i]<-sum(S[ind,1:15],L[ind,1:15],R[ind,1:15],I[ind,1:15],NI[ind,1:15],Sv[ind,1:15],Lv[ind,1:15],Rv[ind,1:15])
        psize1554[i]<-sum(S[ind,16:55],L[ind,16:55],R[ind,16:55],I[ind,16:55],NI[ind,16:55],Sv[ind,16:55],Lv[ind,16:55],Rv[ind,16:55])
        psize5564[i]<-sum(S[ind,56:65],L[ind,56:65],R[ind,56:65],I[ind,56:65],NI[ind,56:65],Sv[ind,56:65],Lv[ind,56:65],Rv[ind,56:65])
        psize65plus[i]<-sum(S[ind,66:Mnage],L[ind,66:Mnage],R[ind,66:Mnage],I[ind,66:Mnage],NI[ind,66:Mnage],Sv[ind,66:Mnage],Lv[ind,66:Mnage],Rv[ind,66:Mnage])
        #ages needed to fit to mort  and prevalence as have different groupings
        psize1559[i]<-sum(S[ind,16:60],L[ind,16:60],R[ind,16:60],I[ind,16:60],NI[ind,16:60],Sv[ind,16:60],Lv[ind,16:60],Rv[ind,16:60])
        psize1529[i]<-sum(S[ind,16:30],L[ind,16:30],R[ind,16:30],I[ind,16:30],NI[ind,16:30],Sv[ind,16:30],Lv[ind,16:30],Rv[ind,16:30])
        psize3044[i]<-sum(S[ind,31:45],L[ind,31:45],R[ind,31:45],I[ind,31:45],NI[ind,31:45],Sv[ind,31:45],Lv[ind,31:45],Rv[ind,31:45])
        psize4459[i]<-sum(S[ind,45:60],L[ind,45:60],R[ind,45:60],I[ind,45:60],NI[ind,45:60],Sv[ind,45:60],Lv[ind,45:60],Rv[ind,45:60])
        psize60plus[i]<-sum(S[ind,61:Mnage],L[ind,61:Mnage],R[ind,61:Mnage],I[ind,61:Mnage],NI[ind,61:Mnage],Sv[ind,61:Mnage],Lv[ind,61:Mnage],Rv[ind,61:Mnage])
        
        ## number vaccinated
        #need to set up matrix for psizevacc to be recorded in to**
        #psizevacc[i]<-sum(Sv[i,],Lv[i,],Rv[i,])
        #nmbvacc<-psizevacc[i]-psizevacc[i-1]
        
        
        ## Death markers
        # Number of TB deaths
        TBDeaths[i,]=dt*((ui)*I[i-1,]+(uni)*NI[i-1,]);
        
        # Age deaths TB and background ##why age deaths???
        ADeaths[i,]=dt*(u*S[i-1,]+u*L[i-1,]+(u+ui)*I[i-1,]+(u+uni)*NI[i-1,]+u*R[i-1,]+u*Sv[i-1,]+u*Lv[i-1,]+u*Rv[i-1,])

        # Deaths matrix holds all death indices
        # Columns: Number deaths HIV-, av age at death av. age HIV death, number HIV+ deaths, av age HIV+ death, av. age death
        Deaths[i,1]=sum(ADeaths[i,]);   Deaths[i,2]=sum(ADeaths[i,]*seq(1:Mnage))/sum(ADeaths[i,])
        #Deaths[i,3]=sum(ADeathsH[i,]);  Deaths[i,4]=sum(ADeathsH[i,]*seq(1:Mnage))/sum(ADeathsH[i,])
        #Deaths[i,5]=sum((ADeathsH[i,]+ADeaths[i,])*seq(1:Mnage))/sum(ADeathsH[i,]+ADeaths[i,])
        
        ## NUMBER ON TREATMENT & NUMBER SUCCESSFULLY TREATED
        # TBRx columns: HIV- detected, successfully treated, HIV+ detected, successfully treated
        TBRx[i,1]=CDR*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]));    TBRx[i,2]=CDR*(CoT)*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]))
        #TBRx[i,3]=CDRH*(sum(new_IH[i-1,])+e*sum(new_NIH[i-1,]));  TBRx[i,4]=CDRH*(CoTH)*(sum(new_IH[i-1,])+e*sum(new_NI[i-1,]))
        
        # If last year
        #is this therefore only returnign incidence rate for 2050??? OR is it last timepoint of the year?? I dont understand the if statement. 
       
        if(i == ((1/dt)*(k-year1)+1/dt)){
          i1<-((1/dt)*(k-year1)+1); i2<-((1/dt)*(k-year1)+1/dt)
          # TB INCIDENCE AND MORTALITY etc for MODEL FITTING and RESEARCH OUTCOMES
          #print(c(i,TBI[1,],sum(new_I[i1:i2,],new_NI[i1:i2,]),mean(psize[i1:i2])))
          # Yearly average PSIZE, Incidence and mortality
          ## (1) population size - need for 2010 also???
          PSIZEy[(k-year1+1),1]<-mean(psize[i1:i2]);
          PSIZEy[(k-year1+1),2]<-mean(psize014[i1:i2,1:15])
          PSIZEy[(k-year1+1),3]<-mean(psize1554[i1:i2,16:55])
          PSIZEy[(k-year1+1),4]<-mean(psize5564[i1:i2,56:65])
          PSIZEy[(k-year1+1),5]<-mean(psize65plus[i1:i2,66:Mnage])
          PSIZEy[(k-year1+1),6]<-mean(psize1559[i1:i2,16:60])
          PSIZEy[(k-year1+1),7]<-mean(psize1529[i1:i2,16:30])
          PSIZEy[(k-year1+1),8]<-mean(psize3044[i1:i2,31:45])
          PSIZEy[(k-year1+1),9]<-mean(psize4459[i1:i2,45:60])
          PSIZEy[(k-year1+1),10]<-mean(psize60plus[i1:i2,61:Mnage])

          ## (2) TB incidence rate
          #where TBI and TBM set up so can add rows for additional TBI estimates???
          TBI[(k-year1+1),1]<-100000*sum(new_I[i1:i2,],new_NI[i1:i2,])/mean(psize[i1:i2])
          TBI[(k-year1+1),2]<-100000*sum(new_I[i1:i2,1:15],new_NI[i1:i2,1:15])/mean(psize014[i1:i2])
          TBI[(k-year1+1),3]<-100000*sum(new_I[i1:i2,16:55],new_NI[i1:i2,16:55])/mean(psize1554[i1:i2])
          TBI[(k-year1+1),4]<-100000*sum(new_I[i1:i2,56:65],new_NI[i1:i2,56:65])/mean(psize5564[i1:i2])
          TBI[(k-year1+1),5]<-100000*sum(new_I[i1:i2,66:Mnage],new_NI[i1:i2,66:Mnage])/mean(psize65plus[i1:i2])
          
          ## (3) TB prevalence rate (need for 2000 and 2010)???
          TBP[(k-year1+1),1]<-100000*sum(I[i1:i2,],NI[i1:i2,])/mean(psize[i1:i2])
          TBP[(k-year1+1),2]<-100000*sum(I[i1:i2,1:15],NI[i1:i2,1:15])/mean(psize014[i1:i2])
          TBP[(k-year1+1),3]<-100000*sum(I[i1:i2,16:30],NI[i1:i2,16:30])/mean(psize1529[i1:i2])
          TBP[(k-year1+1),4]<-100000*sum(I[i1:i2,31:45],NI[i1:i2,31:45])/mean(psize3044[i1:i2])
          TBP[(k-year1+1),5]<-100000*sum(I[i1:i2,46:60],NI[i1:i2,46:60])/mean(psize4559[i1:i2])
          TBP[(k-year1+1),6]<-100000*sum(I[i1:i2,60:Mnage],NI[i1:i2,60:Mnage])/mean(psize60plus[i1:i2])
          
          ## (4) TB mortality
          #print(c("IH",sum(new_IH[i,],new_NIH[i,]),TBI[i,2]))
          TBM[(k-year1+1),1]<-100000*sum(TBDeaths[i1:i2,])/mean(psize[i1:i2])
          TBM[(k-year1+1),2]<-100000*sum(TBDeaths[i1:i2,1:15])/mean(psize014[i1:i2])
          TBM[(k-year1+1),3]<-100000*sum(TBDeaths[i1:i2,16:55])/mean(psize1554[i1:i2])
          TBM[(k-year1+1),4]<-100000*sum(TBDeaths[i1:i2,56:65])/mean(psize5564[i1:i2])
          TBM[(k-year1+1),5]<-100000*sum(TBDeaths[i1:i2,66:Mnage])/mean(psize65plus[i1:i2])
          TBM[(k-year1+1),6]<-100000*sum(TBDeaths[i1:i2,16:60])/mean(psize1559[i1:i2])
          TBM[(k-year1+1),7]<-100000*sum(TBDeaths[i1:i2,61:Mnage])/mean(psize60plus[i1:i2])
          
          ## (5) Prevalence of infection - in case get data to fit to. WHAT ABOUT RECOVERDS???
          TBPI[(k-year1+1),1]<-100000*sum(L[i1:i2,])/mean(psize[i1:i2])
          TBPI[(k-year1+1),2]<-100000*sum(L[i1:i2,1:15])/mean(psize014[i1:i2])
          TBPI[(k-year1+1),3]<-100000*sum(L[i1:i2,16:55])/mean(psize1554[i1:i2])
          TBPI[(k-year1+1),4]<-100000*sum(L[i1:i2,56:65])/mean(psize5564[i1:i2])
          TBPI[(k-year1+1),5]<-100000*sum(L[i1:i2,66:Mnage])/mean(psize65plus[i1:i2])
          
          #### FOR ADDITIONAL RESEARCH OUTCOMES
          
          # % of transmission due to the elderly
          ##worried this wont sum across the right things, as want to sum across the i's cycled through the j's??? Do I need to loop one of them?
          eldtrans[(k-year1+1)]<- 100* (sum(lambda[i1:i2,66:Mnage] * I[i1:i2,66:Mnage])/(i1-i2)) / (sum(lambda[i1:i2,] * I[i1:i2,])/(i1-i2))
          
        }
        ####••••••••••••••••• END OF MIDDLE YEAR RUNS
      }
    }
  }
  
  ## Outputs - in R, allows output to be seen without expressly wanting it - what is this???
  assign('S',S,envir = .GlobalEnv);assign('L',L,envir = .GlobalEnv);assign('I',I,envir = .GlobalEnv);assign('NI',NI,envir = .GlobalEnv);assign('R',R,envir = .GlobalEnv);assign('new_I',new_I,envir = .GlobalEnv);assign('new_NI',new_NI,envir = .GlobalEnv)
  assign('SH',SH,envir = .GlobalEnv);assign('LH',LH,envir = .GlobalEnv);assign('IH',IH,envir = .GlobalEnv);assign('NIH',NIH,envir = .GlobalEnv);assign('RH',RH,envir = .GlobalEnv);assign('new_IH',new_IH,envir = .GlobalEnv);assign('new_NIH',new_NIH,envir = .GlobalEnv)
  assign('Sv',Sv,envir = .GlobalEnv);assign('Lv',Lv,envir = .GlobalEnv);assign('Rv',Rv,envir = .GlobalEnv);assign('SvH',SvH,envir = .GlobalEnv);assign('LvH',LvH,envir = .GlobalEnv);assign('RvH',RvH,envir = .GlobalEnv);
  assign('lambda',lambda,envir=.GlobalEnv);assign('theta',theta,envir=.GlobalEnv);assign('d',d,envir=.GlobalEnv);
  assign('TBDeaths',TBDeaths,envir=.GlobalEnv);assign('TBDeathsH',TBDeathsH,envir=.GlobalEnv);
  assign('AllDeathsH',AllDeathsH,envir=.GlobalEnv);assign('ADeaths',ADeaths,envir=.GlobalEnv);assign('ADeathsH',ADeathsH,envir=.GlobalEnv);
  assign('Deaths',Deaths,envir=.GlobalEnv);assign('u',u,envir=.GlobalEnv);assign('hiv',hiv,envir=.GlobalEnv);
  assign('psize',psize,envir=.GlobalEnv);assign('bv',bv,envir=.GlobalEnv);assign('prevHIV',prevHIV,envir=.GlobalEnv);assign('prevHIV1549',prevHIV1549,envir=.GlobalEnv);
  assign('TBI',TBI,envir=.GlobalEnv);assign('TBM',TBM,envir=.GlobalEnv);assign('TBRx',TBRx,envir=.GlobalEnv);assign('VX',VX,envir=.GlobalEnv);
  assign('Econout',EconOut,envir=.GlobalEnv);assign('Out',Out,envir=.GlobalEnv);assign('hbcout',hbcOut,envir=.GlobalEnv);
  
  ## CUMULATIVE RESEARCH OUTCOMES
  totmort<- matrix(0,1,7)
  colnames(totmort)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "60+")
  totmort[,1]<- sum(TBM[,1])
  totmort[,2]<- sum(TBM[,2])
  totmort[,3]<- sum(TBM[,3])
  totmort[,4]<- sum(TBM[,4])
  totmort[,5]<- sum(TBM[,5])
  totmort[,6]<- sum(TBM[,6])
  totmort[,7]<- sum(TBM[,7])
  
  totcase<- matrix(0,1,5)
  colnames(totcase)<-c("All ages","0-14", "15-54", "55-64", "65+")
  totcase[,1]<- sum(TBI[,1])
  totcase[,2]<- sum(TBI[,2])
  totcase[,3]<- sum(TBI[,3])
  totcase[,4]<- sum(TBI[,4])
  totcase[,5]<- sum(TBI[,5])
  
  
  ## Actual Output required (collected as progressed with model)
  # need to update this. what does Ana do???
  X<-cbind(psize,rowSums(S),S[,1],TBI[,1],TBI[,2],TBM[,1],TBM[,2],PSIZEy,rowSums(I),rowSums(NI),prevHIV,prevHIV1549,rowSums(L),rowSums(new_I),rowSums(new_NI),rowSums(new_IH),rowSums(new_NI))
  colnames(X)<-c("PSIZE","S","Births","TBI","TBIH","TBM","TBMH","YearPsize","I","NI","PHIV","PHIV1549","L","new_I","new_NI","new_IH","new_NIH")
  X<-data.frame(X)
  # To show
  Ana<-matrix(0,1,6);Ana[1:2]<-Popsize[1:2,cntry];Ana[3]<-as.numeric(TBIm[cntry]);Ana[4]<-as.numeric(TBMm[cntry]);Ana[5]<-as.numeric(TBIHIVm[cntry]);Ana[6]<-as.numeric(TBMHIVm[cntry]);
  show<-c("FIT",round(Fit,2),"TBI/M/psz 2009/psz 2050/TBIH/TBMH",round(TBI[2009-year1+1,1],2),round(TBM[2009-year1+1,1],2),round(psize[(2009-year1)*(1/dt)+1],2),round(psize[(2050-year1)*(1/dt)+1],2),round(TBI[2009-year1+1,2],2),round(TBM[2009-year1+1,2],2),"Data",round(Ana,2),"prop",round(prop,2))
  #print(show)
  
  # Record of every run
  #if (C==0){setwd(paste("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel/fitout/",cntry,"/",sep=''))
  #          write.csv(t(show),paste(date(),'.csv',sep=''))
  #          setwd("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel")}
  #if (C==1){setwd(paste("/users/eidegkni/Documents/vaccine/fit/fitout/",cntry,"/",sep=''))
  #          write.csv(t(show),paste(date(),'.csv',sep=''))
  #          setwd("/users/eidegkni/Documents/vaccine")}
  
  # If want to see plots 
  if (Plot==1){source("#Plot.R")}
  
  # What outputting
  return(X)
}
