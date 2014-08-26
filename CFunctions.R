#### All functions needed 


#### MAIN Function to generate model output
# Takes in country, vaccine details, fit parameters, initial values, time for simulation, whether to plot, cluster or not
FitGo <- function(cntry,Vx,Fit,InitV,TimeScale,Plot,C){
  # Cntry <- country name; 
  # Vx <- vector of vaccine properties. c(vxtype, coverage, efficacy, duration). If blank then no vaccine (for fit)
  # Fit <- 7 fit parameters
  # InitV <- vector of initial values: Run number, timestep, proportion TB initially (either 1 or four = ? )
  # TimeScale <- c(year1, yearend)
  # Plot <- whether to plot or not (1 = yes)
  
  # C is the cluster parameter -> C==0 if on laptop
  #if (C == 0){setwd("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel/")}
  #if (C == 1){setwd("/users/eidegkni/Documents/vaccine")}
  
  # Paranames in Fit and InitV
  FitV<-c('psz1900','rmort','neta','rmortTB','CDRscale','CDRscaleE','alpha')
  InitialV<-c('run','dt','prop') 
  
  #### Run model with these input parameters
  # Assign parameters to values inputted
  if(length(InitV)==3){for(i in 1:length(InitV)){assign(InitialV[i],InitV[i],envir = .GlobalEnv)}
  } else {for(i in 1:2){assign(InitialV[i],as.numeric(InitV[i]),envir = .GlobalEnv)};assign(InitialV[3],as.numeric(InitV[3:6]),envir = .GlobalEnv)}
  assign('cntry',cntry,envir = .GlobalEnv)
  assign('year1',TimeScale[1],envir = .GlobalEnv); assign('yearend',TimeScale[2],envir = .GlobalEnv)
  ## Parameters that are kept in but don't use in this version: different progression proportions from Stover
  g<-x; gH<-xH;
  hchild<-fchild; hadult<-fadult; hH<-fH; helderly<-felderly
  
  #identify number of years in each age category, create vectors of age-specific parameters for adults vs kids vs elderly
  chiyrs<-15
  aduyrs<-50
  eldyrs<-(Mnage-chiyrs-aduyrs)
  
  p=c((rep(pchild, l=chiyrs)),(rep(padult, l=aduyrs)),(rep(pelderly, l=eldyrs)))
  f=c((rep(fchild, l=chiyrs)),(rep(fadult, l=aduyrs)),(rep(felderly, l=eldyrs)))
  h=c((rep(hchild, l=chiyrs)),(rep(hadult, l=aduyrs)),(rep(helderly, l=eldyrs)))
  v=c((rep(vchild, l=chiyrs)),(rep(vadult, l=aduyrs)),(rep(velderly, l=eldyrs)))
  r=c((rep(rchild, l=chiyrs)),(rep(radult, l=aduyrs)),(rep(relderly, l=eldyrs)))
  n=c((rep(n, l=chiyrs)),(rep(n, l=aduyrs)),(rep(nelderly, l=eldyrs)))
  print("doneeldyears")
  
  # Timesteps with inputted start end and timestep
  times<-seq(year1,yearend,dt);steps<-length(times);
  
  # Country data (may be overruled by FIT values)
  source("#CountryCalc.R")
  
  print("done country calc")
  for(i in 1:length(Fit)){assign(FitV[i],Fit[i],envir = .GlobalEnv)} # Only assign those variables that want to 
  #print(c("neta",neta))
  # FIT: rmortTB = proportion multiply TB mortality by. Range=[-1,1].
  if(length(Fit)>3){if (rmortTB<0){ui<-rmortTB*(ui)+ui; uni<-rmortTB*(uni)+uni;
  } else {ui<-rmortTB*(1-ui)+ui; uni<-rmortTB*(1-uni)+uni}
  }
  # u and ui use age depandent pattern and apply to existing number
  uichild<-ui*uiscaleC
  uiadult<-ui*uiscaleA
  uielderly<-ui*uiscaleE
  unichild<-uni*uiscaleC
  uniadult<-uni*uiscaleA
  unielderly<-uni*uiscaleE
    
  ui=c((rep(uichild, l=chiyrs)),(rep(uiadult, l=aduyrs)),(rep(uielderly, l=eldyrs)))
  uni=c((rep(unichild, l=chiyrs)),(rep(uniadult, l=aduyrs)),(rep(unielderly, l=eldyrs)))
  
 
  
  # If don't assign CDRscale set it to 1 (i.e. use data)
  if(length(Fit)<5){CDRscale <- 1}
  print('cdrscale set')
  ## Generate Vaccine specific data using above eff and D if specified in input
  if(length(Vx)>1){
    assign('vaccine',Vx[1],envir = .GlobalEnv); assign('coverage',Vx[2],envir = .GlobalEnv); assign('eff',Vx[3],envir = .GlobalEnv); assign('D',Vx[4],envir = .GlobalEnv)
    source("#VxGen.R")
  } else {d<-matrix(0,steps,Mnage); 
          thetaS<-matrix(0,steps,Mnage);
          thetaL<-matrix(0,steps,Mnage)
          thetaR<-matrix(0,steps,Mnage)
          vaccine<-0;#thetaH<-theta;
          eff<-0;Dur<-0;coverage<-0
          }
  print("Done vacc gen")
  
  ## Initialise
  source("#Init.R")
  
  print("done init")
  
  ## To check with fitting vector
  #yyy<-c(pchild,padult,pH,v,vH,x,xH,fchild,fadult,fH,w,n,nH,r,rH,e,g,gH,hchild,hadult,hH,LEHIV,LEART,effH,effHa,rmort,neta,rmortTB,CDRscale,alpha)
  #if(sum(xxx-yyy)>0){print(c("AT",which(xxx-yyy>0),"yyy",yyy,"error with para"))}else{print("para match")}
  
  # # # # # # RUN 
  for (k in year1:(yearend)){
    print(k)
    
    #### •••••••••••••••••• Yearly parameters #••••••••••••••••••
    # Only start marking years after 2009 

    if (k <= 2010){ yr <- 2010 } else { yr <- k }
    
    
    #### MORTALITY. Runs from age 1 to age 101 (equiv 0-100yo)
    # FIT: Rmort multiplies background death rates. Range [-1,1]
    #is in k loop, so generates a new u (vector of length number of ages) each year
    if (rmort < 0) {u<-as.vector(rmort*(mort[1+yr-2010,2:102]) + mort[1+yr-2010,2:102])#;names(u)<-NULL;#print(c("neg","u",u,"rmort",rmort)) 
    } else {u<-as.vector(rmort*(1-(mort[1+yr-2010,2:102])) + mort[1+yr-2010,2:102])#;names(u)<-NULL;#print(c("u",u,"rmort",rmort))
    }
    
    #print(c("U",u,mort[1+yr-2010,2:102],rmort))
    #u<-mort[1+yr-2010,2:102]
    #print(u)
    #### HIV MORTALITY (weighted by ART coverage)
    #uH <- u + 1/( (1 - art[1+yr-2010]) * LEHIV + art[1+yr-2010] * LEART)
    
    #### HIV Incidence
    # Take the same value before 2010, otherwise the year's values
    #if (k == year1){ ind <- 1 } else { ind <- ( (1/dt) * (k - year1) ) }
    # Population size in each of the groups (15-49yos, same but HIV+)
    #psize1549<-sum(S[ind,15:49],L[ind,15:49],R[ind,15:49],I[ind,15:49],NI[ind,15:49],Sv[ind,15:49],Lv[ind,15:49],Rv[ind,15:49])
  
    #print(c(psize1549,psizeH1549,psize1549))
    # Proportion who become HIV+ is then weighted to only be in the 15-49yo pop that is hiv-
    #hiv<-hivI/(100*(psize1549/(psizeH1549+psize1549)))
    
    #### HIV specific parameters - yearly ART variation and vH (could go earlier)
    # FIT: Alpha multiplies the progression rates for HIV+s. Range [-1,1] 
    #if (length(Fit) > 5){if (alpha < 0) {pH2 <- alpha*pH + pH; vH2 <- alpha*vH + vH; rH2 <- alpha*rH + rH
    #} else {pH2 <- alpha*(1 - pH) + pH; vH2 <- alpha*(1 - vH) + vH; rH2 <- alpha*(1 - rH) + rH
    #}} else {pH2<-pH; vH2<-vH; rH2<-rH}
    
    # Calculate the ART weighted averages for progression
    #pHA<-pH2*(1-art[1+yr-2010]) + art[1+yr-2010] * 0.35 * pH2 
    #vHA<-vH2*(1-art[1+yr-2010]) + art[1+yr-2010] * 0.35 * vH2 
    #rHA<-rH2*(1-art[1+yr-2010]) + art[1+yr-2010] * 0.35 * rH2 
    #xHA<-xH*(1-art[1+yr-2010]) + art[1+yr-2010] * 0.35 * xH
    #gHA<-gH*(1-art[1+yr-2010]) + art[1+yr-2010] * 0.35 * gH
    
    # And for mortality 
    #uiHA<-(1-art[1+yr-2010])*uiH + art[1+yr-2010] * 0.25 * uiH 
    #uniHA<-(1-art[1+yr-2010])*uniH + art[1+yr-2010] * 0.25 * uniH 
    
    # And vaccine efficacy
    #effHA<-effH*(1-art[1+yr-2010]) + art[1+yr-2010] * effHa
    #thetaH<-thetaH*effHA
    
    # print(c('ui',ui,'uiH',uiH,'uiHA',uiHA,'uniHA',uniHA))
    # print(c('pH',pH,'pHA',pHA,'vHa',vHA,'xHA',xHA,'rHA',rHA,'art',art[20],'alpha',alpha))
    
    # yr changed for some parameters
    if (k <= 1990){ CDR_yr <- 1990 } else { CDR_yr <- k }
    if (k <= 1994){ CoT_yr <- 1994 } else { CoT_yr <- k }
    
    #### CDR & TREATMENT SUCCESS (a proportion of cases that are found and successfully treated)
    # FIT: CDRscale multiplies the cdr value for both HIV+s and HIV-s
  
    CDRscales<-c((rep(CDRscale, l=chiyrs)),(rep(CDRscale, l=aduyrs)),(rep(CDRscaleE, l=eldyrs)))
    CDRscaled<-CDRscales[1:Mnage]*cdr[,1:Mnage];
    CDR<-CDRscaled[1+CDR_yr-1990,]
    
    #CDRH<-CDRscale*cdrH[1+yr-2010];
    CoT<-suctt[1+CoT_yr-1994];#CoTH<-sucttH[1+yr-2010];
    #print(c(yr,'CDRH',CDRH,'CoT',CoT,'CoTH',CoTH,1+yr-2010))
    
    #### BIRTHS 
    # Need to have 2010 birth RATE pre-2010 else won't get curve 
    #added in a step pre-2010 to account for chinese one-child policy impact on population structure
    #one child policy started in 1979. early numbers are 1980 population (984016) and av fertility in 1975-1980 (20887) (average number of births in the 1950-1980period would be 24000, so could go higher)
    fertdrop<-1980
    e_bb<-20887
    e_pop<-984016
#     e_bb<-20887
#     e_pop<-984016
    
    if (k < fertdrop){ br<-e_bb/e_pop
                      if (k == year1){B<-round(br*psize[1]); bv<-c(bv,B)}
                      else { B<-round(br*psize[((k-year1)*(1/dt))]); bv<-c(bv,B);}} 
    else{
    if (k < 2010) { br<-bb[1]/(Popsize[1,cntry]);
                    B<-round(br*psize[((k-year1)*(1/dt))]); bv<-c(bv,B);
    } else { B<-bb[1+yr-2010] }}
    
#     if (k < 2010) { br<-bb[1]/(Popsize[1,cntry])
#                 
#                     if (k == year1){B<-round(br*psize[1]); bv<-c(bv,B)             
#                     } else { B<-round(br*psize[((k-year1)*(1/dt))]); bv<-c(bv,B);}     
#     } else { B<-bb[1+yr-2010] }
    print(c("BIRTHS",br,B,psize[((k-year1)*(1/dt))],((k-year1)*(1/dt))))
    BIRTHS[i]<-B
    ####•••••••••••••••••• END OF YEARLY PARAMETERS
    
    print("done params")
    
    #### Start of model run
    if (k > year1){ # Start of year 1 is the initial condition
      ## These are those where the time step is the first of the year
      i = ((1/dt)*(k-year1)+1) # Time zero is first row
      start <- 1 # whether first ts of year. All year dependent parameters must be last years.
      # FIT: Takes in the neta defined as an input
      print("pre-start lambda")
      #lambda[i-1] <- (1 - exp(-(neta) * z * ((sum(I[i-1,])/(psize[i-1])))))
      #1-exp turns rate->probability
      lambda[i-1,1:Mnage] <- t(neta * (1-exp(colSums(-(myneta[1:4,1:Mnage]) * z * ((Imatrix[i-1,1:4])/(psizematrix[i-1,1:4]))))))
                      
      print("post-start lambda")
      ####•••••••••••••••••• TB model ••••••••••••••••••
      # Age 1, first time step of the year, all births occur
      
      j = 1; S[i,j]<-B #B is set by year by lines BIRTHS above    
      
      
      S[i,2:Mnage] = S[i-1,1:(Mnage-1)] - (u[1:Mnage-1]+lambda[i-1,1:(Mnage-1)])*S[i-1,1:(Mnage-1)]*dt 
      L[i,2:Mnage] = L[i-1,1:(Mnage-1)] + lambda[i-1,1:(Mnage-1)]*(1 - p[1:(Mnage-1)])*(S[i-1,1:(Mnage-1)] + g*R[i-1,1:(Mnage-1)])*dt - (v[1:(Mnage-1)] + lambda[i-1,1:(Mnage-1)]*p[1:(Mnage-1)]*x + u[1:(Mnage-1)])*L[i-1,1:(Mnage-1)]*dt 

      new_I_react[i,2:Mnage] = v[1:(Mnage-1)]*f[1:(Mnage-1)]*(L[i-1,1:(Mnage-1)])*dt + r[1:(Mnage-1)]*h[1:(Mnage-1)]*R[i-1,1:(Mnage-1)]*dt 
      new_NI_react[i,2:Mnage] =  v[1:(Mnage-1)]*(1 - f[1:(Mnage-1)])*L[i-1,1:(Mnage-1)]*dt + r[1:(Mnage-1)]*(1 - h[1:(Mnage-1)])*R[i-1,1:(Mnage-1)]*dt  
      
      new_I[i,2:Mnage] = lambda[i-1,1:(Mnage-1)]*p[1:(Mnage-1)]*f[1:(Mnage-1)]*(S[i-1,1:(Mnage-1)] + g*R[i-1,1:(Mnage-1)])*dt + (v[1:(Mnage-1)] + lambda[i-1,1:(Mnage-1)]*p[1:(Mnage-1)]*x)*f[1:(Mnage-1)]*L[i-1,1:(Mnage-1)]*dt + r[1:(Mnage-1)]*h[1:(Mnage-1)]*R[i-1,1:(Mnage-1)]*dt + w*NI[i-1,1:(Mnage-1)]*dt
      new_NI[i,2:Mnage] = lambda[i-1,1:(Mnage-1)]*p[1:(Mnage-1)]*(1 - f[1:(Mnage-1)])*(S[i-1,1:(Mnage-1)] + g*R[i-1,1:(Mnage-1)])*dt + (v[1:(Mnage-1)] + lambda[i-1,1:(Mnage-1)]*p[1:(Mnage-1)]*x)*(1 - f[1:(Mnage-1)])*L[i-1,1:(Mnage-1)]*dt + r[1:(Mnage-1)]*(1 - h[1:(Mnage-1)])*R[i-1,1:(Mnage-1)]*dt  
      
      R[i,2:Mnage] = R[i-1,1:(Mnage-1)] + n[1:(Mnage-1)]*(I[i-1,1:(Mnage-1)] + NI[i-1,1:(Mnage-1)])*dt + CDR[1:(Mnage-1)]*CoT*(new_I[i,2:Mnage] + e*new_NI[i,2:Mnage]) - (r[1:(Mnage-1)] + g*lambda[i-1,1:(Mnage-1)] + u[1:(Mnage-1)])*R[i-1,1:(Mnage-1)]*dt 
      I[i,2:Mnage] = I[i-1,1:(Mnage-1)] + (1 - CDR[1:(Mnage-1)]*CoT)*(new_I[i,2:Mnage]) - (n[1:(Mnage-1)] + u[1:(Mnage-1)] + ui[1:(Mnage-1)])*I[i-1,1:(Mnage-1)]*dt
      NI[i,2:Mnage] = NI[i-1,1:(Mnage-1)] + (1 - CDR[1:(Mnage-1)]*CoT)*(e*new_NI[i,2:Mnage]) - (n[1:(Mnage-1)] + u[1:(Mnage-1)] + uni[1:(Mnage-1)] + w)*NI[i-1,1:(Mnage-1)]*dt                    
      
      #if(I[i,2] < I[i-1,1]){print(c(i,I[i,2],I[i-1,1],"stop",(n + u[1:13] + ui),"cdr",CDR,CoT))}
      
      
      
      ####•••••••••••••••••••• TB HIV model •••••••••••••••••
      
      #SH[i,15:Mnage] = SH[i-1,14:(Mnage-1)] - (uH[14:(Mnage-1)] + lambda[i-1,1:Mnage])*SH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*S[i-1,14:(Mnage-1)]*dt 
      #LH[i,15:Mnage] = LH[i-1,14:(Mnage-1)] + lambda[i-1,1:Mnage]*(1 - pHA)*(SH[i-1,14:(Mnage-1)] + gHA*RH[i-1,14:(Mnage-1)])*dt - (vHA + lambda[i-1,1:Mnage]*pHA*xHA + uH[14:(Mnage-1)])*LH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*L[i-1,14:(Mnage-1)]*dt 
      
      #new_IH[i,15:Mnage] = lambda[i-1,1:Mnage]*pHA*fH*(SH[i-1,14:(Mnage-1)] + gHA*RH[i-1,14:(Mnage-1)])*dt + (vHA + lambda[i-1,1:Mnage]*pHA*xHA)*fH*LH[i-1,14:(Mnage-1)]*dt + rHA*hH*RH[i-1,14:(Mnage-1)]*dt + w*NIH[i-1,14:(Mnage-1)]*dt
      #new_NIH[i,15:Mnage] = lambda[i-1,1:Mnage]*pHA*(1 - fH)*(SH[i-1,14:(Mnage-1)] + gHA*RH[i-1,14:(Mnage-1)])*dt + (vHA + lambda[i-1,1:Mnage]*pHA*xHA)*(1 - fH)*LH[i-1,14:(Mnage-1)]*dt + rHA*(1 - hH)*RH[i-1,14:(Mnage-1)]*dt  
      
      #RH[i,15:Mnage] = RH[i-1,14:(Mnage-1)] + nH*(IH[i-1,14:(Mnage-1)] + NIH[i-1,14:(Mnage-1)])*dt + CDRH*CoTH*(new_IH[i-1,14:(Mnage-1)] + e*new_NIH[i-1,14:(Mnage-1)]) - (rHA + gHA*lambda[i-1,1:Mnage] + uH[14:(Mnage-1)])*RH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*R[i-1,14:(Mnage-1)]*dt
      #IH[i,15:Mnage] = IH[i-1,14:(Mnage-1)] + (1 - CDRH*CoTH)*(new_IH[i-1,14:(Mnage-1)]) - (nH + uH[14:(Mnage-1)] + uiHA)*IH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*I[i-1,14:(Mnage-1)]*dt
      #NIH[i,15:Mnage] = NIH[i-1,14:(Mnage-1)] + (1 - CDRH*CoTH)*(e*new_NIH[i-1,14:(Mnage-1)]) - (nH + uH[14:(Mnage-1)] + uniHA + w)*NIH[i-1,14:(Mnage-1)]*dt + hiv[14:(Mnage-1)]*NI[i-1,14:(Mnage-1)]*dt                
      
      ####•••••••••••••••••••••••••••••••••••••••••••••••••••••••• VACCINE STRATA AGING + INFECTION •••••••••••••••••••••••••
      ## Others have aged so need these to be reset to zero
      Sv[i,1] = 0;Lv[i,1] = 0;Rv[i,1] = 0; #SvH[i,1] = 0;LvH[i,1] = 0;RvH[i,1] = 0;
      
      Sv[i,2:Mnage] = Sv[i-1,1:(Mnage-1)] - (u[1:(Mnage-1)] + lambda[i-1,1:(Mnage-1)])*Sv[i-1,1:(Mnage-1)]*dt
      Lv[i,2:Mnage] = Lv[i-1,1:(Mnage-1)] - (u[1:(Mnage-1)])*Lv[i-1,1:(Mnage-1)]*dt + lambda[i-1,1:(Mnage-1)]*(Sv[i-1,1:(Mnage-1)] + g*Rv[i-1,1:(Mnage-1)])*dt
      Rv[i,2:Mnage] = Rv[i-1,1:(Mnage-1)] - (u[1:(Mnage-1)] + lambda[i-1,1:(Mnage-1)]*g)*Rv[i-1,1:(Mnage-1)]*dt
      
      #SvH[i,15:Mnage] = SvH[i-1,14:(Mnage-1)] + hiv[14:(Mnage-1)]*Sv[i-1,14:(Mnage-1)]*dt - (uH[14:(Mnage-1)])*SvH[i-1,14:(Mnage-1)]*dt - lambda[i-1,1:Mnage]*(SvH[i-1,14:(Mnage-1)])*dt
      #LvH[i,15:Mnage] = LvH[i-1,14:(Mnage-1)] + hiv[14:(Mnage-1)]*Lv[i-1,14:(Mnage-1)]*dt - (uH[14:(Mnage-1)])*LvH[i-1,14:(Mnage-1)]*dt + lambda[i-1,1:Mnage]*(SvH[i-1,14:(Mnage-1)] + gHA*RvH[i-1,14:(Mnage-1)])*dt
      #RvH[i,15:Mnage] = RvH[i-1,14:(Mnage-1)] + hiv[14:(Mnage-1)]*Rv[i-1,14:(Mnage-1)]*dt - (uH[14:(Mnage-1)])*RvH[i-1,14:(Mnage-1)]*dt - lambda[i-1,1:Mnage]*(gHA*RvH[i-1,14:(Mnage-1)])*dt          
      
      ###•••••••••••••••••• Vaccine coverage and duration ••••••••••••••••
      # NUMBER OF VACCINES (column 1=infant, 2=10yos, 3=mass)
      if(vaccine == 0){VX[i,1:3]<-0}
      #if(vaccine == 1){VX[i,1]<-sum(thetaV1[i,]*(S[i,]+L[i,]+R[i,]))}
      if(vaccine == 2){VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]))}
      #if(vaccine == 3){VX[i,1]<-sum(thetaV1[i,]*(S[i,]+L[i,]+R[i,])); VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2a[i,]*(SH[i,]+LH[i,]+RH[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]) + thetaV2m[i,]*(SH[i,]+LH[i,]+RH[i,]))}
      
      ##•••••••••••••••••• Vaccination campaign: age everyone and then implement (both vaccination and return)
      S2 = S[i,] + Sv[i,]*(d[i,]*(1-thetaS[i,])) - thetaS[i,]*S[i,]
      L2 = L[i,] + Lv[i,]*(d[i,]*(1-thetaL[i,])) - thetaL[i,]*L[i,]
      R2 = R[i,] + Rv[i,]*(d[i,]*(1-thetaR[i,])) - thetaR[i,]*R[i,]
      
      #SH2 = SH[i,] + SvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*SH[i,]
      #LH2 = LH[i,] + LvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*LH[i,]
      #RH2 = RH[i,] + RvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*RH[i,]
      
      Sv2 = Sv[i,] - Sv[i,]*(d[i,]*(1-thetaS[i,])) + thetaS[i,]*S[i,]
      Lv2 = Lv[i,] - Lv[i,]*(d[i,]*(1-thetaL[i,])) + thetaL[i,]*L[i,]
      Rv2 = Rv[i,] - Rv[i,]*(d[i,]*(1-thetaR[i,])) + thetaR[i,]*R[i,]
      
      #SvH2 = SvH[i,] - SvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*SH[i,]
      #LvH2 = LvH[i,] - LvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*LH[i,]
      #RvH2 = RvH[i,] - RvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*RH[i,]
      
      S[i,]<-S2;L[i,]<-L2;R[i,]<-R2;       #SH[i,]<-SH2;LH[i,]<-LH2;RH[i,]<-RH2;
      Sv[i,]<-Sv2;Lv[i,]<-Lv2;Rv[i,]<-Rv2; #SvH[i,]<-SvH2;LvH[i,]<-LvH2;RvH[i,]<-RvH2;
      
      ####•••••••••••••••••• Economic Output ••••••••••••••••••
      #### Output for cost-effectiveness 
      ## POPULATION SIZE
      psize[i]<-sum(S[i,],L[i,],R[i,],I[i,],NI[i,],Sv[i,],Lv[i,],Rv[i,])
      #print(c("PSIZE",i,psize[i]))
      #what is ind??? also, was written as 15-49 for 15-49, but isnt age 0 j=1, so should be 16:50 for age 15-49???
      #ages needed to fit to incidence and population size
      psize014[i]<-sum(S[i,1:15],L[i,1:15],R[i,1:15],I[i,1:15],NI[i,1:15],Sv[i,1:15],Lv[i,1:15],Rv[i,1:15])
      psize1554[i]<-sum(S[i,16:55],L[i,16:55],R[i,16:55],I[i,16:55],NI[i,16:55],Sv[i,16:55],Lv[i,16:55],Rv[i,16:55])
      psize5564[i]<-sum(S[i,56:65],L[i,56:65],R[i,56:65],I[i,56:65],NI[i,56:65],Sv[i,56:65],Lv[i,56:65],Rv[i,56:65])
      psize65plus[i]<-sum(S[i,66:Mnage],L[i,66:Mnage],R[i,66:Mnage],I[i,66:Mnage],NI[i,66:Mnage],Sv[i,66:Mnage],Lv[i,66:Mnage],Rv[i,66:Mnage])
      #ages needed to fit to mort  and prevalence as have different groupings
      psize1559[i]<-sum(S[i,16:60],L[i,16:60],R[i,16:60],I[i,16:60],NI[i,16:60],Sv[i,16:60],Lv[i,16:60],Rv[i,16:60])
      psize1529[i]<-sum(S[i,16:30],L[i,16:30],R[i,16:30],I[i,16:30],NI[i,16:30],Sv[i,16:30],Lv[i,16:30],Rv[i,16:30])
      psize3044[i]<-sum(S[i,31:45],L[i,31:45],R[i,31:45],I[i,31:45],NI[i,31:45],Sv[i,31:45],Lv[i,31:45],Rv[i,31:45])
      psize4559[i]<-sum(S[i,46:60],L[i,46:60],R[i,46:60],I[i,46:60],NI[i,46:60],Sv[i,46:60],Lv[i,46:60],Rv[i,46:60])
      psize60plus[i]<-sum(S[i,61:Mnage],L[i,61:Mnage],R[i,61:Mnage],I[i,61:Mnage],NI[i,61:Mnage],Sv[i,61:Mnage],Lv[i,61:Mnage],Rv[i,61:Mnage])
      #ages and incidence for contact matrices
      psizematrix[i,1]<-sum(S[i,1:6],L[i,1:6],R[i,1:6],I[i,1:6],NI[i,1:6],Sv[i,1:6],Lv[i,1:6],Rv[i,1:6])
      psizematrix[i,2]<-sum(S[i,7:20],L[i,7:20],R[i,7:20],I[i,7:20],NI[i,7:20],Sv[i,7:20],Lv[i,7:20],Rv[i,7:20])
      psizematrix[i,3]<-sum(S[i,21:65],L[i,21:65],R[i,21:65],I[i,21:65],NI[i,21:65],Sv[i,21:65],Lv[i,21:65],Rv[i,21:65])
      psizematrix[i,4]<-sum(S[i,66:Mnage],L[i,66:Mnage],R[i,66:Mnage],I[i,66:Mnage],NI[i,66:Mnage],Sv[i,66:Mnage],Lv[i,66:Mnage],Rv[i,66:Mnage])
      
      Imatrix[i,1]<-sum(I[i,1:6])
      Imatrix[i,2]<-sum(I[i,7:20])
      Imatrix[i,3]<-sum(I[i,21:65])
      Imatrix[i,4]<-sum(I[i,66:Mnage])
      
      print ("done psize start yr")
      
      ## number vaccinated
      #need to set up matrix for psizevacc to be recorded in to**  not needed as in econout???
      #psizevacc[i]<-sum(Sv[i,],Lv[i,],Rv[i,])
      #nmbvacc<-psizevacc[i]-psizevacc[i-1]
    
    
      ## Death markers
      # Number of TB deaths in HIV-, in HIV+, all form HIV deaths
      TBDeaths[i,]=dt*((ui)*I[i-1,]+(uni)*NI[i-1,]);
      #TBDeathsH[i,]=dt*(uiHA*IH[i-1,]+(uniHA)*NIH[i-1,]);
      #AllDeathsH[i,]=dt*((uH+uiHA)*IH[i-1,]+(uH+uniHA)*NIH[i-1,]);
      print ("done TB deaths")
      
      # Age deaths HIV-, HIV+
      ADeaths[i,]=dt*(u*S[i-1,]+u*L[i-1,]+(u+ui)*I[i-1,]+(u+uni)*NI[i-1,]+u*R[i-1,]+u*Sv[i-1,]+u*Lv[i-1,]+u*Rv[i-1,])
      #ADeathsH[i,]=dt*(uH*SH[i-1,]+uH*LH[i-1,]+(uH+uiHA)*IH[i-1,]+(uH+uniHA)*NIH[i-1,]+uH*RH[i-1,]+u*SvH[i-1,]+u*LvH[i-1,]+u*RvH[i-1,])
      print ("done Adeaths")
      
      # Deaths matrix holds all death indices
      # Columns: Number deaths HIV-, av. age death, number HIV+ deaths, av age HIV+ death, av. age death
      Deaths[i,1]=sum(ADeaths[i,]);   Deaths[i,2]=sum(ADeaths[i,]*seq(1:Mnage))/sum(ADeaths[i,])
      #Deaths[i,3]=sum(ADeathsH[i,]);  Deaths[i,4]=sum(ADeathsH[i,]*seq(1:Mnage))/sum(ADeathsH[i,])
      #Deaths[i,5]=sum((ADeathsH[i,]+ADeaths[i,])*seq(1:Mnage))/sum(ADeathsH[i,]+ADeaths[i,])
      print ("done deaths 1 2")
      
      ## NUMBER ON TREATMENT & NUMBER SUCCESSFULLY TREATED
      # TBRx columns: HIV- detected, successfully treated, HIV+ detected, successfully treated
      #TBRx[i,1]=CDR*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]));    TBRx[i,2]=CDR*(CoT)*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]))
      #TBRx[i,3]=CDRH*(sum(new_IH[i-1,])+e*sum(new_NIH[i-1,]));  TBRx[i,4]=CDRH*(CoTH)*(sum(new_IH[i-1,])+e*sum(new_NI[i-1,]))
      
      if (i==length(seq(year1,yearend,dt))){ # LAST TIME STEP
        # First is for whole population, second for HIV positives only... 
        Out<-cbind(Deaths,psize,rowSums(S),rowSums(L),rowSums(R),rowSums(I),rowSums(NI),rowSums(Sv),rowSums(Lv),rowSums(Rv),rowSums(thetaS),rowSums(thetaL),rowSums(thetaR),rowSums(d),VX)
        nms<-c("Deaths","Psz","S","L","R","I","NI","Sv","Lv","Rv","thetaS","thetaL","thetaR","d","VaccDTP3")
        Out<-as.data.frame(Out);colnames(Out)<-nms
        print ("done out and nms")
        
        #### FOR CE OUTPUT
        #yrcount<-seq(1,(yearend-year1)*(1/dt)+1,(1/dt))
        
        #print ("done year count")
        #EconOut<-matrix(0,length(yrcount)-1,9)
        #EconOut[,1]<-seq(year1,yearend-1)
        #nns<-c("Year","HIV-cases","HIV+cases","TBDeaths","AvAgeD","VaccDTP3","Vacc10","VaccMass","Treatments")
        #EconOut<-as.data.frame(EconOut); colnames(EconOut)<-nns
        
        #hbcOut<-matrix(0,length(yrcount)-1,6)
        #hbcOut[,1]<-seq(year1,yearend-1)
        #nns2<-c("Year","Psize","negcases","poscases","negdeaths","posdeaths")
        #hbcOut<-as.data.frame(hbcOut); colnames(hbcOut)<-nns2
        
        # Calculate that years average
        
       # for (i in 1:(length(yrcount)-1)){
          #gives first and last timestep of a year??? no as first timestep of year runs. so why need??? calcs for the last year
#           #i1<-yrcount[i]; i2<-yrcount[i+1]-1
#           #EconOut[i,"AvAgeD"]=sum(colSums(ADeathsH[i1:i2,]+ADeaths[i1:i2,])*seq(1:Mnage))/sum(ADeathsH[i1:i2,]+ADeaths[i1:i2,])
#           #EconOut[i,"TBDeaths"]=sum(TBDeaths[i1:i2,]+TBDeathsH[i1:i2,])
#           #EconOut[i,"HIV-cases"]=sum(new_I[i1:i2,]+new_NI[i1:i2,])
#          # EconOut[i,"HIV+cases"]=sum(new_IH[i1:i2,]+new_NIH[i1:i2,])
#           EconOut[i,"VaccDTP3"]=sum(VX[i1:i2,1])
#           EconOut[i,"Vacc10"]=sum(VX[i1:i2,2])
#           EconOut[i,"VaccMass"]=sum(VX[i1:i2,3])
#           EconOut[i,"Treatments"]=sum(TBRx[i1:i2,1]+TBRx[i1:i2,3])
#           
#           hbcOut[i,"Psize"] = mean(psize[i1:i2])
#           hbcOut[i,"negcases"] = sum(new_I[i1:i2,]+new_NI[i1:i2,])
#           #hbcOut[i,"poscases"] = sum(new_IH[i1:i2,]+new_NIH[i1:i2,])
#           hbcOut[i,"negdeaths"] =sum(TBDeaths[i1:i2,])
#           #hbcOut[i,"posdeaths"] = sum(TBDeathsH[i1:i2,])
        }
      #} 
    } ####•••••••••••••••••• END OF START OF YEAR RUNS
    
print("done start year")

    ####•••••••••••••••••• MIDDLE OF YEAR RUNS
    if (k < yearend){ 
      for (i in (2+(1/dt)*(k-year1)):((1/dt)*(k-year1)+1/dt)){
        start <- 0 # Not the start of the year
        #lambda[i-1] <- (1 - exp(-(neta) * z * ((sum(I[i-1,])/(psize[i-1])))))
        print("pre-post lambda")
        lambda[i-1,1:Mnage] <- t(neta * (1-exp(colSums(-(myneta[1:4,1:Mnage]) * z * ((Imatrix[i-1,1:4])/(psizematrix[i-1,1:4]))))))
        
        print("post-lambda")
        #print(lambda)
        ####•••••••••••••••••• TB model ••••••••••••••••••
        ## If the time step is not the first of the year 
        #save(S,file="S1.RData")
        S[i,1:Mnage] = S[i-1,1:Mnage] - (u[1:Mnage]+lambda[i-1,1:Mnage])*S[i-1,1:Mnage]*dt
        #print("1")
        #save(S,file="S.RData")
        L[i,1:Mnage] = L[i-1,1:Mnage] + lambda[i-1,1:Mnage]*(1 - p[1:Mnage])*(S[i-1,1:Mnage] + g*R[i-1,1:Mnage])*dt - (v[1:Mnage] + lambda[i-1,1:Mnage]*p[1:Mnage]*x + u[1:Mnage])*L[i-1,1:Mnage]*dt 
        #print("1")
        new_I_react[i,1:Mnage] = v[1:Mnage]*f[1:(Mnage)]*(L[i-1,1:(Mnage)])*dt + r[1:Mnage]*h[1:(Mnage)]*R[i-1,1:(Mnage)]*dt 
        #print("1")
        new_NI_react[i,1:Mnage] =  v[1:Mnage]*(1 - f[1:(Mnage)])*L[i-1,1:(Mnage)]*dt + r[1:Mnage]*(1 - h[1:(Mnage)])*R[i-1,1:(Mnage)]*dt  
        #print("1")
        new_I[i,1:Mnage] = lambda[i-1,1:Mnage]*p[1:(Mnage)]*f[1:(Mnage)]*(S[i-1,1:(Mnage)] + g*R[i-1,1:(Mnage)])*dt + (v[1:Mnage] + lambda[i-1,1:Mnage]*p[1:(Mnage)]*x)*f[1:(Mnage)]*L[i-1,1:(Mnage)]*dt + r[1:Mnage]*h[1:(Mnage)]*R[i-1,1:(Mnage)]*dt + w*NI[i-1,1:(Mnage)]*dt
        #print("1")
        new_NI[i,1:Mnage] = lambda[i-1,1:Mnage]*p[1:(Mnage)]*(1 - f[1:(Mnage)])*(S[i-1,1:(Mnage)] + g*R[i-1,1:(Mnage)])*dt + (v[1:Mnage] + lambda[i-1,1:Mnage]*p[1:(Mnage)]*x)*(1 - f[1:(Mnage)])*L[i-1,1:(Mnage)]*dt + r[1:Mnage]*(1 - h[1:(Mnage)])*R[i-1,1:(Mnage)]*dt  
        #print("1")
        R[i,1:Mnage] = R[i-1,1:(Mnage)] + n[1:Mnage]*(I[i-1,1:(Mnage)] + NI[i-1,1:(Mnage)])*dt + CDR[1:Mnage]*CoT*(new_I[i,1:Mnage] + e*new_NI[i,1:Mnage]) - (r[1:Mnage] + g*lambda[i-1,1:Mnage] + u[1:(Mnage)])*R[i-1,1:(Mnage)]*dt 
        #print("1")
        I[i,1:Mnage] = I[i-1,1:(Mnage)] + (1 - CDR[1:Mnage]*CoT)*(new_I[i,1:Mnage]) - (n[1:Mnage] + u[1:(Mnage)] + ui[1:Mnage])*I[i-1,1:(Mnage)]*dt
        #print("1")
        NI[i,1:Mnage] = NI[i-1,1:(Mnage)] + (1 - CDR[1:Mnage]*CoT)*(e*new_NI[i,1:Mnage]) - (n[1:Mnage] + u[1:(Mnage)] + uni[1:Mnage] + w)*NI[i-1,1:(Mnage)]*dt                    
        
        print("done nonvacc")                        
        
        ####•••••••••••••••••••• TB HIV model •••••••••••••••••
        #SH[i,15:Mnage] = SH[i-1,15:Mnage] - (uH[15:Mnage] + lambda[i-1,1:Mnage])*SH[i-1,15:Mnage]*dt + hiv[15:Mnage]*S[i-1,15:Mnage]*dt 
        #LH[i,15:Mnage] = LH[i-1,15:Mnage] + lambda[i-1,1:Mnage]*(1 - pHA)*(SH[i-1,15:Mnage] + gHA*RH[i-1,15:Mnage])*dt - (vHA + lambda[i-1,1:Mnage]*pHA*xHA + uH[15:Mnage])*LH[i-1,15:Mnage]*dt + hiv[15:Mnage]*L[i-1,15:Mnage]*dt 
        
        #new_IH[i,15:Mnage] = lambda[i-1,1:Mnage]*pHA*fH*(SH[i-1,15:Mnage] + gHA*RH[i-1,15:Mnage])*dt + (vHA + lambda[i-1,1:Mnage]*pHA*xHA)*fH*LH[i-1,15:Mnage]*dt + rHA*hH*RH[i-1,15:Mnage]*dt + w*NIH[i-1,15:Mnage]*dt
        #new_NIH[i,15:Mnage] = lambda[i-1,1:Mnage]*pHA*(1 - fH)*(SH[i-1,15:Mnage] + gHA*RH[i-1,15:Mnage])*dt + (vHA + lambda[i-1,1:Mnage]*pHA*xHA)*(1 - fH)*LH[i-1,15:Mnage]*dt + rHA*(1 - hH)*RH[i-1,15:Mnage]*dt  
        
        #RH[i,15:Mnage] = RH[i-1,15:Mnage] + nH*(IH[i-1,15:Mnage] + NIH[i-1,15:Mnage])*dt + CDRH*CoTH*(new_IH[i-1,15:Mnage] + e*new_NIH[i-1,15:Mnage]) - (rHA + gHA*lambda[i-1,1:Mnage] + uH[15:Mnage])*RH[i-1,15:Mnage]*dt + hiv[15:Mnage]*R[i-1,15:Mnage]*dt
        #IH[i,15:Mnage] = IH[i-1,15:Mnage] + (1 - CDRH*CoTH)*(new_IH[i-1,15:Mnage]) - (nH + uH[15:Mnage] + uiHA)*IH[i-1,15:Mnage]*dt + hiv[15:Mnage]*I[i-1,15:Mnage]*dt
        #NIH[i,15:Mnage] = NIH[i-1,15:Mnage] + (1 - CDRH*CoTH)*(e*new_NIH[i-1,15:Mnage]) - (nH + uH[15:Mnage] + uniHA + w)*NIH[i-1,15:Mnage]*dt + hiv[15:Mnage]*NI[i-1,15:Mnage]*dt                
        
        #### •••••••••••••••••••• VACCINE AGING •••••••••••••••••••••••••
        
        Sv[i,1:Mnage] = Sv[i-1,1:Mnage] - (u[1:Mnage])*Sv[i-1,1:Mnage]*dt - lambda[i-1,1:Mnage]*(Sv[i-1,1:Mnage])*dt 
        Lv[i,1:Mnage] = Lv[i-1,1:Mnage] - (u[1:Mnage])*Lv[i-1,1:Mnage]*dt + lambda[i-1,1:Mnage]*(Sv[i-1,1:Mnage] + g*Rv[i-1,1:Mnage])*dt 
        Rv[i,1:Mnage] = Rv[i-1,1:Mnage] - (u[1:Mnage])*Rv[i-1,1:Mnage]*dt - lambda[i-1,1:Mnage]*(g*Rv[i-1,1:Mnage])*dt
        
        #SvH[i,15:Mnage] = SvH[i-1,15:Mnage] + hiv[15:Mnage]*Sv[i-1,15:Mnage]*dt - (uH[15:Mnage])*SvH[i-1,15:Mnage]*dt - lambda[i-1,1:Mnage]*(SvH[i-1,15:Mnage])*dt 
        #LvH[i,15:Mnage] = LvH[i-1,15:Mnage] + hiv[15:Mnage]*Lv[i-1,15:Mnage]*dt - (uH[15:Mnage])*LvH[i-1,15:Mnage]*dt + lambda[i-1,1:Mnage]*(SvH[i-1,15:Mnage] + gHA*RvH[i-1,15:Mnage])*dt 
        #RvH[i,15:Mnage] = RvH[i-1,15:Mnage] + hiv[15:Mnage]*Rv[i-1,15:Mnage]*dt - (uH[15:Mnage])*RvH[i-1,15:Mnage]*dt - lambda[i-1,1:Mnage]*(gHA*RvH[i-1,15:Mnage])*dt 
        print("done vacc")
        ###•••••••••••••••••• Vaccine coverage and duration ••••••••••••••••
        # NUMBER OF VACCINES
        if(vaccine == 0){VX[i,1:3]<-0}
        #check what VX does - might want to include l and r in eqn as vaccine will be delivered, it just wont work,
        if(vaccine == 2){VX[i,2]<-sum(thetaV2a[i,]*(S[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]))}
        if(vaccine == 3){VX[i,2]<-sum(thetaV2a[i,]*(S[i,]+L[i,]+R[i,])); VX[i,3]<-sum(thetaV2m[i,]*(S[i,]+L[i,]+R[i,]))}
        print("done vacc assignment")
        ####•••••••••••••••••• Vaccination and Removal of protection
        
        S2 = S[i,] + Sv[i,]*(d[i,]*(1-thetaS[i,])) - thetaS[i,]*S[i,]  #should this theta be k???
        L2 = L[i,] + Lv[i,]*(d[i,]*(1-thetaL[i,])) - thetaL[i,]*L[i,]
        R2 = R[i,] + Rv[i,]*(d[i,]*(1-thetaR[i,])) - thetaR[i,]*R[i,]
        
        #SH2 = SH[i,] + SvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*SH[i,]
        #LH2 = LH[i,] + LvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*LH[i,]
        #RH2 = RH[i,] + RvH[i,]*(d[i,]*(1-theta[i,])) - thetaH[i,]*RH[i,]
        
        Sv2 = Sv[i,] - Sv[i,]*(d[i,]*(1-thetaS[i,])) + thetaS[i,]*S[i,]
        Lv2 = Lv[i,] - Lv[i,]*(d[i,]*(1-thetaL[i,])) + thetaL[i,]*L[i,]
        Rv2 = Rv[i,] - Rv[i,]*(d[i,]*(1-thetaR[i,])) + thetaR[i,]*R[i,]
        
        #SvH2 = SvH[i,] - SvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*SH[i,]
        #LvH2 = LvH[i,] - LvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*LH[i,]
        #RvH2 = RvH[i,] - RvH[i,]*(d[i,]*(1-thetaH[i,])) + thetaH[i,]*RH[i,]
        
        S[i,]<-S2;L[i,]<-L2;R[i,]<-R2;       #SH[i,]<-SH2;LH[i,]<-LH2;RH[i,]<-RH2;
        Sv[i,]<-Sv2;Lv[i,]<-Lv2;Rv[i,]<-Rv2; #SvH[i,]<-SvH2;LvH[i,]<-LvH2;RvH[i,]<-RvH2;
        
        print("end of eqns")
        ####•••••••••••••••••• Economic Output ••••••••••••••••••
        #### Output for cost-effectiveness 
        ## POPULATION SIZE 
        psize[i]<-sum(S[i,],L[i,],R[i,],I[i,],NI[i,],Sv[i,],Lv[i,],Rv[i,])
        #print(c("PSIZE",i,psize[i]))
        
        #ages needed to fit to incidence and population size
        psize014[i]<-sum(S[i,1:15],L[i,1:15],R[i,1:15],I[i,1:15],NI[i,1:15],Sv[i,1:15],Lv[i,1:15],Rv[i,1:15])
        psize1554[i]<-sum(S[i,16:55],L[i,16:55],R[i,16:55],I[i,16:55],NI[i,16:55],Sv[i,16:55],Lv[i,16:55],Rv[i,16:55])
        psize5564[i]<-sum(S[i,56:65],L[i,56:65],R[i,56:65],I[i,56:65],NI[i,56:65],Sv[i,56:65],Lv[i,56:65],Rv[i,56:65])
        psize65plus[i]<-sum(S[i,66:Mnage],L[i,66:Mnage],R[i,66:Mnage],I[i,66:Mnage],NI[i,66:Mnage],Sv[i,66:Mnage],Lv[i,66:Mnage],Rv[i,66:Mnage])
        #inset check of pop size
        #ages needed to fit to mort  and prevalence as have different groupings
        psize1559[i]<-sum(S[i,16:60],L[i,16:60],R[i,16:60],I[i,16:60],NI[i,16:60],Sv[i,16:60],Lv[i,16:60],Rv[i,16:60])
        psize1529[i]<-sum(S[i,16:30],L[i,16:30],R[i,16:30],I[i,16:30],NI[i,16:30],Sv[i,16:30],Lv[i,16:30],Rv[i,16:30])
        psize3044[i]<-sum(S[i,31:45],L[i,31:45],R[i,31:45],I[i,31:45],NI[i,31:45],Sv[i,31:45],Lv[i,31:45],Rv[i,31:45])
        psize4559[i]<-sum(S[i,46:60],L[i,46:60],R[i,46:60],I[i,46:60],NI[i,46:60],Sv[i,46:60],Lv[i,46:60],Rv[i,46:60])
        psize60plus[i]<-sum(S[i,61:Mnage],L[i,61:Mnage],R[i,61:Mnage],I[i,61:Mnage],NI[i,61:Mnage],Sv[i,61:Mnage],Lv[i,61:Mnage],Rv[i,61:Mnage])
        print("psize pre-matrix")
        #ages and incidence for contact matrices
        psizematrix[i,1]<-sum(S[i,1:6],L[i,1:6],R[i,1:6],I[i,1:6],NI[i,1:6],Sv[i,1:6],Lv[i,1:6],Rv[i,1:6])
        psizematrix[i,2]<-sum(S[i,7:20],L[i,7:20],R[i,7:20],I[i,7:20],NI[i,7:20],Sv[i,7:20],Lv[i,7:20],Rv[i,7:20])
        psizematrix[i,3]<-sum(S[i,21:65],L[i,21:65],R[i,21:65],I[i,21:65],NI[i,21:65],Sv[i,21:65],Lv[i,21:65],Rv[i,21:65])
        psizematrix[i,4]<-sum(S[i,66:Mnage],L[i,66:Mnage],R[i,66:Mnage],I[i,66:Mnage],NI[i,66:Mnage],Sv[i,66:Mnage],Lv[i,66:Mnage],Rv[i,66:Mnage])
        print("psize post-matrix")
        Imatrix[i,1]<-sum(I[i,1:6])
        Imatrix[i,2]<-sum(I[i,7:20])
        Imatrix[i,3]<-sum(I[i,21:65])
        Imatrix[i,4]<-sum(I[i,66:Mnage])
        print("imatrix post-matrix")
        
        print("done pop")
        
        ## number vaccinated
        #need to set up matrix for psizevacc to be recorded in to**
        #psizevacc[i]<-sum(Sv[i,],Lv[i,],Rv[i,])
        #nmbvacc<-psizevacc[i]-psizevacc[i-1]
        
        
        ## Death markers
        # Number of TB deaths
        TBDeaths[i,]=dt*((ui)*I[i-1,]+(uni)*NI[i-1,]);
        
        # All deaths TB and background ##add in by age???
        ADeaths[i,]=dt*(u*S[i-1,]+u*L[i-1,]+(u+ui)*I[i-1,]+(u+uni)*NI[i-1,]+u*R[i-1,]+u*Sv[i-1,]+u*Lv[i-1,]+u*Rv[i-1,])

        # Deaths matrix holds all death indices
        # Columns: Number deaths HIV-, av age at death av. age HIV death, number HIV+ deaths, av age HIV+ death, av. age death
        Deaths[i,1]=sum(ADeaths[i,]);   Deaths[i,2]=sum(ADeaths[i,]*seq(1:Mnage))/sum(ADeaths[i,])
        #Deaths[i,3]=sum(ADeathsH[i,]);  Deaths[i,4]=sum(ADeathsH[i,]*seq(1:Mnage))/sum(ADeathsH[i,])
        #Deaths[i,5]=sum((ADeathsH[i,]+ADeaths[i,])*seq(1:Mnage))/sum(ADeathsH[i,]+ADeaths[i,])
        
        ## NUMBER ON TREATMENT & NUMBER SUCCESSFULLY TREATED
        # TBRx columns: HIV- detected, successfully treated, HIV+ detected, successfully treated
        #TBRx[i,1]=CDR*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]));    TBRx[i,2]=CDR*(CoT)*(sum(new_I[i-1,])+e*sum(new_NI[i-1,]))
        #TBRx[i,3]=CDRH*(sum(new_IH[i-1,])+e*sum(new_NIH[i-1,]));  TBRx[i,4]=CDRH*(CoTH)*(sum(new_IH[i-1,])+e*sum(new_NI[i-1,]))
        
        # If last timestep of the year
        
        if(i == ((1/dt)*(k-year1)+1/dt)){
          
          print("last time step")
          
          i1<-((1/dt)*(k-year1)+1); i2<-((1/dt)*(k-year1)+1/dt)
          # TB INCIDENCE AND MORTALITY etc for MODEL FITTING and RESEARCH OUTCOMES
          #print(c(i,TBI[1,],sum(new_I[i1:i2,],new_NI[i1:i2,]),mean(psize[i1:i2])))
          # Yearly average PSIZE, Incidence and mortality
          ## (1) population size 
          PSIZEy[(k-year1+1),1]<-mean(psize[i1:i2]);
          PSIZEy[(k-year1+1),2]<-mean(psize014[i1:i2])
          PSIZEy[(k-year1+1),3]<-mean(psize1554[i1:i2])
          PSIZEy[(k-year1+1),4]<-mean(psize5564[i1:i2])
          PSIZEy[(k-year1+1),5]<-mean(psize65plus[i1:i2])
          PSIZEy[(k-year1+1),6]<-mean(psize1559[i1:i2])
          PSIZEy[(k-year1+1),7]<-mean(psize1529[i1:i2])
          PSIZEy[(k-year1+1),8]<-mean(psize3044[i1:i2])
          PSIZEy[(k-year1+1),9]<-mean(psize4559[i1:i2])
          PSIZEy[(k-year1+1),10]<-mean(psize60plus[i1:i2])
          print("k2")
          
          ## (2) TB incidence rate
          TBI[(k-year1+1),1]<-100000*sum(new_I[i1:i2,],new_NI[i1:i2,])/mean(psize[i1:i2])
          TBI[(k-year1+1),2]<-100000*sum(new_I[i1:i2,1:15],new_NI[i1:i2,1:15])/mean(psize014[i1:i2])
          TBI[(k-year1+1),3]<-100000*sum(new_I[i1:i2,16:55],new_NI[i1:i2,16:55])/mean(psize1554[i1:i2])
          TBI[(k-year1+1),4]<-100000*sum(new_I[i1:i2,56:65],new_NI[i1:i2,56:65])/mean(psize5564[i1:i2])
          TBI[(k-year1+1),5]<-100000*sum(new_I[i1:i2,66:Mnage],new_NI[i1:i2,66:Mnage])/mean(psize65plus[i1:i2])
          
          ## (3) TB prevalence rate 
          TBP[(k-year1+1),1]<-100000*sum(I[i1:i2,],NI[i1:i2,])/mean(psize[i1:i2])
          TBP[(k-year1+1),2]<-100000*sum(I[i1:i2,1:15],NI[i1:i2,1:15])/mean(psize014[i1:i2])
          TBP[(k-year1+1),3]<-100000*sum(I[i1:i2,16:30],NI[i1:i2,16:30])/mean(psize1529[i1:i2])
          TBP[(k-year1+1),4]<-100000*sum(I[i1:i2,31:45],NI[i1:i2,31:45])/mean(psize3044[i1:i2])
          TBP[(k-year1+1),5]<-100000*sum(I[i1:i2,46:60],NI[i1:i2,46:60])/mean(psize4559[i1:i2])
          TBP[(k-year1+1),6]<-100000*sum(I[i1:i2,61:Mnage],NI[i1:i2,61:Mnage])/mean(psize60plus[i1:i2])
          
          ## (4) TB bacteriologically positive prevalence rate 
          TBPb[(k-year1+1),1]<-100000*sum(I[i1:i2,])/mean(psize[i1:i2])
          TBPb[(k-year1+1),2]<-100000*sum(I[i1:i2,1:15])/mean(psize014[i1:i2])
          TBPb[(k-year1+1),3]<-100000*sum(I[i1:i2,16:30])/mean(psize1529[i1:i2])
          TBPb[(k-year1+1),4]<-100000*sum(I[i1:i2,31:45])/mean(psize3044[i1:i2])
          TBPb[(k-year1+1),5]<-100000*sum(I[i1:i2,46:60])/mean(psize4559[i1:i2])
          TBPb[(k-year1+1),6]<-100000*sum(I[i1:i2,61:Mnage])/mean(psize60plus[i1:i2])
          
          
          ## (5) TB mortality
          #print(c("IH",sum(new_IH[i,],new_NIH[i,]),TBI[i,2]))
          TBM[(k-year1+1),1]<-100000*sum(TBDeaths[i1:i2,])/mean(psize[i1:i2])
          TBM[(k-year1+1),2]<-100000*sum(TBDeaths[i1:i2,1:15])/mean(psize014[i1:i2])
          TBM[(k-year1+1),3]<-100000*sum(TBDeaths[i1:i2,16:55])/mean(psize1554[i1:i2])
          TBM[(k-year1+1),4]<-100000*sum(TBDeaths[i1:i2,56:65])/mean(psize5564[i1:i2])
          TBM[(k-year1+1),5]<-100000*sum(TBDeaths[i1:i2,66:Mnage])/mean(psize65plus[i1:i2])
          TBM[(k-year1+1),6]<-100000*sum(TBDeaths[i1:i2,16:60])/mean(psize1559[i1:i2])
          TBM[(k-year1+1),7]<-100000*sum(TBDeaths[i1:i2,61:Mnage])/mean(psize60plus[i1:i2])
          
          ## (5) Prevalence of infection - in case get data to fit to. WHAT ABOUT RECOVERDS???
          TBPI[(k-year1+1),1]<-100*(((sum(L[i1:i2,]))/2)/mean(psize[i1:i2]))
          TBPI[(k-year1+1),2]<-100*(((sum(L[i1:i2,1:15]))/2)/mean(psize014[i1:i2]))
          TBPI[(k-year1+1),3]<-100*(((sum(L[i1:i2,16:55]))/2)/mean(psize1554[i1:i2]))
          TBPI[(k-year1+1),4]<-100*(((sum(L[i1:i2,56:65]))/2)/mean(psize5564[i1:i2]))
          TBPI[(k-year1+1),5]<-100*(((sum(L[i1:i2,66:Mnage]))/2)/mean(psize65plus[i1:i2]))
          
          #### FOR ADDITIONAL RESEARCH OUTCOMES
          
          # % of transmission due to the elderly
          ##worried this wont sum across the right things, as want to sum across the i's cycled through the j's??? play with mock vectors. suceptibles on top and bottom? terms form eqns Do I need to loop one of them?
          #haven't updated lambda
          #eldtrans[(k-year1+1)]<- 100* (sum(lambda[i1:i2,66:Mnage] * I[i1:i2,66:Mnage])/(i1-i2)) / (sum(lambda[i1:i2,] * I[i1:i2,])/(i1-i2))
          
        }
        ####••••••••••••••••• END OF MIDDLE YEAR RUNS
      }
    }
  print("done mid year")
  } 

  ## Outputs - in R, allows output to be seen without expressly wanting it - what is this??? comment out? 
  assign('S',S,envir = .GlobalEnv);assign('L',L,envir = .GlobalEnv);assign('I',I,envir = .GlobalEnv);assign('NI',NI,envir = .GlobalEnv);assign('R',R,envir = .GlobalEnv);assign('new_I',new_I,envir = .GlobalEnv);assign('new_NI',new_NI,envir = .GlobalEnv)
  assign('NBirths',BIRTHS,envir=.GlobalEnv)  
#assign('SH',SH,envir = .GlobalEnv);assign('LH',LH,envir = .GlobalEnv);#assign('IH',IH,envir = .GlobalEnv);assign('NIH',NIH,envir = .GlobalEnv);assign('RH',RH,envir = .GlobalEnv);assign('new_IH',new_IH,envir = .GlobalEnv);assign('new_NIH',new_NIH,envir = .GlobalEnv)
  assign('Sv',Sv,envir = .GlobalEnv);assign('Lv',Lv,envir = .GlobalEnv);assign('Rv',Rv,envir = .GlobalEnv);#assign('SvH',SvH,envir = .GlobalEnv);assign('LvH',LvH,envir = .GlobalEnv);assign('RvH',RvH,envir = .GlobalEnv);
  assign('lambda',lambda,envir=.GlobalEnv);assign('thetaS',thetaS,envir=.GlobalEnv);assign('thetaL',thetaL,envir=.GlobalEnv);assign('thetaR',thetaR,envir=.GlobalEnv);
assign('d',d,envir=.GlobalEnv);
  assign('TBDeaths',TBDeaths,envir=.GlobalEnv);#assign('TBDeathsH',TBDeathsH,envir=.GlobalEnv);
  #assign('AllDeathsH',AllDeathsH,envir=.GlobalEnv);
  assign('ADeaths',ADeaths,envir=.GlobalEnv);#assign('ADeathsH',ADeathsH,envir=.GlobalEnv);
  assign('Deaths',Deaths,envir=.GlobalEnv);assign('u',u,envir=.GlobalEnv);#assign('hiv',hiv,envir=.GlobalEnv);
  assign('psize',psize,envir=.GlobalEnv);assign('bv',bv,envir=.GlobalEnv);#assign('prevHIV',prevHIV,envir=.GlobalEnv);assign('prevHIV1549',prevHIV1549,envir=.GlobalEnv);
  assign('psize014',psize014,envir=.GlobalEnv)
  assign('psize1529',psize1529,envir=.GlobalEnv)
  assign('psize1554',psize1554,envir=.GlobalEnv)
  assign('psize1559',psize1559,envir=.GlobalEnv)
  assign('psize3044',psize3044,envir=.GlobalEnv)
  assign('psize4559',psize4559,envir=.GlobalEnv)
  assign('psize5564',psize5564,envir=.GlobalEnv)
  assign('psize60plus',psize60plus,envir=.GlobalEnv)
  assign('psize65plus',psize65plus,envir=.GlobalEnv)
  assign('TBI',TBI,envir=.GlobalEnv);assign('TBM',TBM,envir=.GlobalEnv);assign('TBRx',TBRx,envir=.GlobalEnv);assign('VX',VX,envir=.GlobalEnv);
  assign('TBP',TBP,envir=.GlobalEnv);assign('TBPb',TBPb,envir=.GlobalEnv)
  assign('TBPI',TBPI,envir=.GlobalEnv);assign('PSIZEy',PSIZEy,envir=.GlobalEnv);
  #assign('Econout',EconOut,envir=.GlobalEnv);
  assign('Out',Out,envir=.GlobalEnv);
  #assign('hbcout',hbcOut,envir=.GlobalEnv);
  
print("done assign")

  ## CUMULATIVE RESEARCH OUTCOMES
  
  totmort[,1]<- sum(TBM[,1])
  totmort[,2]<- sum(TBM[,2])
  totmort[,3]<- sum(TBM[,3])
  totmort[,4]<- sum(TBM[,4])
  totmort[,5]<- sum(TBM[,5])
  totmort[,6]<- sum(TBM[,6])
  totmort[,7]<- sum(TBM[,7])
  
  totcase[,1]<- sum(TBI[,1])
  totcase[,2]<- sum(TBI[,2])
  totcase[,3]<- sum(TBI[,3])
  totcase[,4]<- sum(TBI[,4])
  totcase[,5]<- sum(TBI[,5])
  
  print("done tot case")
  colnames(TBI)<-c("All ages","0-14", "15-54", "55-64", "65+")
  colnames(TBM)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "60+")
  colnames(TBP)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+")
  colnames(TBPb)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+")
  colnames(TBPI)<-c("All ages","0-14", "15-54", "55-64", "65+")
  colnames(PSIZEy)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "15-29", "30-44", "45-59", "60+")
  
  ## Actual Output required (collected as progressed with model)
  # need to update this. what does Ana do???
  X<-cbind(psize,rowSums(S),S[,1],rowSums(I),rowSums(NI),rowSums(L),rowSums(R),rowSums(new_I),rowSums(new_NI),
           rowSums(new_I_react),rowSums(new_NI_react), 
           rowSums(Sv),rowSums(Lv),rowSums(Rv),
           TBI[,1],TBI[,2], TBI[,3], TBI[,4], TBI[,5], TBM[,1],TBM[,2],TBM[,3], TBM[,4], TBM[,5], TBM[,6], TBM[,7],TBP[,1],TBP[,2],TBP[,3], TBP[,4], TBP[,5], TBP[,6],TBPb[,1],TBPb[,2],TBPb[,3], TBPb[,4], TBPb[,5], TBP[,6],TBPI[,1],TBPI[,2],TBPI[,3], TBPI[,4], TBPI[,5], PSIZEy[,1],PSIZEy[,2], PSIZEy[,3], PSIZEy[,4], PSIZEy[,5], PSIZEy[,6], PSIZEy[,7], PSIZEy[,8], PSIZEy[,9], PSIZEy[,10])
  colnames(X)<-c("PSIZE","S","Births","I","NI","L","R","new_I","new_NI","new_I_react","new_NI_react", "Sv","Lv","Rv",
                 "TBItot","TBI0-14","TBI15-54","TBI55-64","TBI65+","TBMtot","TBM0-14", "TBM15-54", "TBM55-64", "TBM65+", "TBM15-59", "TBM60+","TBPtot","TBP0-14", "TBP15-29", "TBP30-44", "TBP45-59", "TBP60+","TBPbtot","TBPb0-14", "TBPb15-29", "TBPb30-44", "TBPb45-59", "TBP60+", "TBPItot","TBPI0-14", "TBPI15-54", "TBPI55-64", "TBPI65+", "YearPsizetot", "YearPsize0-14", "YearPsize15-54", "YearPsize55-64", "YearPsize65+", "YearPsize15-59", "YearPsize15-29", "YearPsize30-44", "YearPsize45-59", "YearPsize60+")
  print("X")
#X<-data.frame(X)
  # To show
  # reads in what fitting to???
#   Ana<-matrix(0,4,6);Ana[,1:2]<-Popsize[1:2,cntry];Ana[,3]<-as.numeric(unlist(TBIm[cntry]));Ana[,4]<-as.numeric(TBMm[cntry]);Ana[,5]<-as.numeric(TBIHIVm[cntry]);Ana[,6]<-as.numeric(TBMHIVm[cntry]);
#   show<-c("FIT",round(Fit,2),"TBI/M/psz 2010/psz 2050/TBIH/TBMH",round(TBI[2010-year1+1,1],2),round(TBM[2010-year1+1,1],2),round(psize[(2010-year1)*(1/dt)+1],2),round(psize[(2050-year1)*(1/dt)+1],2),round(TBI[2010-year1+1,2],2),round(TBM[2010-year1+1,2],2),"Data",round(Ana,2),"prop",round(prop,2))
#   #print(show)
  
  # Record of every run
  #if (C==0){setwd(paste("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel/fitout/",cntry,"/",sep=''))
  #          write.csv(t(show),paste(date(),'.csv',sep=''))
  #          setwd("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel")}
  #if (C==1){setwd(paste("/users/eidegkni/Documents/vaccine/fit/fitout/",cntry,"/",sep=''))
  #          write.csv(t(show),paste(date(),'.csv',sep=''))
  #          setwd("/users/eidegkni/Documents/vaccine")}
  
  # If want to see plots 
  #if (Plot==1){source("#Plot.R")}
  
  # What outputting
  return(X)
}
