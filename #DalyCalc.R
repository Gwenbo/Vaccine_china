## DALY CALCULATIONS
# Function to do DALY calc and overarching function to do Simulation
# Inputs: country, vx price, camp price, Rx cost, MDR cost, proportion MDR, proportion MDR treated, Epi
# Returns: {Cases/Deaths/DALYS/discounted DALYS/Number Rx averted, Netcosts, discounted net costs, 
# cost per death averted, cost per DALY averted, discounted cost per death av. and discounted cost per DALY av.}

# Before hand grab Life Expectancy data and set input (C=0 implies laptop, C=1 implies cluster)
C=0
if (C == 0){setwd("/Users/londonschool/Documents/My\ Documents/Vaccine/CEmodel/")}
if (C == 1){setwd("/users/eidegkni/Documents/vaccine")}

# Takes in final values for everything (all things varied)
# Varied = vaccine price pvxd (dtp3,10,mass vx data), campaign price pC (routine, school, mass, data), 
# treatment costs (pDots,pMDR), MDR proportion (MDRprop, mdrproptr)
# X = epi values. Has columns where .x indicates with no vaccine, .y with vaccine
# (Year  HIV-cases.x	HIV+cases.x	TBDeaths.x	AvAgeD.x	Treatments.x	HIV-cases.y	HIV+cases.y	TBDeaths.y	AvAgeD.y	VaccDTP3.y	Vacc10.y	VaccMass.y	Treatments.ys)

# Inputs: country, vx price, camp price, Rx cost, MDR cost, proportion MDR, proportion MDR treated, Epi
# .x indicates with no vaccine, .y with vaccine, neg=HIV-, pos=HIV+
DALY<-function(cntry,pvxd,pc,pdots,pmdr,mdrprop,mdrproptr,X){
  # Constant CE parameters
  dr=0.03;
  L=0.4109589;
  d_hivneg=0.331;
  d_hivpos=0.399;
  K=0;
  B=0.04;
  C=0.1658;
  a=35;
  vw=0.05;
  # Life expectancy
  le=LifeE[,cntry]
  # Vaccine dose price - includes wastage and delivery cost into one value
  pvx<-c()
  for (i in 1:3){
    pvx[i]<-2*pvxd[i]*(1/(1-vw))+pc[i]
  }
  
  # X manipulations: Don't want per 1000, but actual values 
  X[,"TBDeaths.x"]=X[,"TBDeaths.x"]*1000; X[,"TBDeaths.y"]=X[,"TBDeaths.y"]*1000
  X[,"HIVnegcases.x"]=X[,"HIVnegcases.x"]*1000; X[,"HIVnegcases.y"]=X[,"HIVnegcases.y"]*1000
  X[,"HIVposcases.x"]=X[,"HIVposcases.x"]*1000; X[,"HIVposcases.y"]=X[,"HIVposcases.y"]*1000
  X[,"Treatments.x"]=X[,"Treatments.x"]*1000; X[,"Treatments.y"]=X[,"Treatments.y"]*1000;
  
  # Discounting (prefix of disc indicates this throughout)
  discTBdeaths.x=X[,"TBDeaths.x"]/(1+dr)^(X[,"Year"]-2024); discTBdeaths.y=X[,"TBDeaths.y"]/(1+dr)^(X[,"Year"]-2024)
  discTBhivneg.x=X[,"HIVnegcases.x"]/(1+dr)^(X[,"Year"]-2024);discTBhivneg.y=X[,"HIVnegcases.y"]/(1+dr)^(X[,"Year"]-2024);
  discTBhivpos.x=X[,"HIVposcases.x"]/(1+dr)^(X[,"Year"]-2024);discTBhivpos.y=X[,"HIVposcases.y"]/(1+dr)^(X[,"Year"]-2024);
  
  # Years left from age die
  leatD.x=le-X[,"AvAgeD.x"]
  leatD.y=le-X[,"AvAgeD.y"]
  
  # **********************************  YLL 
  YLL.x     <- K*(C*exp(-B*X[,"AvAgeD.x"])/B^2)*(exp(-B*leatD.x))*{-B*(X[,"AvAgeD.x"]+leatD.x)-1-(-B*X[,"AvAgeD.x"]-1)} + (1 + K) * leatD.x
  YLL.y     <- K*(C*exp(-B*X[,"AvAgeD.y"])/B^2)*(exp(-B*leatD.y))*{-B*(X[,"AvAgeD.y"]+leatD.y)-1-(-B*X[,"AvAgeD.y"]-1)} + (1 + K) * leatD.y
  TotalYLL.x <- YLL.x * X[,"TBDeaths.x"]
  TotalYLL.y <- YLL.y * X[,"TBDeaths.y"]
  
  discYLL.x <- K*C*exp(dr*X[,"AvAgeD.x"])/((dr+B)^2)*(exp(-(dr+B)*(leatD.x+X[,"AvAgeD.x"]))*{-(dr+B)*(leatD.x+X[,"AvAgeD.x"])-1}-exp(-(dr+B)*X[,"AvAgeD.x"])*{-(dr+B)*X[,"AvAgeD.x"]-1}) + ((1-K)/dr)*(1-exp(-dr*leatD.x))
  discYLL.y <- K*C*exp(dr*X[,"AvAgeD.y"])/((dr+B)^2)*(exp(-(dr+B)*(leatD.y+X[,"AvAgeD.y"]))*{-(dr+B)*(leatD.y+X[,"AvAgeD.y"])-1}-exp(-(dr+B)*X[,"AvAgeD.y"])*{-(dr+B)*X[,"AvAgeD.y"]-1}) + ((1-K)/dr)*(1-exp(-dr*leatD.y))
  discTotalYLL.x = discYLL.x * discTBdeaths.x
  discTotalYLL.y = discYLL.y * discTBdeaths.y
  
  # **********************************  YLD
  bb<-(K*C*exp(-B*a)/(B^2)*(exp(-B*L)*(-B*(L+a)-1)-(-B*a-1))+(1+K)*L)
  YLDnegpp <- d_hivneg * bb
  YLDpospp <- d_hivpos * bb
  
  YLDneg.x <- YLDnegpp * X[,"HIVnegcases.x"]; YLDneg.y <- YLDnegpp * X[,"HIVnegcases.y"]
  YLDpos.x <- YLDpospp * X[,"HIVposcases.x"]; YLDpos.y <- YLDpospp * X[,"HIVposcases.y"]
  
  discbb<-(K*C*exp(dr*a)/((dr+B)^2)*(exp(-(dr+B)*(L+a))*{-(dr+B)*(L+a)-1}-exp(-(dr+B)*a)*{-(dr+B)*a-1}) + ((1-K)/dr)*(1-exp(-dr*L)))
  discYLDnegpp <- d_hivneg*discbb; discYLDpospp <- d_hivpos*discbb
  
  discYLDneg.x <- discYLDnegpp * discTBhivneg.x; discYLDneg.y <- discYLDnegpp * discTBhivneg.y
  discYLDpos.x <- discYLDpospp * discTBhivpos.x; discYLDpos.y <- discYLDpospp * discTBhivpos.y
  
  TotalYLD.x <- YLDneg.x + YLDpos.x; TotalYLD.y <- YLDneg.y + YLDpos.y
  
  discTotalYLD.x <- discYLDneg.x + discYLDpos.x; discTotalYLD.y <- discYLDneg.y + discYLDpos.y
  
  # **********************************  DALYS
  TotalDALYS.x <- TotalYLD.x + TotalYLL.x; TotalDALYS.y <- TotalYLD.y + TotalYLL.y 
  discTotalDALYS.x <- discTotalYLD.x + discTotalYLL.x 
  discTotalDALYS.y <- discTotalYLD.y + discTotalYLL.y 
  
  # **********************************  Treatment costs
  Dotscost.x<-X[,"Treatments.x"]*pdots
  MDRcost.x<-mdrprop*X[,"Treatments.x"]*pmdr*mdrproptr
  Dotscost.y<-X[,"Treatments.y"]*pdots
  MDRcost.y<-mdrprop*X[,"Treatments.y"]*pmdr*mdrproptr
  
  discDotscost.x<-Dotscost.x/(1+dr)^(X[,"Year"]-2024); discMDRcost.x<-MDRcost.x/(1+dr)^(X[,"Year"]-2024)
  discDotscost.y<-Dotscost.y/(1+dr)^(X[,"Year"]-2024); discMDRcost.y<-MDRcost.y/(1+dr)^(X[,"Year"]-2024)
  
  # **********************************  Vaccine costs
  Vxcost <- sum(1000*(X[,"VaccDTP3.y"]*pvx[1] + X[,"Vacc10.y"]*pvx[2] + X[,"VaccMass.y"]*pvx[3]))
  
  # **********************************  Results
  CasesAv <- sum(X[,"HIVnegcases.x"]+X[,"HIVposcases.x"] - X[,"HIVnegcases.y"] - X[,"HIVposcases.y"])
  DeathsAv <- sum(X[,"TBDeaths.x"] - X[,"TBDeaths.y"])
  discDeathsAv <- sum(discTBdeaths.x-discTBdeaths.y)
  DALYSAv <- sum(TotalDALYS.x - TotalDALYS.y)
  discDALYSAv <- sum(discTotalDALYS.x - discTotalDALYS.y)
  TreatAv <- sum(Dotscost.x + MDRcost.x - Dotscost.y - MDRcost.y)
  discTreatAv <- sum(discDotscost.x + discMDRcost.x - discDotscost.y - discMDRcost.y)
  
  Netcosts <- Vxcost - TreatAv
  discNetcosts <- Vxcost - discTreatAv
  CostpD<-Netcosts/DeathsAv
  CostpDALY<-Netcosts/DALYSAv
  CostpdiscD<-discNetcosts/discDeathsAv
  CostpdiscDALY<-discNetcosts/discDALYSAv
  
  # Returns Cases/Deaths/DALYS/discounted DALYS/Number Rx averted, Netcosts, discounted net costs, cost per death averted, cost per DALY averted, discounted cost per death av. and discounted cost per DALY av.
  Output<-round(c(CasesAv,DeathsAv,DALYSAv,discDALYSAv,TreatAv,Vxcost,Netcosts,discNetcosts,CostpD,CostpDALY,CostpdiscD,CostpdiscDALY),0)
  return(Output) 
}




