## INITIALISING
# Builds all matrices and initialises (both indicating matrices and output ones)

times<-seq(year1,yearend,dt)
steps<-length(times)

# Susceptible, Susceptible with HIV
S<-matrix(0,steps,Mnage); #SH<-matrix(0,steps,Mnage)
# Latent, Latent with HIV
L<-matrix(0,steps,Mnage);  #LH<-matrix(0,steps,Mnage)
# Infectious, Infectious with HIV
I<-matrix(0,steps,Mnage);  #IH<-matrix(0,steps,Mnage)
# Non-Infectious, Non-Infectious with HIV
NI<-matrix(0,steps,Mnage);  #NIH<-matrix(0,steps,Mnage)
# Recovered, Recovered, Recovered with HIV
R<-matrix(0,steps,Mnage);  #RH<-matrix(0,steps,Mnage)
# New Infectious, New Infectious with HIV
new_I<-matrix(0,steps,Mnage); #new_IH<-matrix(0,steps,Mnage); 
# New non-Infectious, New non-Infectious with HIV
new_NI<-matrix(0,steps,Mnage); #new_NIH<-matrix(0,steps,Mnage);
#new reactivation Infectious/non infectious
new_I_react<-matrix(0,steps,Mnage)
new_NI_react<-matrix(0,steps,Mnage)
# Prevelance of HIV for checking
#prevHIV<-matrix(0,steps,1);prevHIV1549<-matrix(0,steps,1);

# Vaccine matrices: Susceptible, Latent, Recovered and vaccinated. And then with HIV
Sv<-matrix(0,steps,Mnage); Lv<-matrix(0,steps,Mnage);Rv<-matrix(0,steps,Mnage);
#SvH<-matrix(0,steps,Mnage);LvH<-matrix(0,steps,Mnage);RvH<-matrix(0,steps,Mnage);

## Initialising depends on fit...  (Maybe change this...)
if (run==1){
# If first ever run 
  S0<-(1-prop)*psz1900*(ps/(sum(ps)))
  I0<-prop*psz1900*(ps/(sum(ps)))
  S[1,]<-S0; I[1,]<-I0; ### MORE IF HAVE FROM RUN
} else if (run > 1){
  # If not first run then take input
  S0<-(1-sum(prop))*psz1900*(ps/(sum(ps)))
  I0<-prop[1]*psz1900*(ps/(sum(ps)))
  NI0<-prop[2]*psz1900*(ps/(sum(ps)))
  L0<-prop[3]*psz1900*(ps/(sum(ps)))
  R0<-prop[4]*psz1900*(ps/(sum(ps)))
  S[1,]<-S0; I[1,]<-I0; NI[1,]<-NI0; L[1,]<-L0; R[1,]<-R0;  ### MORE IF HAVE FROM RUN
}

# Population size
psize<-matrix(0,steps,1);
psize[1]<-sum(S[1,],L[1,],R[1,],I[1,],NI[1,],Sv[1,],Lv[1,],Rv[1,])
# Number of births
birthsnum<-matrix(0,steps,1); bv<-c();
# FOI
lambda<-matrix(0,steps,1);

## Matrices for cost-effectiveness
# Age of death, age of death with HIV
ADeaths<-matrix(0,steps,Mnage); ADeathsH<-matrix(0,steps,Mnage);
# Number of TB deaths, with HIV, all deaths with HIV, all deaths (column=total of diff types)
TBDeaths<-matrix(0,steps,Mnage);TBDeathsH<-matrix(0,steps,Mnage);AllDeathsH<-matrix(0,steps,Mnage);Deaths<--matrix(0,steps,5);
# Number vaccinated
VX<--matrix(0,steps,3);
# Number of TB treatments, TB Incidence, TB mortality, ?
TBRx<--matrix(0,steps,2);
TBI<-matrix(0,steps,5);
TBM<-matrix(0,steps,7);
TBP<-matrix(0,steps,6)
TBPI<-matrix(0,steps,5)
PSIZEy<-matrix(0,steps,10);

colnames(TBI)<-c("All ages","0-14", "15-54", "55-64", "65+")
colnames(TBM)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "60+")
colnames(TBP)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+")
colnames(TBPI)<-c("All ages","0-14", "15-54", "55-64", "65+")
colnames(PSIZEy)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "15-29", "30-44", "45-59", "60+")


#cumulative cases and deaths
totmort<- matrix(0,1,7)
colnames(totmort)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "60+")

totcase<- matrix(0,1,5)
colnames(totcase)<-c("All ages","0-14", "15-54", "55-64", "65+")
