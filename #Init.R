## INITIALISING
# Builds all matrices and initialises (both indicating matrices and output ones)

times<-seq(year1,yearend,dt)
steps<-length(times)

# Susceptible, Susceptible with HIV
S<-matrix(0,steps,Mnage); SH<-matrix(0,steps,Mnage)
# Latent, Latent with HIV
L<-matrix(0,steps,Mnage);  LH<-matrix(0,steps,Mnage)
# Infectious, Infectious with HIV
I<-matrix(0,steps,Mnage);  IH<-matrix(0,steps,Mnage)
# Non-Infectious, Non-Infectious with HIV
NI<-matrix(0,steps,Mnage);  NIH<-matrix(0,steps,Mnage)
# Recovered, Recovered, Recovered with HIV
R<-matrix(0,steps,Mnage);  RH<-matrix(0,steps,Mnage)
#newly infected people
new_infect<-matrix(0,steps,Mnage)
# New Infectious, New Infectious with HIV
new_I<-matrix(0,steps,Mnage); new_IH<-matrix(0,steps,Mnage); 
# New non-Infectious, New non-Infectious with HIV
new_NI<-matrix(0,steps,Mnage); new_NIH<-matrix(0,steps,Mnage);
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
psize<-matrix(0,steps,1)
psize014<-matrix(0,steps,1)
psize1554<-matrix(0,steps,1)
psize5564<-matrix(0,steps,1)
psize65plus<-matrix(0,steps,1)
psize1559<-matrix(0,steps,1)
psize1529<-matrix(0,steps,1)
psize3044<-matrix(0,steps,1)
psize4559<-matrix(0,steps,1)
psize60plus<-matrix(0,steps,1)

#psize and I for contact matirx
psizematrix<-matrix(0,steps,4)
Imatrix<-matrix(0,steps,4)
BIRTHS<-vector('numeric',steps)

#filling in first time step as is usually calculated at later tme steps of the year
psize[1]<-sum(S[1,],L[1,],R[1,],I[1,],NI[1,],Sv[1,],Lv[1,],Rv[1,])
psize014[1]<-sum(S[1,1:15],L[1,1:15],R[1,1:15],I[1,1:15],NI[1,1:15],Sv[1,1:15],Lv[1,1:15],Rv[1,1:15])
psize1554[1]<-sum(S[1,16:55],L[1,16:55],R[1,16:55],I[1,16:55],NI[1,16:55],Sv[1,16:55],Lv[1,16:55],Rv[1,16:55])
psize5564[1]<-sum(S[1,56:65],L[1,56:65],R[1,56:65],I[1,56:65],NI[1,56:65],Sv[1,56:65],Lv[1,56:65],Rv[1,56:65])
psize65plus[1]<-sum(S[1,66:Mnage],L[1,66:Mnage],R[1,66:Mnage],I[1,66:Mnage],NI[1,66:Mnage],Sv[1,66:Mnage],Lv[1,66:Mnage],Rv[1,66:Mnage])

psize1559[1]<-sum(S[1,16:60],L[1,16:60],R[1,16:60],I[1,16:60],NI[1,16:60],Sv[1,16:60],Lv[1,16:60],Rv[1,16:60])
psize1529[1]<-sum(S[1,16:30],L[1,16:30],R[1,16:30],I[1,16:30],NI[1,16:30],Sv[1,16:30],Lv[1,16:30],Rv[1,16:30])
psize3044[1]<-sum(S[1,31:45],L[1,31:45],R[1,31:45],I[1,31:45],NI[1,31:45],Sv[1,31:45],Lv[1,31:45],Rv[1,31:45])
psize4559[1]<-sum(S[1,46:60],L[1,46:60],R[1,46:60],I[1,46:60],NI[1,46:60],Sv[1,46:60],Lv[1,46:60],Rv[1,46:60])
psize60plus[1]<-sum(S[1,61:Mnage],L[1,61:Mnage],R[1,61:Mnage],I[1,61:Mnage],NI[1,61:Mnage],Sv[1,61:Mnage],Lv[1,61:Mnage],Rv[1,61:Mnage])

#initalise contact matrices
psizematrix[1,1]<-sum(S[1,1:6],L[1,1:6],R[1,1:6],I[1,1:6],NI[1,1:6],Sv[1,1:6],Lv[1,1:6],Rv[1,1:6])
psizematrix[1,2]<-sum(S[1,7:20],L[1,7:20],R[1,7:20],I[1,7:20],NI[1,7:20],Sv[1,7:20],Lv[1,7:20],Rv[1,7:20])
psizematrix[1,3]<-sum(S[1,21:65],L[1,21:65],R[1,21:65],I[1,21:65],NI[1,21:65],Sv[1,21:65],Lv[1,21:65],Rv[1,21:65])
psizematrix[1,4]<-sum(S[1,66:Mnage],L[1,66:Mnage],R[1,66:Mnage],I[1,66:Mnage],NI[1,66:Mnage],Sv[1,66:Mnage],Lv[1,66:Mnage],Rv[1,66:Mnage])

Imatrix[1,1]<-sum(I[1,1:6])
Imatrix[1,2]<-sum(I[1,7:20])
Imatrix[1,3]<-sum(I[1,21:65])
Imatrix[1,4]<-sum(I[1,66:Mnage])

# Imatrix[1,1]<-sum(I0[1:6])
# Imatrix[1,2]<-sum(I0[7:20])
# Imatrix[1,3]<-sum(I0[21:65])
# Imatrix[1,4]<-sum(I0[66:Mnage])
# psizematrix[1,1]<-sum(ps[1:6])
# psizematrix[1,2]<-sum(ps[7:20])
# psizematrix[1,3]<-sum(ps[21:65])
# psizematrix[1,4]<-sum(ps[66:Mnage])

# FOI
lambda<-matrix(0,steps,Mnage);

#initialise lambda. not sure if necessary since other elements are initialised?
lambda[1,1:Mnage]<- t(neta * (1-exp(colSums(-(myneta[1:4,1:Mnage]) * z * ((Imatrix[1,1:4])/(psizematrix[1,1:4]))))))


# Number of births,b rate
birthsnum<-matrix(0,steps,1); bv<-c(); brate<-c();


## Matrices for cost-effectiveness
# Age of death, age of death with HIV
ADeaths<-matrix(0,steps,Mnage); ADeathsH<-matrix(0,steps,Mnage);
# Number of TB deaths, with HIV, all deaths with HIV, all deaths (column=total of diff types)
TBDeaths<-matrix(0,steps,Mnage);TBDeathsH<-matrix(0,steps,Mnage);AllDeathsH<-matrix(0,steps,Mnage);Deaths<--matrix(0,steps,5);
# Number vaccinated
VX<-matrix(0,steps,3); vaccgive<-matrix(0,1,3)
NNV<-matrix(0,12,4)
# Number of TB treatments, TB Incidence, TB mortality, ?

TBRx<--matrix(0,steps,2);
TBI<-matrix(0,steps,5);
TBM<-matrix(0,steps,7);
TBP<-matrix(0,steps,6);
TBPb<-matrix(0,steps,6);
TBPI<-matrix(0,steps,5);
PSIZEy<-matrix(0,steps,10);
TBRa<-matrix(0,steps,5);
TBRi<-matrix(0,steps,5);
TBInew<-matrix(0,steps,5);

colnames(TBI)<-c("All ages","0-14", "15-54", "55-64", "65+")
colnames(TBM)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "60+")
colnames(TBP)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+")
colnames(TBPb)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+")
colnames(TBPI)<-c("All ages","0-14", "15-54", "55-64", "65+")
colnames(PSIZEy)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "15-29", "30-44", "45-59", "60+")


#cumulative cases and deaths
totmort<- matrix(0,1,2)
colnames(totmort)<-c("All ages", "65+")

totcase<- matrix(0,1,2)
colnames(totcase)<-c("All ages","65+")
