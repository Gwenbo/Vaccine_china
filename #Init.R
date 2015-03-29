## INITIALISING
# Builds all matrices and initialises (both indicating matrices and output ones)

times<-seq(year1,(yearend+(1-dt)),dt)
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
new_I_noconv<-matrix(0,steps,Mnage)
new_NI_react<-matrix(0,steps,Mnage)
new_actv<-matrix(0,steps,Mnage)
new_actv_react<-matrix(0,steps,Mnage)
new_actv_inf<-matrix(0,steps,Mnage)
new_actv_chk<-matrix(0,steps,Mnage)
num_vac<-matrix(0,steps,Mnage)


# Prevelance of HIV for checking
#prevHIV<-matrix(0,steps,1);prevHIV1549<-matrix(0,steps,1);
#New notifications
new_notif<-matrix(0,steps,Mnage)


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
psize55plus<-matrix(0,steps,1)
psize55minus<-matrix(0,steps,1)
psize0509<-matrix(0,steps,1)
psize1019<-matrix(0,steps,1)
psize2029<-matrix(0,steps,1)
psize3039<-matrix(0,steps,1)
psize4049<-matrix(0,steps,1)
psize5059<-matrix(0,steps,1)
psize6069<-matrix(0,steps,1)
psize70plus<-matrix(0,steps,1)
psize5574<-matrix(0,steps,1)
psize75plus<-matrix(0,steps,1)
psize1524<-matrix(0,steps,1)
psize2554<-matrix(0,steps,1)



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
psize55plus[1]<-sum(S[1,56:Mnage],L[1,56:Mnage],R[1,56:Mnage],I[1,56:Mnage],NI[1,56:Mnage],Sv[1,56:Mnage],Lv[1,56:Mnage],Rv[1,56:Mnage])
psize55minus[1]<-sum(S[1,1:55],L[1,1:55],R[1,1:55],I[1,1:55],NI[1,1:55],Sv[1,1:55],Lv[1,1:55],Rv[1,1:55])

psize0509[1]<-sum(S[i,6:10],L[i,6:10],R[i,6:10],I[i,6:10],NI[i,6:10],Sv[i,6:10],Lv[i,6:10],Rv[i,6:10])
psize1019[1]<-sum(S[i,11:20],L[i,11:20],R[i,11:20],I[i,11:20],NI[i,11:20],Sv[i,11:20],Lv[i,11:20],Rv[i,11:20])
psize2029[1]<-sum(S[i,21:30],L[i,21:30],R[i,21:30],I[i,21:30],NI[i,21:30],Sv[i,21:30],Lv[i,21:30],Rv[i,21:30])
psize3039[1]<-sum(S[i,31:40],L[i,31:40],R[i,31:40],I[i,31:40],NI[i,31:40],Sv[i,31:40],Lv[i,31:40],Rv[i,31:40])
psize4049[1]<-sum(S[i,41:50],L[i,41:50],R[i,41:50],I[i,41:50],NI[i,41:50],Sv[i,41:50],Lv[i,41:50],Rv[i,41:50])
psize5059[1]<-sum(S[i,51:60],L[i,51:60],R[i,51:60],I[i,51:60],NI[i,51:60],Sv[i,51:60],Lv[i,51:60],Rv[i,51:60])
psize6069[1]<-sum(S[i,61:70],L[i,61:70],R[i,61:70],I[i,61:70],NI[i,61:70],Sv[i,61:70],Lv[i,61:70],Rv[i,61:70])
psize70plus[1]<-sum(S[i,71:Mnage],L[i,71:Mnage],R[i,71:Mnage],I[i,71:Mnage],NI[i,71:Mnage],Sv[i,71:Mnage],Lv[i,71:Mnage],Rv[i,71:Mnage])
psize5574[1]<-sum(S[i,56:75],L[i,56:75],R[i,56:75],I[i,56:75],NI[i,56:75],Sv[i,56:75],Lv[i,56:75],Rv[i,56:75])
psize75plus[1]<-sum(S[i,76:Mnage],L[i,76:Mnage],R[i,76:Mnage],I[i,76:Mnage],NI[i,76:Mnage],Sv[i,76:Mnage],Lv[i,76:Mnage],Rv[i,76:Mnage])
psize1524[1]<-sum(S[i,16:25],L[i,16:25],R[i,16:25],I[i,16:25],NI[i,16:25],Sv[i,16:25],Lv[i,16:25],Rv[i,16:25])
psize2554[1]<-sum(S[i,26:55],L[i,26:55],R[i,26:55],I[i,26:55],NI[i,26:55],Sv[i,26:55],Lv[i,26:55],Rv[i,26:55])



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
VX<-matrix(0,steps,3); vaccgive<-matrix(0,1,5); 
vaccgiveyr<-matrix(0,1,26);
NNV<-matrix(0,12,6)
pcreduI<-matrix(0,9,36)
pcreduM<-matrix(0,9,36)

# Number of TB treatments, TB Incidence, TB mortality, ?

TBRx<--matrix(0,steps,2);
TBI<-matrix(0,steps,9);
TBM<-matrix(0,steps,11);
TBP<-matrix(0,steps,7);
TBPb<-matrix(0,steps,7);
TBPI<-matrix(0,steps,16);
PSIZEy<-matrix(0,steps,23);
TBRa<-matrix(0,steps,6);
TBRa2<-matrix(0,steps,6);
TBRi<-matrix(0,steps,6);
TBRi2<-matrix(0,steps,6);
TBInew<-matrix(0,steps,6);
TBProp<-matrix(0,steps,4);
TBN<-matrix(0,steps,7)
ARI<-matrix(0,steps,4)
TBAc<-matrix(0,(yearend-year1+1),1)
TBMo<-matrix(0,(yearend-year1+1),1)
NV<-matrix(0,(yearend-year1+1),1)

colnames(TBI)<-c("All ages","0-14", "15-54", "55-64", "65+", "55+", "<55","15-24","25-54")
colnames(TBN)<-c("All ages","0-14", "15-54", "55-64", "65+","55+", "<55")
colnames(TBM)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "60+", "55+", "<55","15-24","25-54")
colnames(TBP)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+", "55+")
colnames(TBPb)<-c("All ages","0-14", "15-29", "30-44", "45-59", "60+", "55+")
colnames(TBPI)<-c("All ages", "0-14", "15-54", "55-64", "65+", "55+", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+","6574", "75+")
colnames(PSIZEy)<-c("All ages", "0-14", "15-54", "55-64", "65+", "15-59", "15-29", "30-44", "45-59", "60+", "55+", "5-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+","55-74", "75+","15-24", "25-54")

#ohthers
I2050<-matrix(0,5,1)


#cumulative cases and deaths
totmort<- matrix(0,1,4)
totmortyr<- matrix(0,(steps*dt),4)
colnames(totmort)<-c("All ages", "65+", "55+","<55")

totcase<- matrix(0,1,4)
totcaseyr<- matrix(0,(steps*dt),4)
colnames(totcase)<-c("All ages","65+","55+", "<55")

cumuloutyr<- matrix(0,26,8)