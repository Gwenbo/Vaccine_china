## COUNTRY SPECIFIC 
# Calculates country specific values and rates, and assigns parameters where they vary by country
# hello world

## ********* TRANSMISSION
probt <- 0.1
neta <- sing[1,cntry]
#### TRYING GIT

## ********* AGE specific mortality ** SHOULD PROBABLY BE DONE IN DATA
mm<-mortm[mortm$Country==cntry,]
times<-seq(2009,2050,1)
mort<-matrix(0,length(times),85)
mort<-cbind(times,mort)
## Allocate out the 5yr time periods. 
# for 2009
for (i in 3:87){
  mort[1,i-1]<-mm[1,i]
}
# for 2010-2049
for (kk in 1:8){
  for (j in 1:5){
    for (i in 3:87){
      mort[1+(kk-1)*5+j,i-1]<-mm[kk+1,i]
    }
  }
}
# for 2050
for (i in 3:87){
  mort[42,i-1]<-mm[10,i]
}
colnames(mort)<-c('year',seq(1,85,1))

## ********* New births per year
bb <- births[,cntry]

## ********* Detection and treatment
cdr<-cdrm[cntry,];cdrH<-cdr;suctt<-suctm[cntry,];sucttH<-suctt;
#cdr<-matrix(0.42,1,42);cdrH<-cdr;suctt<-matrix(0.84,1,42);sucttH<-suctt;
## *********** INITIAL population structure 
ps<-pstruc[1:85,cntry] ## Remove those in the pstruc over the age of 85.... 

## ********* HIV and ART 
hivI1<-HIVdata[1,cntry] # HIV incidence in 15-49yos
#steps<-length(seq(year1,yearend,dt))
#hiv<-matrix(0,steps,Mnage);
hivI<-matrix(0,1,Mnage);
hivI[,15:49]<-hivI1; # Only those aged 15-49 can become infected with HIV

art<-artm[cntry,] #matrix(0,1,42)## Changes by year from 2009-2050

## VACCINE SPECIFICS ••••••••••••••••••  
# Coverage varies by country. To keep index as age 
infantcov=Cov[1,cntry]/100
adultcov=Cov[2,cntry]/100
mcampcov=Cov[3,cntry]/100
#infantcov=0.1
#mcampcov=0.1
#adultcov=0.1
