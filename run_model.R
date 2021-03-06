# Example code to run vaccine model
# What libraries or packages do you need?
library(plyr)

# Where do you want the model to run? where are the parameters/functions I need to load?
home<-"/Users/Rebecca/Vaccine_china" # Could also set up and output folder
setwd(home)
source('#DataGrab.R')
setwd(home)
source('CFunctions.R')


# On cluster (C=1) or no (C=0)?
C=0

# What country? (For you this will always be China for now)
cntry<-"China"

## Grab parameter fits for China (these are the 1000 from the paper)
setwd(home);setwd("Data")
para<-read.csv(paste("paraout_",cntry,".csv",sep=''))[-1]
nm<-c(pararange[,1],"p0") # The parameter ranges
setwd(home)

# Simulate data 
 rrun<-1 # It can be as high as 1000 but just do a few to get a feel and for it to be quicker
 xout<-c(); eee<-c(); # Initialise all vectors to be empty
 for (kkk in 1:rrun){
   print(kkk)
  #kkk<-1
  for (i in 1:length(nm)){assign(nm[i],as.numeric(para[kkk,i]))} # Assign the parameters to the correct values
  neta2<-neta # this parameter needs extra assigning for some annoying reason! 
  
  # Run the model with these parameters  
  #4th setof terms, second entry of that term is the timestep
  system.time(Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,(1/12),c(0.02,0.02,0.8,0.07)),c(1900,2050),0,0))   
  }

setwd(home)
source('#BasicPlot.R')

setwd(home)
source('TBPI by age plot.R')

setwd(home)
source('#notif fitting.R')

  #EOn<-Econout;h10<-hbcout
  # save in big df for plot - original one
  #eee<-cbind(hbcout,kkk); colnames(eee)<-c(colnames(hbcout),"fit");
  #eee<-data.frame(eee)
  #newo<-ddply(eee,.(Year,fit),summarise,psize=Psize,tbi=100000*(negcases)/Psize,tbm=100000*(negdeaths)/Psize,tbih=100000*(poscases)/Psize,tbmh=100000*(posdeaths)/Psize)
  #newo<-ddply(eee,.(Year,fit),summarise,psize=Psize,tbi=100000*(negcases)/Psize,tbm=100000*(negdeaths)/Psize,tbih=100000*(poscases)/Psize,tbmh=100000*(posdeaths)/Psize)
  #xout<-rbind(xout,newo)
# }

# I1990<-matrix(0,5,1)
# I2020<-matrix(0,5,1)
# I2050<-matrix(0,5,1)
# 
# I1990
# I2020
 I2050
##for use in calculating NNV
#print(TBInew)

setwd(home)
source('#BasicPlot.R')

par(mfrow=c(1,1))
plot(seq(1900,2050),brate, ylab="birth rate",xlab="year", ylim=c(0,0.05),main="birthrate",type='l',col='red')


plot(seq(1900,2050.5,0.5),rowSums(I[1:302,1:101]), ylab="number",xlab="year", ylim=c(0,2000000),type='l',col='red')
lines((seq(1900,2050.5,0.5)),rowSums(R[1:302,1:101]),type='l',col='orange')
lines((seq(1900,2050.5,0.5)),rowSums(S[1:302,1:101]),type='l',col='green')
lines((seq(1900,2050.5,0.5)),rowSums(NI[1:302,1:101]),type='l',col='yellow')
lines((seq(1900,2050.5,0.5)),rowSums(L[1:302,1:101]),type='l',col='purple')

plot(seq(1970,2050.5,0.5),rowSums(I[141:302,1:101]), ylab="number",xlab="year", ylim=c(0,200),type='l',col='red')
lines((seq(1970,2050.5,0.5)),rowSums(TBDeaths[141:302,1:101]),type='l',col='orange')



# #### DATA - what it was fitted to 
# Ana<-M[7:12,cntry]
# AnaLimo<-cbind(M[1:6,cntry],M[13:18,cntry])
# AnaLim<-AnaLimo

# # Convert to data.frame and only want 1980 onwards
# #xout<-data.frame(xout)
# neww<-xout[which(xout[,"Year"]>1980),]
# 
# ## Plot data - required packages
# library(ggplot2);theme_set(theme_bw())
# library(plyr);library(gridExtra)
# 
# #### DATA - what it was fitted to 
# Ana<-M[7:12,cntry]
# AnaLimo<-cbind(M[1:6,cntry],M[13:18,cntry])
# AnaLim<-AnaLimo
# 
# # Need to convert HIV range to 5x larger 
# if(cntry=="Afghanistan"){lower<-0}else{lower<-0.01}
# AnaLim[5,1] = max(lower,Ana[5] - (Ana[5]-AnaLim[5,1])*5)
# AnaLim[6,1] = max(lower,Ana[6] - (Ana[6]-AnaLim[6,1])*5)
# AnaLim[5:6,2] = Ana[5:6] + (AnaLim[5:6,2]-Ana[5:6])*5
# 
# 
# # ****************** Plot median 
# ### Get median and ranges
# meds <- ddply(neww, .(Year), summarise, min_ps=min(psize),med_ps = median(psize),max_ps=max(psize),min_tbi=min(tbi),
#               med_tbi = median(tbi),max_tbi=max(tbi), 
#               min_tbm=min(tbm),med_tbm = median(tbm),max_tbm=max(tbm),
#               min_tbih=min(tbih),med_tbih = median(tbih),max_tbih=max(tbih),
#               min_tbmh=min(tbmh),med_tbmh = median(tbmh),max_tbmh=max(tbmh))
# 
# setwd(home);setwd("Output/A.output")
# write.csv(meds,paste(cntry,"_median&range.csv",sep=''))
# 
# ## Plot
# setwd(home);setwd("Output/A.plots")
# gg <- ggplot(meds, aes(x=Year)) + scale_y_continuous("Population size") + scale_x_continuous("Year")
# gg <- gg + geom_ribbon(aes(ymin=min_ps, ymax=max_ps),fill='grey') + geom_line(aes(y=med_ps)) + theme(legend.position="none")
# gg <- gg + geom_errorbar(aes(x=2009, ymin=AnaLim[1,1], ymax=AnaLim[1,2]),width=1,col='red')+ geom_point(x=2009,y=Ana[1],pch=4,col='red',size=2.5) 
# gg <- gg + geom_errorbar(aes(x=2049, ymin=AnaLim[2,1], ymax=AnaLim[2,2]),width=5,col='red')+ geom_point(x=2049,y=Ana[2],pch=4,col='red',size=2.5) 
# ggsave(paste(cntry,"_tbps_med.pdf",sep=''))
# h <- ggplot(meds, aes(x=Year)) + scale_y_continuous("TB Incidence \n (cases per 100,000)") + scale_x_continuous("Year")
# h <- h + geom_ribbon(aes(ymin=min_tbi, ymax=max_tbi),fill='grey') + geom_line(aes(y=med_tbi)) + theme(legend.position="none")
# h <- h + geom_errorbar(aes(x=2009, ymin=AnaLim[3,1], ymax=AnaLim[3,2]),width=1,col='red')+ geom_point(x=2009,y=Ana[3],pch=4,col='red',size=2.5) 
# ggsave(paste(cntry,"_tbi_med.pdf",sep=''))
# hh <- ggplot(meds, aes(x=Year)) + scale_y_continuous("TB Mortality \n (deaths per 100,000)") + scale_x_continuous("Year")
# hh <- hh + geom_ribbon(aes(ymin=min_tbm, ymax=max_tbm),fill='grey') + geom_line(aes(y=med_tbm))+ theme(legend.position="none")
# hh <- hh + geom_errorbar(aes(x=2009, ymin=AnaLim[4,1], ymax=AnaLim[4,2]),width=1,col='red')+ geom_point(x=2009,y=Ana[4],pch=4,col='red',size=2.5) 
# ggsave(paste(cntry,"_tbm_med.pdf",sep=''))
# hhh <- ggplot(meds, aes(x=Year)) + scale_y_continuous("TB Incidence in HIV+ \n (cases per 100,000)") + scale_x_continuous("Year")
# hhh <- hhh + geom_ribbon(aes(ymin=min_tbih, ymax=max_tbih),fill='grey') + geom_line(aes(y=med_tbih))+ theme(legend.position="none")
# hhh <- hhh + geom_errorbar(aes(x=2009, ymin=AnaLim[5,1], ymax=AnaLim[5,2]),width=1,col='red')+ geom_point(x=2009,y=Ana[5],pch=4,col='red',size=2.5) 
# ggsave(paste(cntry,"_tbih_med.pdf",sep=''))
# hg <- ggplot(meds, aes(x=Year)) + scale_y_continuous("TB Mortality in HIV+ \n (deaths per 100,000)") + scale_x_continuous("Year")
# hg <- hg + geom_ribbon(aes(ymin=min_tbmh, ymax=max_tbmh),fill='grey') + geom_line(aes(y=med_tbmh))+ theme(legend.position="none")
# hg <- hg + geom_errorbar(aes(x=2009, ymin=AnaLim[6,1], ymax=AnaLim[6,2]),width=1,col='red')+ geom_point(x=2009,y=Ana[6],pch=4,col='red',size=2.5) 
# ggsave(paste(cntry,"_tbmh_med.pdf",sep=''))
# 
# # Structure with psize row1, hiv- row2, hiv+ row3
# blankPanel<-grid.rect(gp=gpar(col="white")) 
# pdf(paste(cntry,"cloud_all.pdf"))
# grid.arrange(gg,blankPanel,h,hh,hhh,hg,ncol=2)    
# dev.off() 
# 
# # Figure 1 plot
# blankPanel<-grid.rect(gp=gpar(col="white")) 
# pdf(paste(cntry,"cloud_3.pdf"),width=10,height=4)
# grid.arrange(gg,h,hh,ncol=3)
# dev.off() 
# 
# ## FOLLOWING MAY NOT WORK IF >1000....
# # *************** Plot fits to one pdf
# pp1<-ggplot(neww,aes(x=Year,y=psize,colour=factor(fit)))+geom_line(size=1)+scale_x_continuous("Years",lim=c(1980,2050))
# pp<- pp1 + scale_y_continuous("Population size",lim=c(0,max(neww[,'psize'],AnaLim[1:2,]))) + theme(legend.position="none")
# pp<- pp + geom_errorbar(aes(x=2009, ymin=AnaLim[1,1], ymax=AnaLim[1,2]),width=1,col='red')+ geom_point(x=2009,y=Ana[1],pch=4,col='red',size=2.5) 
# pp<- pp + geom_errorbar(aes(x=2049, ymin=AnaLim[2,1], ymax=AnaLim[2,2]),width=5,col='red')+ geom_point(x=2049,y=Ana[2],pch=4,col='red',size=2.5) 
# 
# qq1<-ggplot(neww,aes(x=Year,y=tbi,colour=factor(fit)))+geom_line(size=1)+scale_x_continuous("Years",lim=c(1980,2050))
# qq<- qq1 + scale_y_continuous("TB Incidence",lim=c(0,max(neww[,'tbi'],AnaLim[3,]))) + geom_point(x=2009,y=Ana[3],pch=4,col='red',size=2.5) 
# qq11<- qq + geom_errorbar(aes(x=2009, ymin=AnaLim[3,1], ymax=AnaLim[3,2]),width=5,col='red')+ theme(legend.position="none")
# 
# qq1<-ggplot(neww,aes(x=Year,y=tbm,colour=factor(fit)))+geom_line(size=1)+scale_x_continuous("Years",lim=c(1980,2050))
# qq<- qq1 + scale_y_continuous("TB Mortality",lim=c(0,max(neww[,'tbm'],AnaLim[4,]))) + geom_point(x=2009,y=Ana[4],pch=4,col='red',size=2.5) 
# qq2<- qq + geom_errorbar(aes(x=2009, ymin=AnaLim[4,1], ymax=AnaLim[4,2]),width=5,col='red')+ theme(legend.position="none")
# 
# qq1<-ggplot(neww,aes(x=Year,y=tbih,colour=factor(fit)))+geom_line(size=1)+scale_x_continuous("Years",lim=c(1980,2050))
# qq<- qq1 + scale_y_continuous("TB Incidence in HIV+",lim=c(0,max(neww[,'tbih'],AnaLim[5,]))) + geom_point(x=2009,y=Ana[5],pch=4,col='red',size=2.5) 
# qq3<- qq + geom_errorbar(aes(x=2009, ymin=AnaLim[5,1], ymax=AnaLim[5,2]),width=5,col='red')+ theme(legend.position="none")
# 
# qq1<-ggplot(neww,aes(x=Year,y=tbmh,colour=factor(fit)))+geom_line(size=1)+scale_x_continuous("Years",lim=c(1980,2050))
# qq<- qq1 + scale_y_continuous("TB Mortality in HIV+",lim=c(0,max(neww[,'tbmh'],AnaLim[6,]))) + geom_point(x=2009,y=Ana[6],pch=4,col='red',size=2.5) 
# qq4<- qq + geom_errorbar(aes(x=2009, ymin=AnaLim[6,1], ymax=AnaLim[6,2]),width=5,col='red')+ theme(legend.position="none")
# 
# # Structure with psize row1, hiv- row2, hiv+ row3
# blankPanel<-grid.rect(gp=gpar(col="white")) 
# # save in plots folder
# setwd(home);setwd("Output/A.plots")
# pdf(paste(cntry,"fits.pdf"))
# grid.arrange(pp,blankPanel,qq11,qq2,qq3,qq4, ncol=2)
# dev.off()
# 
# 
######******************************************* Run vaccine scenarios


home<-"/Users/Rebecca/Vaccine_china" # Could also set up and output folder
setwd(home)
source('#DataGrab.R')
setwd(home)
source('CFunctions.R')
# On cluster (C=1) or no (C=0)?
C=0
# What country? (For you this will always be China for now)
cntry<-"China"

## Grab parameter fits for China (these are the 1000 from the paper)
setwd(home);setwd("Data")
para<-read.csv(paste("paraout_",cntry,".csv",sep=''))[-1]
nm<-c(pararange[,1],"p0") # The parameter ranges
setwd(home)

## Vaccine interventions
typen<-7 ## Number of vaccine types (increased to 7 to inc latency vaccine and adult/ado vaccine)
effs<-c(40,60,80)/100
durs<-c(10,20)
cover<-c(0.3,0.7)
combn<-length(effs)*length(durs)*length(cover) ## Number of efficacy and duration combinations

# Run Vaccines and where to store
print("Running vaccines")
setwd(home);

# Storage data frame
dfvx<-c()
cumulvx<-c()
vaxgive<-c()
vaxgiveyr<-c()
cumulvxyrM<-c()
cumulvxyrI<-c()
NumV<-c()
inc2050<-c()
mort2050<-c()


# Run through all fits
for (kkk in 1:1){ # Again this could be 1000 but just do 10 for example 
  
  for (i in 1:length(nm)){assign(nm[i],as.numeric(para[kkk,i]))}
  neta2<-neta
  
  # CHECK against yyy in FITGO - usually all OK apart from neta!
  #xxx<-c(pchild,padult,pH,v,vH,x,xH,fchild,fadult,fH,w,n,nH,r,rH,e,g,gH,hchild,hadult,hH,LEHIV,LEART,effH,effHa,rmort,neta,rmortTB,CDRscale,alpha)
  #print(xxx)
  # CHECK parameters in correct range
  #pp<-cbind(as.numeric(pararange[,2]),as.numeric(pararange[,3]))
  #for (i in 1:length(pararange[,1])){if((dat[kkk,i+1]<=pp[i,2])&(dat[kkk,i+1]>=pp[i,1])){print(c(i,"OK"))}else{print(c(i,dat[kkk,i+1],pp[i,]))}}
  
  # Run the model with these parameters 
  # Second input of length 1 so "no vaccine" scenario
  Xn<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE, alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,C)  
  #EOn<-Econout;h10<-hbcout
  # save in big df for plot - original one
  setwd(home)
  source('#BasicPlot.R')
  
  new_active<-cbind(TBAc,0,0)
  new_ac_age<-cbind(TBAc_age,0,0)
  new_mort<-cbind(TBMo,0,0)
  inc2050<-rbind(TBI[151,])
  mort2050<-rbind(TBM[151,])
  
  
  eee<-cbind(Xn,0,0); colnames(eee)<-c(colnames(Xn),"type","vxint")
   dfvx<-rbind(dfvx,eee)
#     ggg<-cbind(cumulout,0,0); colnames(ggg)<-c(colnames(cumulout),"type","vxint")
#     cumulvx<-rbind(cumulvx,ggg)
#     cumulvx<-as.matrix(cumulvx)
#     cumulvxyrM<-cbind(cumulvxyrM,cumuloutyr[,1])
#     cumulvxyrM<-as.matrix(cumulvxyrM)
#     cumulvxyrI<-cbind(cumulvxyrI,cumuloutyr[,3])
#     cumulvxyrI<-as.matrix(cumulvxyrI)
  # For each type of vaccine
  #have set to start at 2 only as only doing vaccine type 2(S only), 3 (L/R only) and 4 (all)  plus same again for ado/adult at the moment
  for (nn in 2:typen){
    
    count<-0;coms<-matrix(0,combn,3);
    #for each coverage
    for (vv in 1:length(cover)){
      # For each efficacy
      for (zz in 1:length(effs)){
      # For each duration
      for (xx in 1:length(durs)){
        count<-count+1  
        coms[count,]<-c(cover[vv],effs[zz],durs[xx])
        cov<-cover[vv]; tic <- effs[zz];    toc <- durs[xx];   print(c(cov,tic,toc))
        # Length of second input > 1 so triggers FitGo to do a vaccine scenario
       X<-FitGo(cntry,c(nn,cov,tic,toc),c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,C)  
        #X<-FitGo(cntry,1,c(p0,rmort,neta2,rmortTB,CDRscale,CDRscaleE,alpha),c(2,0.5,c(0.02,0.02,0.8,0.07)),c(1900,2050),0,C)  
        
        
        #         ae<-merge(EOn,Econout,by="Year");ae<-ae[c(1:5,9:17)];
#         ae<-ae[125:150,]
#         # save in countries VXout for DALY calc       
#         # save in big df for plot
          eee<-cbind(X,nn,count); colnames(eee)<-c(colnames(X),"type","vxint")
          dfvx<-rbind(dfvx,eee)
          dfvx<-as.matrix(dfvx)
          
          #calcs needed for NNV
          new_active<-cbind(new_active,TBAc,nn,count)
          new_ac_age<-cbind(new_ac_age,TBAc_age,nn,count)
          new_mort<-cbind(new_mort,TBMo,nn,count)
          NumV<-cbind(NumV,NV,nn,count)

          #outputting 2050 incidence rate
          inc2050<-rbind(inc2050,TBI[151,])
          mort2050<-rbind(mort2050,TBM[151,])


      }}}}
assign('dfvx',dfvx,envir=.GlobalEnv)
assign('new_active',new_active,envir=.GlobalEnv)
assign('new_ac_age',new_ac_age,envir=.GlobalEnv)
assign('new_mort',new_mort,envir=.GlobalEnv)
assign('NumV',NumV,envir=.GlobalEnv)
assign('inc2050',inc2050,envir=.GlobalEnv)
assign('mort2050',mort2050,envir=.GlobalEnv)


setwd("Outputs")
write.table(new_active,'new_active.csv',sep=",",row.names=FALSE)
write.table(new_mort,'new_mort.csv',sep=",",row.names=FALSE)
write.table(NumV,'number_vaccinated.csv',sep=",",row.names=FALSE)
write.table(inc2050,'inc_rates_2050.csv',sep=",",row.names=FALSE)
write.table(mort2050,'mort_rates_2050.csv',sep=",",row.names=FALSE)

setwd(home)
} # end of fits

setwd(home)
source('#PlotVax.R')
setwd(home)

setwd(home)
source('#%reduction.R')
setwd(home)

source('#NNV.R')
setwd(home)

write.table(dfvx,'dfvx.csv',sep=",",row.names=FALSE)



#reduction in number of cases or deaths calc by subtracting cumul cases in vaccine scenario from the cumulative number in the baseline scenario
# redu<-rbind((cumulvx[1,]-cumulvx[2,]),(cumulvx[1,]-cumulvx[3,]),(cumulvx[1,]-cumulvx[4,]),(cumulvx[1,]-cumulvx[5,]),(cumulvx[1,]-cumulvx[6,]),(cumulvx[1,]-cumulvx[7,]),(cumulvx[1,]-cumulvx[8,]),(cumulvx[1,]-cumulvx[9,]),(cumulvx[1,]-cumulvx[10,]),(cumulvx[1,]-cumulvx[11,]),(cumulvx[1,]-cumulvx[12,]),(cumulvx[1,]-cumulvx[13,]))
# redu<-redu[,-7]
# redu<-redu[,-8]
# 
# NNV[,1]<-vaxgive[,1]/redu[,1]
# NNV[,3]<-vaxgive[,1]/redu[,2]
# NNV[,2]<-vaxgive[,1]/redu[,3]
# NNV[,4]<-vaxgive[,1]/redu[,4]
# NNV[,6]<-vaxgive[,1]/redu[,5]
# NNV[,5]<-vaxgive[,1]/redu[,6]
# colnames(NNV)<-c("NNVm_all","NNVm_55+","NNVm_eld","NNVi_all", "NNVi_55+","NNVi_eld")
# write.table(NNV,'NNV_results.csv',sep=",",row.names=FALSE)
# write.table(redu,'redu_results.csv',sep=",",row.names=FALSE)
# 
# cumulvxyrI<-t(cumulvxyrI)
# cumulvxyrM<-t(cumulvxyrM)
# vaxgiveyr<-vaxgiveyr[,1:26]
# cumulvxyrI<-cumulvxyrI[,1:26]
# cumulvxyrM<-cumulvxyrM[,1:26]
# reduyrI<-rbind((cumulvxyrI[1,]-cumulvxyrI[2,]),(cumulvxyrI[1,]-cumulvxyrI[3,]),(cumulvxyrI[1,]-cumulvxyrI[4,]),(cumulvxyrI[1,]-cumulvxyrI[5,]),(cumulvxyrI[1,]-cumulvxyrI[6,]),(cumulvxyrI[1,]-cumulvxyrI[7,]),(cumulvxyrI[1,]-cumulvxyrI[8,]),(cumulvxyrI[1,]-cumulvxyrI[9,]),(cumulvxyrI[1,]-cumulvxyrI[10,]),(cumulvxyrI[1,]-cumulvxyrI[11,]),(cumulvxyrI[1,]-cumulvxyrI[12,]),(cumulvxyrI[1,]-cumulvxyrI[13,]))
# reduyrM<-rbind((cumulvxyrM[1,]-cumulvxyrM[2,]),(cumulvxyrM[1,]-cumulvxyrM[3,]),(cumulvxyrM[1,]-cumulvxyrM[4,]),(cumulvxyrM[1,]-cumulvxyrM[5,]),(cumulvxyrM[1,]-cumulvxyrM[6,]),(cumulvxyrM[1,]-cumulvxyrM[7,]),(cumulvxyrM[1,]-cumulvxyrM[8,]),(cumulvxyrM[1,]-cumulvxyrM[9,]),(cumulvxyrM[1,]-cumulvxyrM[10,]),(cumulvxyrM[1,]-cumulvxyrM[11,]),(cumulvxyrM[1,]-cumulvxyrM[12,]),(cumulvxyrM[1,]-cumulvxyrM[13,]))
# 
# NNVi<-vaxgiveyr/reduyrI
# NNVi<-t(NNVi)
# NNVm<-vaxgiveyr/reduyrM
# NNVm<-t(NNVm)
# par(mfrow=c(1,1))
# plot(seq(2030,2050),NNVi[6:26,1],ylab='NNV',xlab='yr',type='l')
# lines(seq(2030,2050),NNVi[6:26,2], lty=1,col='red')
# lines(seq(2030,2050),NNVi[6:26,3], lty=1,col='orange')
# lines(seq(2030,2050),NNVi[6:26,4], lty=1,col='yellow')
# lines(seq(2030,2050),NNVi[6:26,5], lty=1,col='green')
# lines(seq(2030,2050),NNVi[6:26,6], lty=1,col='purple')
# lines(seq(2030,2050),NNVi[6:26,7], lty=5,col='black')
# lines(seq(2030,2050),NNVi[6:26,8], lty=5,col='red')
# lines(seq(2030,2050),NNVi[6:26,9], lty=5,col='orange')
# lines(seq(2030,2050),NNVi[6:26,10], lty=5,col='yellow')
# lines(seq(2030,2050),NNVi[6:26,11], lty=5,col='green')
# lines(seq(2030,2050),NNVi[6:26,12], lty=5,col='purple')
# 
# test<-matrix(0,12,1)
# test[1,]<-mean(NNVi[,1])
# test[2,]<-mean(NNVi[,4])

# y<-subset(dfvx,dfvx$type==1)
# y<-subset(dfvx,dvfx[,54]==3)
#           
#           select=c('TBItot','TBI0-14','TBI15-54','TBI55-64','TBI65+'))
# 
# y<-dfvx[,dfvx$type == 2]
# y<-dfvx[,'type' == 2]
# add line of Y






setwd(paste(home,"Output/A.vx_plots",sep=''))

# Make matrix of required values
neww<-ddply(dfvx,.(Year,fit,type,vxint),summarise,psize=Psize,tbi=100000*(negcases)/Psize,tbm=100000*(negdeaths)/Psize,tbih=100000*(poscases)/Psize,tbmh=100000*(posdeaths)/Psize)

### Calculate median
meds_ps <- ddply(neww, .(Year,type,vxint), summarise, min=min(psize),med = median(psize),max=max(psize))
meds_tbi <- ddply(neww, .(Year,type,vxint), summarise, min=min(tbi),med = median(tbi),max=max(tbi))
meds_tbm <- ddply(neww, .(Year,type,vxint), summarise, min=min(tbm),med = median(tbm),max=max(tbm))
meds_tbih <- ddply(neww, .(Year,type,vxint), summarise, min=min(tbih),med = median(tbih),max=max(tbih))
meds_tbmh <- ddply(neww, .(Year,type,vxint), summarise, min=min(tbmh),med = median(tbmh),max=max(tbmh))

## Which to plot? eff = 40, 60, 80. Dur = 5, 10, LL. New sub-DFs...
mm<-c(40,60,80)/100;md<-c(5,10,100); combn2<-c()
for (im in 1:length(mm)){am<-which(coms[,1]==mm[im])
                         for (imd in 1:length(md)){bm<-which(coms[,2]==md[imd]);
                                                   combn2<-c(combn2,intersect(am,bm))}}
# these have only the interventions required for the plot
ps2o<-c();tbi2o<-c();tbm2o<-c();tbih2o<-c();tbmh2o<-c();
for (kj in 1:length(combn2)){ps2o <- rbind(ps2o,meds_ps[which(meds_ps[,"vxint"]==combn2[kj]),])}
for (kj in 1:length(combn2)){tbi2o <- rbind(tbi2o,meds_tbi[which(meds_tbi[,"vxint"]==combn2[kj]),])}
for (kj in 1:length(combn2)){tbm2o <- rbind(tbm2o,meds_tbm[which(meds_tbm[,"vxint"]==combn2[kj]),])}
for (kj in 1:length(combn2)){tbih2o <- rbind(tbih2o,meds_tbih[which(meds_tbih[,"vxint"]==combn2[kj]),])}
for (kj in 1:length(combn2)){tbmh2o <- rbind(tbmh2o,meds_tbmh[which(meds_tbmh[,"vxint"]==combn2[kj]),])}

psn<-meds_ps[which(meds_ps[,"vxint"]==0),]; psn<-psn[,!(names(psn) %in% "type")]
tbin<-meds_tbi[which(meds_tbi[,"vxint"]==0),];tbin<-tbin[,!(names(tbin) %in% "type")]
tbmn<-meds_tbm[which(meds_tbm[,"vxint"]==0),];tbmn<-tbmn[,!(names(tbmn) %in% "type")]
tbihn<-meds_tbih[which(meds_tbih[,"vxint"]==0),];tbihn<-tbihn[,!(names(tbihn) %in% "type")]
tbmhn<-meds_tbmh[which(meds_tbmh[,"vxint"]==0),];tbmhn<-tbmhn[,!(names(tbmhn) %in% "type")]

## Which type?
for (ty in 1:typen){
  ps2<-ps2o[which(ps2o[,"type"]==ty),]; ps2<-ps2[,!(names(ps2) %in% "type")]
  tbi2<-tbi2o[which(tbi2o[,"type"]==ty),]; tbi2<-tbi2[,!(names(tbi2) %in% "type")]
  tbm2<-tbm2o[which(tbm2o[,"type"]==ty),]; tbm2<-tbm2[,!(names(tbm2) %in% "type")]
  tbih2<-tbih2o[which(tbih2o[,"type"]==ty),];  tbih2<-tbih2[,!(names(tbih2) %in% "type")]
  tbmh2<-tbmh2o[which(tbmh2o[,"type"]==ty),];  tbmh2<-tbmh2[,!(names(tbmh2) %in% "type")]
  ps2<-rbind(ps2,psn);tbi2<-rbind(tbi2,tbin);tbm2<-rbind(tbm2,tbmn);tbih2<-rbind(tbih2,tbihn);tbmh2<-rbind(tbmh2,tbmhn)
  write.csv(ps2,paste(cntry,"_",ty,"_vx_psize.csv",sep=''))
  write.csv(tbi2,paste(cntry,"_",ty,"_vx_tbi.csv",sep=''))
  write.csv(tbm2,paste(cntry,"_",ty,"_vx_tbm.csv",sep=''))
  write.csv(tbih2,paste(cntry,"_",ty,"_vx_tbih.csv",sep=''))
  write.csv(tbmh2,paste(cntry,"_",ty,"_vx_tbmh.csv",sep=''))
  
  ## PLOT
  gg <- ggplot(ps2, aes(x=Year,y=med,group=factor(vxint),ymin=min,ymax=max,fill=factor(vxint))) + scale_y_continuous("Population size") + scale_x_continuous(limits=c(2024,2050),"Year")
  gg <- gg + geom_ribbon(aes(ymin=min, ymax=max),alpha=.25) + geom_line(aes(y=med)) 
  gg <- gg + scale_fill_manual(values=c('grey','red','orange','yellow','green','cyan','blue','pink','purple','brown'))
  if(C==0){gg}
  if(C==1){ggsave(paste(cntry,"_",ty,"_vx_psize.pdf",sep=''))}
  
  gg1 <- ggplot(tbi2, aes(x=Year,y=med,group=factor(vxint),ymin=min,ymax=max,fill=factor(vxint))) + scale_y_continuous("TB incidence \n (cases per 100,000)",lim=c(0,30)) + scale_x_continuous(limits=c(2024,2050),"Year")
  gg1 <- gg1 + geom_ribbon(aes(ymin=min, ymax=max),alpha=.25) + geom_line(aes(y=med)) 
  gg1 <- gg1 + scale_fill_manual(values=c('grey','red','orange','yellow','green','cyan','blue','pink','purple','brown'))
  if(C==0){gg1}
  if(C==1){ggsave(paste(cntry,"_",ty,"_vx_tbi.pdf",sep=''))}
  
  gg2 <- ggplot(tbm2, aes(x=Year,y=med,group=factor(vxint),ymin=min,ymax=max,fill=factor(vxint))) + scale_y_continuous("TB mortality \n (deaths per 100,000)",lim=c(0,1)) + scale_x_continuous(limits=c(2024,2050),"Year")
  gg2 <- gg2 + geom_ribbon(aes(ymin=min, ymax=max),alpha=.25) + geom_line(aes(y=med)) 
  gg2 <- gg2 + scale_fill_manual(values=c('grey','red','orange','yellow','green','cyan','blue','pink','purple','brown'))
  if(C==0){gg2}
  if(C==1){ggsave(paste(cntry,"_",ty,"_vx_tbm.pdf",sep=''))}
  
  gg3 <- ggplot(tbih2, aes(x=Year,y=med,group=factor(vxint),ymin=min,ymax=max,fill=factor(vxint))) + scale_y_continuous("TB incidence in HIV+s \n (cases per 100,000)",lim=c(0,0.3)) + scale_x_continuous(limits=c(2024,2050),"Year")
  gg3 <- gg3 + geom_ribbon(aes(ymin=min, ymax=max),alpha=.25) + geom_line(aes(y=med)) 
  gg3 <- gg3 + scale_fill_manual(values=c('grey','red','orange','yellow','green','cyan','blue','pink','purple','brown'))
  if(C==0){gg3}
  if(C==1){ggsave(paste(cntry,"_",ty,"_vx_tbih.pdf",sep=''))}
  
  gg4 <- ggplot(tbmh2, aes(x=Year,y=med,group=factor(vxint),ymin=min,ymax=max,fill=factor(vxint))) + scale_y_continuous("TB mortality in HIV+s \n (deaths per 100,000)",lim=c(0,0.01)) + scale_x_continuous(limits=c(2024,2050),"Year")
  gg4 <- gg4 + geom_ribbon(aes(ymin=min, ymax=max),alpha=.25) + geom_line(aes(y=med)) 
  gg4 <- gg4 + scale_fill_manual(values=c('grey','red','orange','yellow','green','cyan','blue','pink','purple','brown'))
  if(C==0){gg4}
  if(C==1){ggsave(paste(cntry,"_",ty,"_vx_tbmh.pdf",sep=''))}
  
  # Structure with psize row1, hiv- row2, hiv+ row3
  blankPanel<-grid.rect(gp=gpar(col="white")) 
  pdf(paste(cntry,"_",ty,"_vximpact.pdf",sep=''))
  grid.arrange(gg,blankPanel,gg1,gg2,gg3,gg4,ncol=2)    
  dev.off() 
  
}
