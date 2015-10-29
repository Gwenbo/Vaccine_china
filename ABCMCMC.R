###  ###

library(EasyABC)

C=0
if (C == 0){home<-"/Users/Rebecca/Vaccine_china"}
if (C == 1){home<-"/home/lsh355020/China/"}
setwd(home)

#prepare function for calling model
source('MCMCmodel.R')


#vector of parameter ranges (priors)
if (C == 0){input<-"/Users/Rebecca/Vaccine_china/Data"}
if (C == 1){input<-"/home/lsh355020/China/Data"}
setwd(input)

pararange <- as.matrix(drop.levels(read.csv('pararanges.csv',header=TRUE,check.names=F)))       # Number of parameters same for all countries - CHECK with new model
setwd(home)

pararange<-cbind(as.numeric(as.character(pararange[,2])),as.numeric(as.character(pararange[,3])))

paraprior<-list(c("unif",pararange[1,1],pararange[1,2]),
                c("unif",pararange[2,1],pararange[2,2]),
                c("unif",pararange[3,1],pararange[3,2]),
                c("unif",pararange[4,1],pararange[4,2]),
                c("unif",pararange[5,1],pararange[5,2]),
                c("unif",pararange[6,1],pararange[6,2]),
                c("unif",pararange[7,1],pararange[7,2]),
                c("unif",pararange[8,1],pararange[8,2]),
                c("unif",pararange[9,1],pararange[9,2]),
                c("unif",pararange[10,1],pararange[10,2]),
                c("unif",pararange[11,1],pararange[11,2]),
                c("unif",pararange[12,1],pararange[12,2]),
                c("unif",pararange[13,1],pararange[13,2]),
                c("unif",pararange[14,1],pararange[14,2]),
                c("unif",pararange[15,1],pararange[15,2]),
                c("unif",pararange[16,1],pararange[16,2]),
                c("unif",pararange[17,1],pararange[17,2]),
                c("unif",pararange[18,1],pararange[18,2]),
                c("unif",pararange[19,1],pararange[19,2]),
                c("unif",pararange[20,1],pararange[20,2]),
                c("unif",pararange[21,1],pararange[21,2]),
                c("unif",pararange[22,1],pararange[22,2]),
                c("unif",pararange[23,1],pararange[23,2]),
                c("unif",pararange[24,1],pararange[24,2]),
                c("unif",pararange[25,1],pararange[25,2]),
                c("unif",pararange[26,1],pararange[26,2]),
                c("unif",1267142,1267142))


## target summary stat
sum_stat_fits<-c(10)

#prior test for setting constraints on parameters
conprior<-c("X9>=X10","X11>=X21","X12>=X23","X13>=X14","X16>=X17","X18<=X22","X19>=X26","X23<=X24")

    
## to perform the Marjoram et al. (2003)'s method:
## NEEDS TO BE BEST FITS CALLED IN FROM PARA

inipa <- c(-0.6,-0.933087394,0.092287611,0.134356489,0.000126302,0.014923374,0.017959284,0.037839335,0.328241145,0.182054961,0.187302247,1.354826963,0.026351182,0.00865935,0.316065403,0.669741324,0.232510374,0.000194231,0.136385085,0.559122341,0.100871144,0.003217605,0.637493554,1.077768741,1.449519268,-0.200340127,1267142)
#inipa<-

para<-c()
#run ABC MCMC
ABCmcmc<-ABC_mcmc(method="Marjoram_original", model=MCMCmodel, prior=paraprior, summary_stat_target=sum_stat_fits, n_rec=10,
                  init_param=inipa,acceptance=TRUE,rejection=TRUE,verbose=TRUE)

#n_between sampling?

ABCmcmc

ABCmcmc<-ABC_mcmc(method="Marjoram_original", model=MCMCmodel, prior=paraprior, summary_stat_target=sum_stat_fits, prior_test=conprior, n_rec=10,
                  init_param=inipa,acceptance=TRUE,rejection=TRUE,verbose=TRUE)



