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
sum_stat_fits<-c(1,0)


#propsal range
#prop_ran<-rep(1/50,times=27)
  
propfrac<-1/10
prop_ran<-c(0,((pararange[2,2]-pararange[2,1])*propfrac),((pararange[3,2]-pararange[3,1])*propfrac),
            ((pararange[4,2]-pararange[4,1])*propfrac),((pararange[5,2]-pararange[5,1])*propfrac),
            ((pararange[6,2]-pararange[6,1])*propfrac),((pararange[7,2]-pararange[7,1])*propfrac),
            ((pararange[8,2]-pararange[8,1])*propfrac),((pararange[9,2]-pararange[9,1])*propfrac),
            ((pararange[10,2]-pararange[10,1])*propfrac),((pararange[11,2]-pararange[11,1])*propfrac),
            ((pararange[12,2]-pararange[12,1])*propfrac),((pararange[13,2]-pararange[13,1])*propfrac),
            ((pararange[14,2]-pararange[14,1])*propfrac),((pararange[15,2]-pararange[15,1])*propfrac),
            ((pararange[16,2]-pararange[16,1])*propfrac),((pararange[17,2]-pararange[17,1])*propfrac),
            ((pararange[18,2]-pararange[18,1])*propfrac),((pararange[19,2]-pararange[19,1])*propfrac),
            ((pararange[20,2]-pararange[20,1])*propfrac),((pararange[21,2]-pararange[21,1])*propfrac),
            ((pararange[22,2]-pararange[22,1])*propfrac),((pararange[23,2]-pararange[23,1])*propfrac),
            ((pararange[24,2]-pararange[24,1])*propfrac),((pararange[25,2]-pararange[25,1])*propfrac),
            ((pararange[26,2]-pararange[26,1])*propfrac),0)




#prior test for setting constraints on parameters - only works with original ABC, doesnt work with GWeen's adapted one, so have adjected code accordingly to only keep parameter sets where the contstraints are correct
#conprior<-c("X9>=X10","X11>=X21","X12>=X23","X13>=X14","X16>=X17","X18<=X22","X19>=X26","X23<=X24")

    
## to perform the Marjoram et al. (2003)'s method:
## NEEDS TO BE BEST FITS CALLED IN FROM PARA

#inipa <- c(-0.6,-0.933087394,0.092287611,0.134356489,0.000126302,0.014923374,0.017959284,0.037839335,0.328241145,0.182054961,0.187302247,1.354826963,0.026351182,0.00865935,0.316065403,0.669741324,0.232510374,0.000194231,0.136385085,0.559122341,0.100871144,0.003217605,0.637493554,1.077768741,1.449519268,-0.200340127,1267142)
#highest likelihood from 1m runs
inipa<-c(-0.6,-0.9330874,0.09228761,0.1343565,0.000126302,0.01492337,0.01795928,0.03783934,0.3282411,0.182055,0.1873022,1.354827,0.02635118,0.00865935,0.3160654,0.6697413,0.2325104,0.0001942307,0.1363851,0.5591223,0.1008711,0.003217605,0.6374936,1.077769,1.449519,-0.2003401,1267142)
#17 hits with 2xinterval
#inipa<-c(-0.6,-0.8921516,0.6611476,0.04718674,0.0001158439,0.01396382,0.0109124,0.02799521,0.2153441,0.1250157,0.2189921,0.7649427,0.03334279,0.01440258,0.3315732,0.7298556,0.5916263,0.0002567397,0.4046917,0.5983042,0.1567878,0.00122059,0.5006388,1.347135,3.147022,-0.0306137,1267142)
#13hits with 1xinterval
#inipa<-c(-0.6,-0.9474576,1.617712,0.1111202,0.0001461118,0.007026255,0.01347849,0.0565967,0.2614424,0.1251389,0.2408055,1.047313,0.03484524,0.006517224,0.3110319,0.5573396,0.2469189,0.0002105262,-0.07236937,0.7671593,0.1799104,0.004130687,0.8494374,1.253521,3.133438,-0.0927992,1267142)
#
# ddd<-c(-0.04927872,0.02149893,0.08008683,0.0002023285,0.01434333,0.01726759,0.03159842,0.3522841,0.177312,0.1918502,1.135914,0.1835771,0.007797707,0.264082,0.3335813,0.2056596,0.0002236011,-0.1316982,0.4967388,0.1515243,0.01301079,0.2604056,1.367018,3.932684,-0.1458154)
#  ddd<-ddd*0.95
# inipa<-c(-0.6,ddd,1267142)


para<-c()
hittrack<-c()
samppoint<-1000
hits<-10

#run ABC MCMC
ABCmcmc<-ABC_mcmc(method="Marjoram_original", model=MCMCmodel, prior=paraprior, summary_stat_target=sum_stat_fits, n_rec=samppoint, n_between_sampling=1, proposal_range=prop_ran,
                  init_param=inipa,acceptance=TRUE,rejection=TRUE,verbose=TRUE)

#n_between sampling?


ABCmcmc
hittrack

#count number not meeting parameter criteria
count99<-count(hittrack==99)
critpc<-count99[2,2]/samppoint*100
critpc

#count number not meeting criteria
countacc<-count(hittrack>=hits & hittrack!=99)
acceptpc<-countacc[2,2]/samppoint*100

steptest<-

mcplot2 <- as.mcmc(ABCmcmc$param)
plot(mcplot2)



#marjoram original so can have calibration????

n_calib<-10
#tol_quant=0.2 ##from example, no idea if ok???
ABCmcmc<-ABC_mcmc(method="Marjoram", model=MCMCmodel, prior=paraprior, summary_stat_target=sum_stat_fits, n_rec=10, n_between_sampling=1, proposal_range=prop_ran,
                  init_param=inipa,acceptance=TRUE,rejection=TRUE,verbose=TRUE, n_calibration=n_calib, tolerance_quantile=tol_quant, use_seed=TRUE)

para<-c()
hittrack<-c()
ABCmcmc_marj<-c()
ABCmcmc_marj<-ABC_mcmc(method="Marjoram", model=MCMCmodel, prior=paraprior, summary_stat_target=sum_stat_fits, n_rec=10, n_between_sampling=5, n_calibration=n_calib)

#accessing array outside of boundary??
## doesnt like proposal range or init_param?!
##not sure if want to add this into marj?
use_seed=TRUE
tolerance_quantile=tol_quant
verbose=TRUE

ABCmcmc_marj
hittrack
hittrack_marj<-hittrack

# #this one was for those with contratints on prior, but doenst work in easyABC version with init_para
# ABCmcmc<-ABC_mcmc(method="Marjoram_original", model=MCMCmodel, prior=paraprior, summary_stat_target=sum_stat_fits, prior_test=conprior, n_rec=10,
#                   init_param=inipa,acceptance=TRUE,rejection=TRUE,verbose=TRUE)
# 
# 
#To perform the algorithm of Marjoram et al. (2003) in which some of the arguments (dist max, tab normalization and proposal range) are automatically determined by the algorithm via an ini- tial calibration step, one needs to specify three arguments: the number n calibration of simulations to perform at the calibration step, the tolerance quantile tolerance quantile to be used for the de- termination of dist max and the scale factor proposal phi to determine the proposal range. These modifications are drawn from the algorithm of Wegmann et al. (2009a), without relying on PLS re- gressions. The arguments are set by default to: n calibration = 10000, tolerance quantile = 0.01 and proposal phi = 1. This way of automatic determination of dist max, tab normalization and proposal range is strongly recommended, compared to the crude automatic determination proposed in the method Marjoram_original.
# 
