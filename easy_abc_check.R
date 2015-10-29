## the model has two parameters and outputs two summary statistics.
## defining a simple toy model:
toy_model<-function(x){ 
  
  temp <- c(x[1] + x[2])
  outp <- c(0,0)
  
  if( temp >1 & temp <1.5) outp <- c(1,0)  
  return(outp)
}

## define prior information
toy_prior=list(c("unif",0,1),c("normal",1,2))
# a uniform prior distribution between 0 and 1 for parameter 1, and a normal distribution
# of mean 1 and standard deviation of 2 for parameter 2.
## define the targeted summary statistics
sum_stat_obs=c(1.5,0.5)

## to perform the Marjoram et al. (2003)'s method:
##

inipa <- c(1.2,0.4)

ABC_Marjoram_original<-ABC_mcmc(method="Marjoram_original", model=toy_model, prior=toy_prior,
                                summary_stat_target=sum_stat_obs,n_rec=10,init_param = inipa,acceptance=TRUE,rejection=TRUE,verbose=TRUE)
ABC_Marjoram_original
