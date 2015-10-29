### Add the job number on to parameter and change set to fit ###
library(ggplot2)
library(data.table)
suppressWarnings(library('gdata')) ## For drop.levels function

setwd(clusteroutput)
numjobs
for (uu in 1801:2300){
  print(uu)
  para<-fread(paste("paraout_China_",uu,".csv",sep=''))
  job<-rep(uu,1000)
  para<-cbind(job,para)
  colnames(para)[colnames(para) == "set"]<-"fit"
  write.table(para,paste("paraout_China_",uu,".csv",sep=''),sep=",",row.names=FALSE)
}