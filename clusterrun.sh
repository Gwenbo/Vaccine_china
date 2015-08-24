
#!/bin/bash #!!
#$ -N chinavac
#$ -cwd -V
#$ -q short.q
#$ -l mem_free=1G,h_vmem=1.2G
#$ -M rebecca.harris@lshtm.ac.uk -m eas
#$ -t 1-10

R CMD BATCH cluster_china.R out_china${SGE_TASK_ID}.out

