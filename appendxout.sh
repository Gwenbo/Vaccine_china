
#!/bin/bash
#$ -N chinaappend
#$ -cwd -V
#$ -q short.q
#$ -l mem_free=6G,h_vmem=6.2G
#$ -M rebecca.harris@lshtm.ac.uk -m eas
#$ -t 1-1

R CMD BATCH /home/lsh355020/China/clusterappend.R appendxout_china${SGE_TASK_ID}.out

