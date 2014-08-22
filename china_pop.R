### Life expectancy check ####


setwd("/Users/Rebecca/Documents/PhD/Model_Materials/CEmodel_Rebecca/China Model")
#setwd("/Users/Karen/My Documents/LSHTM/China_pop")

## reads in data
cpop<- read.csv("chinapop.csv", header=TRUE)
initialpop<- read.csv("Chinapop2010.csv", header=TRUE)
pop_med_fert<- read.csv("china_med_fert.csv", header=FALSE)

LE<- read.csv("LEtables.csv", header=TRUE)

#print (cpop)

#read in yr as vector from cpop
year<- cpop[,1]

#read in birth rate as vector from cpop 
#for now doing it with birth number, but could do rate*popsize if desired using Brate<- cpop[,3]
B<- cpop[,2]



#read in death rate/age/yr
u<-LE[,2:102]

#Life expectancy
LE<-cpop[,6]
#read in death rate/yr
u<-1/LE


#put maxage in file higher than 100??
maxage<-length(initialpop[,1])

#set up initial age structure in 2010
initpop<-initialpop[,2]
#print (initpop)

A<-matrix(0,length(year),(maxage))
rownames(A)<-year
colnames(A)<-initialpop[,1]
#print (A)

poptotals<- matrix(0,length(year),5)
rownames(poptotals)<-year
poplist<- as.list(c("totpop","pop014","pop1554","pop5564","pop65plus"))
colnames(poptotals)<-poplist


#loop through years
for (k in 1:length(year))
    { #print(k)
      if (k==1) 
      {
       A[k,]<-initpop
       totpop<-sum(A[k,1:maxage])
       pop014<-sum(A[k,1:15])
       pop1554<-sum(A[k,16:55])
       pop5564<-sum(A[k,56:65])
       pop65plus<- sum(A[k,66:maxage])
      }
      else
      {
      #set total pop to zero to start count fresh that year
       totpop<-0
       pop014<-0
       pop1554<-0
       pop5564<-0
       pop65plus<-0
       #loop ages, age 1 is the first year of life, so age 1=zero
       for (j in 1:maxage)
       {
        if (j==1) 
        {
          A[k,j]<-B[k]/5
        }
        else
        {

          A[k,j]<-A[k-1,j-1] - u[k-1,j-1]*A[k-1,j-1]

          A[k,j]<-A[k-1,j-1] - u[k-1]*A[k-1,j-1]

          # if decide to vary risk of death by age, will need to set up u as a 2D marix of yr and age A[k,j]<-A[k-1,k-1] - u[k-1,j-1]*A[k-1,j-1]
        }
        #print (A[k,j])
        
        #generate summary populations
        totpop<- totpop + A[k,j]
        
        #print age by age group. Age is 1 yr less than j
        if (j>=1 & j<16)
        {
          pop014<- pop014 + A[k,j]
        }
        else
        { 
          if (j>=16 & j<56)
          {
          pop1554<- pop1554 + A[k,j]
          }
          else
          { 
            if (j>=56 & j<66)
            {
              pop5564<- pop5564 + A[k,j]
            }
            else
            {
              pop65plus<- pop65plus + A[k,j]
            }
          }
        }
        #end of age j loop on next line
       }
      
      #end of yearly else loop on next line
      }
      
      poptotals[k,1]<-totpop
      poptotals[k,2]<-pop014
      poptotals[k,3]<-pop1554
      poptotals[k,4]<-pop5564
      poptotals[k,5]<-pop65plus
      
      #end of year K loop next line
    }
    
#print(A) 
#print(poptotals)

### plots

#barplot(poptotals, main="plot of China population", xlab="year", ylab="Population(thousands)")

##import 2050 estimates and plot against model for age groups
population2050<- c(1384976,204187,623982,225492,331315)
pop2050<-matrix(0,2,5)
colnames(pop2050)<-poplist
pop2050[1,]<-population2050
pop2050[2,]<-poptotals[41,]
h<-barplot(pop2050[2,], main="2050 plot of China population", xlab="popgrp", ylab="Population(thousands)",ylim=c(0,max(pop2050)))
points(h,pop2050[1,],col="red")

# Import total population over time (medium fertility estimates) and plot against model
plot(poptotals[,1],ylim=c(0,1500000)) # Model
points(pop_med_fert[,2],col="red")    # Estimate

# Check births are per 5 year period - plot birth rate*pop against number births/5
plot(cpop[,3]*pop_med_fert[,2]/1000)
points(cpop[,2]/5,col="red")
