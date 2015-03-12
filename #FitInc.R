### Code to calculate incidence to calibrate to using model-generated CDR

#calc of average CDR per age group
avCDR <- sum(CDR2010)/length(CDR2010)
avCDR014 <-sum(CDR2010[1:15])/length(CDR2010[1:15])
avCDR1554 <- sum(CDR2010[16:55])/length(CDR2010[16:55])
avCDR5564 <- sum(CDR2010[56:65])/length(CDR2010[56:65])
avCDR65plus <- sum(CDR2010[66:Mnage])/length(CDR2010[66:Mnage])

#calc of notification given the model CDR
FitI <- #(read in notifications)*avCDR
FitI014 <- #(read in notifications)*avCDR014
FitI1554 <- #(read in notifications)*avCDR1554
FitI5564 <- #(read in notifications)*avCDR5564
FitI65plus <- #(read in notifications)*avCDR65plus