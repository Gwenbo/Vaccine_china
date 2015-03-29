#### checking VE estimates

PGchkold<-matrix(0,)

PGchkold<-sum(new_actv[251:302,56:75])
PGchkvold<-sum(new_actv[251:302,56:Mnage])
PGchkall<-sum(new_actv[251:302,])
PGchk25<- (PGchkvold/PGchkall)*100
PGchkold
PGchkvold
PGchkall
PGchk25

PGchkold<-sum(new_actv[221,56:75])
PGchkvold<-sum(new_actv[221,66:Mnage])
PGchkall<-sum(new_actv[221,])
PGchk10<- (PGchkvold/PGchkall)*100
PGchkold
PGchkvold
PGchkall
PGchk10

PGchkold<-sum(new_actv[301,56:75])
PGchkvold<-sum(new_actv[301,56:Mnage])
PGchkall<-sum(new_actv[301,])
PGchk50<- (PGchkvold/PGchkall)*100
PGchkold
PGchkvold
PGchkall
PGchk50

popsizeold<-sum(psize55plus[221:222])
popsizevold<-sum(psize65plus[221:222])
popsize5574<-sum(psize5574[221:222])
popsize75plus<-sum(psize75plus[221:222])
popsizeall<-sum(psize[221:222])
popchk2010<- (popsize75plus/popsizeall)*100
popchk2010

popsizeold<-sum(psize55plus[251:252])
popsizevold<-sum(psize65plus[251:252])
popsize5574<-sum(psize5574[251:252])
popsize75plus<-sum(psize75plus[251:252])
popsizeall<-sum(psize[251:252])
popchk2025<- (popsize75plus/popsizeall)*100
popchk2025


popsizeold<-sum(psize55plus[301:302])
popsizevold<-sum(psize65plus[301:302])
popsize5574<-sum(psize5574[301:302])
popsize75plus<-sum(psize75plus[301:302])
popsizeall<-sum(psize[301:302])
popchk2050<- (popsize75plus/popsizeall)*100
popchk2050

popsizeold<-sum(psize70plus[301:302])
popsizevold<-sum(psize65plus[301:302])
popsizeall<-sum(psize[301:302])
popchk2050<- (popsizeold/popsizeall)*100
popchk2050

popchk2010
popchk2025
popchk2050


PGchkold<-sum(new_actv[251:302,56:75])
PGchkall<-sum(new_actv[251:302,])
PGchk<- (PGchkold/PGchkall)*100
PGchk


PGchkold<-sum(new_actv[221:252,76:Mnage])
PGchkall<-sum(new_actv[221:252,])
PGchk<- (PGchkold/PGchkall)*100
PGchk

PcPop<-matrix(0,2,6)
popsizeall<-sum(psize[301:302])
casesall<-sum(new_actv[301:302,])
PcPop[1,1]<-sum(psize014[301:302])/popsizeall*100
PcPop[1,2]<-sum(psize1524[301:302])/popsizeall*100
PcPop[1,3]<-sum(psize2554[301:302])/popsizeall*100
PcPop[1,4]<-sum(psize5564[301:302])/popsizeall*100
PcPop[1,5]<-((sum(psize5574[301:302]))-(sum(psize5564[301:302])))/popsizeall*100
PcPop[1,6]<-sum(psize75plus[301:302])/popsizeall*100


PcPop[2,1]<-sum(new_actv[301:302,1:15])/casesall*100
PcPop[2,2]<-sum(new_actv[301:302,16:25])/casesall*100
PcPop[2,3]<-sum(new_actv[301:302,26:55])/casesall*100
PcPop[2,4]<-sum(new_actv[301:302,56:65])/casesall*100
PcPop[2,5]<-sum(new_actv[301:302,66:75])/casesall*100
PcPop[2,6]<-sum(new_actv[301:302,76:Mnage])/casesall*100



LTBchkold<-sum(new_actv[221:252,76:Mnage])
LTBchkall<-sum(new_actv[221:252,])
PGchk<- (PGchkold/PGchkall)*100
PGchk
