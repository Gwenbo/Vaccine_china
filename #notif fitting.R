## notifications plot



par(mfrow=c(2,3))
plot(seq(1990,2050),TBN[91:151,1], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,600),type='l',col='orange')
#points(2010,notif2010[1])
segments(2010,notif2010l[1],2010,notif2010u[1])
plot(seq(1990,2050),TBN[91:151,2], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,600),type='l',col='red')
#points(2010,notif2010[2])
segments(2010,notif2010l[2],2010,notif2010u[2])
plot(seq(1990,2050),TBN[91:151,3], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,600),type='l',col='blue')
#points(2010,notif2010[3])
segments(2010,notif2010l[3],2010,notif2010u[3])
plot(seq(1990,2050),TBN[91:151,4], ylab="p=0.18 and 0.25 Notifications/100,000pop/yr",xlab="Year", ylim=c(0,600),type='l',col='purple')
#points(2010,notif2010[4])
segments(2010,notif2010l[4],2010,notif2010u[4])
plot(seq(1990,2050),TBN[91:151,5], ylab="Notifications/100,000pop/yr",xlab="Year", ylim=c(0,600),type='l',col='green')
#points(2010,notif2010[5])
segments(2010,notif2010l[5],2010,notif2010u[5])
plot.new()
legend("center",c("Overall","0-14 years","15-54 years","55-64 years","≥65 years"), lty=1,col=c("orange","red","blue","purple","green"))

