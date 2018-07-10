#analysis data obtained from move simulations
bar0_climate_bpconstraints = readRDS("bar0_climate_bpconstraints.RData")
bar0_climate_bpnoconstraints = readRDS("bar0_climate_bpnoconstraints.RData")
bar0_climate_middle = readRDS("bar0_climate_middle.RData")
bar0_data10_bpconstraints = readRDS("bar0_data10_bpconstraints.RData")
bar0_data10_bpnoconstraints = readRDS("bar0_data10_bpnoconstraints.RData")
bar0_data10_middle = readRDS("bar0_data10_middle.RData")
bar0_data2_bpconstraints = readRDS("bar0_data2_bpconstraints.RData")
bar0_data2_bpnoconstraints = readRDS("bar0_data2_bpnoconstraints.RData")
bar0_data2_middle = readRDS("bar0_data2_middle.RData")

bar1_climate_bpconstraints = readRDS("bar1_climate_bpconstraints.RData")
bar1_climate_bpnoconstraints = readRDS("bar1_climate_bpnoconstraints.RData")
bar1_climate_middle = readRDS("bar1_climate_middle.RData")
bar1_data10_bpconstraints = readRDS("bar1_data10_bpconstraints.RData")
bar1_data10_bpnoconstraints = readRDS("bar1_data10_bpnoconstraints.RData")
bar1_data10_middle = readRDS("bar1_data10_middle.RData")
bar1_data2_bpconstraints = readRDS("bar1_data2_bpconstraints.RData")
bar1_data2_bpnoconstraints = readRDS("bar1_data2_bpnoconstraints.RData")
bar1_data2_middle = readRDS("bar1_data2_middle.RData")

bar2_climate_bpconstraints = readRDS("bar2_climate_bpconstraints.RData")
bar2_climate_bpnoconstraints = readRDS("bar2_climate_bpnoconstraints.RData")
bar2_climate_middle = readRDS("bar2_climate_middle.RData")
bar2_data10_bpconstraints = readRDS("bar2_data10_bpconstraints.RData")
bar2_data10_bpnoconstraints = readRDS("bar2_data10_bpnoconstraints.RData")
bar2_data10_middle = readRDS("bar2_data10_middle.RData")
bar2_data2_bpconstraints = readRDS("bar2_data2_bpconstraints.RData")
bar2_data2_bpnoconstraints = readRDS("bar2_data2_bpnoconstraints.RData")
bar2_data2_middle = readRDS("bar2_data2_middle.RData")

#---------------------------------MSE and BIC Averages --------------------------------#

#-----------MSE-----
#bar0
mCbar0_bp = mean(bar0_climate_bpconstraints$MSE[c(50000:100000),1])
mCbar0_bpno = mean(bar0_climate_bpnoconstraints$MSE[c(50000:100000),1])
mCbar0_mid = mean(bar0_climate_middle$MSE[c(50000:100000),1])

m10bar0_bp = mean(bar0_data10_bpconstraints$MSE[c(50000:100000),1])
m10bar0_bpno = mean(bar0_data10_bpnoconstraints$MSE[c(50000:100000),1])
m10bar0_mid = mean(bar0_data10_middle$MSE[c(50000:100000),1])

m2bar0_bp = mean(bar0_data2_bpconstraints$MSE[c(50000:100000),1])
m2bar0_bpno = mean(bar0_data2_bpnoconstraints$MSE[c(50000:100000),1])
m2bar0_mid = mean(bar0_data2_middle$MSE[c(50000:100000),1])

sdCbar0_bp = sd(bar0_climate_bpconstraints$MSE[c(50000:100000),1])
sdCbar0_bpno = sd(bar0_climate_bpnoconstraints$MSE[c(50000:100000),1])
sdCbar0_mid = sd(bar0_climate_middle$MSE[c(50000:100000),1])

sd10bar0_bp = sd(bar0_data10_bpconstraints$MSE[c(50000:100000),1])
sd10bar0_bpno = sd(bar0_data10_bpnoconstraints$MSE[c(50000:100000),1])
sd10bar0_mid = sd(bar0_data10_middle$MSE[c(50000:100000),1])

sd2bar0_bp = sd(bar0_data2_bpconstraints$MSE[c(50000:100000),1])
sd2bar0_bpno = sd(bar0_data2_bpnoconstraints$MSE[c(50000:100000),1])
sd2bar0_mid = sd(bar0_data2_middle$MSE[c(50000:100000),1])


#bar1
mCbar1_bp = mean(bar1_climate_bpconstraints$MSE[c(50000:100000),1])
mCbar1_bpno = mean(bar1_climate_bpnoconstraints$MSE[c(50000:100000),1])
mCbar1_mid = mean(bar1_climate_middle$MSE[c(50000:100000),1])

m10bar1_bp = mean(bar1_data10_bpconstraints$MSE[c(50000:100000),1])
m10bar1_bpno = mean(bar1_data10_bpnoconstraints$MSE[c(50000:100000),1])
m10bar1_mid = mean(bar1_data10_middle$MSE[c(50000:100000),1])

m2bar1_bp = mean(bar1_data2_bpconstraints$MSE[c(50000:100000),1])
m2bar1_bpno = mean(bar1_data2_bpnoconstraints$MSE[c(50000:100000),1])
m2bar1_mid = mean(bar1_data2_middle$MSE[c(50000:100000),1])

sdCbar1_bp = sd(bar1_climate_bpconstraints$MSE[c(50000:100000),1])
sdCbar1_bpno = sd(bar1_climate_bpnoconstraints$MSE[c(50000:100000),1])
sdCbar1_mid = sd(bar1_climate_middle$MSE[c(50000:100000),1])

sd10bar1_bp = sd(bar1_data10_bpconstraints$MSE[c(50000:100000),1])
sd10bar1_bpno = sd(bar1_data10_bpnoconstraints$MSE[c(50000:100000),1])
sd10bar1_mid = sd(bar1_data10_middle$MSE[c(50000:100000),1])

sd2bar1_bp = sd(bar1_data2_bpconstraints$MSE[c(50000:100000),1])
sd2bar1_bpno = sd(bar1_data2_bpnoconstraints$MSE[c(50000:100000),1])
sd2bar1_mid = sd(bar1_data2_middle$MSE[c(50000:100000),1])


#bar2
mCbar2_bp = mean(bar2_climate_bpconstraints$MSE[c(50000:100000),1])
mCbar2_bpno = mean(bar2_climate_bpnoconstraints$MSE[c(50000:100000),1])
mCbar2_mid = mean(bar2_climate_middle$MSE[c(50000:100000),1])

m10bar2_bp = mean(bar2_data10_bpconstraints$MSE[c(50000:100000),1])
m10bar2_bpno = mean(bar2_data10_bpnoconstraints$MSE[c(50000:100000),1])
m10bar2_mid = mean(bar2_data10_middle$MSE[c(50000:100000),1])

m2bar2_bp = mean(bar2_data2_bpconstraints$MSE[c(50000:100000),1])
m2bar2_bpno = mean(bar2_data2_bpnoconstraints$MSE[c(50000:100000),1])
m2bar2_mid = mean(bar2_data2_middle$MSE[c(50000:100000),1])

sdCbar2_bp = sd(bar2_climate_bpconstraints$MSE[c(50000:100000),1])
sdCbar2_bpno = sd(bar2_climate_bpnoconstraints$MSE[c(50000:100000),1])
sdCbar2_mid = sd(bar2_climate_middle$MSE[c(50000:100000),1])

sd10bar2_bp = sd(bar2_data10_bpconstraints$MSE[c(50000:100000),1])
sd10bar2_bpno = sd(bar2_data10_bpnoconstraints$MSE[c(50000:100000),1])
sd10bar2_mid = sd(bar2_data10_middle$MSE[c(50000:100000),1])

sd2bar2_bp = sd(bar2_data2_bpconstraints$MSE[c(50000:100000),1])
sd2bar2_bpno = sd(bar2_data2_bpnoconstraints$MSE[c(50000:100000),1])
sd2bar2_mid = sd(bar2_data2_middle$MSE[c(50000:100000),1])


#------------end MSE-----#

m_0_bp = c(mCbar0_bp, m10bar0_bp, m2bar0_bp)
m_0_bpno = c(mCbar0_bpno, m10bar0_bpno, m2bar0_bpno)
m_0_mid = c(mCbar0_mid, m10bar0_mid, m2bar0_mid)

sd_0_bp = c(sdCbar0_bp, sd10bar0_bp, sd2bar0_bp)
sd_0_bpno = c(sdCbar0_bpno, sd10bar0_bpno, sd2bar0_bpno)
sd_0_mid = c(sdCbar0_mid, sd10bar0_mid, sd2bar0_mid)

m_1_bp = c(mCbar1_bp, m10bar1_bp, m2bar1_bp)
m_1_bpno = c(mCbar1_bpno, m10bar1_bpno, m2bar1_bpno)
m_1_mid = c(mCbar1_mid, m10bar1_mid, m2bar1_mid)

sd_1_bp = c(sdCbar1_bp, sd10bar1_bp, sd2bar1_bp)
sd_1_bpno = c(sdCbar1_bpno, sd10bar1_bpno, sd2bar1_bpno)
sd_1_mid = c(sdCbar1_mid, sd10bar1_mid, sd2bar1_mid)

m_2_bp = c(mCbar2_bp, m10bar2_bp, m2bar2_bp)
m_2_bpno = c(mCbar2_bpno, m10bar2_bpno, m2bar2_bpno)
m_2_mid = c(mCbar2_mid, m10bar2_mid, m2bar2_mid)

sd_2_bp = c(sdCbar2_bp, sd10bar2_bp, sd2bar2_bp)
sd_2_bpno = c(sdCbar2_bpno, sd10bar2_bpno, sd2bar2_bpno)
sd_2_mid = c(sdCbar2_mid, sd10bar2_mid, sd2bar2_mid)


bCbar0 = mean(colMeans(bar0_climate$BIC))
bCbar1 = mean(colMeans(bar1_climate$BIC))
bCbar2 = mean(colMeans(bar2_climate$BIC))

bCbar0 = mean(apply(bar0_climate$BIC,1,sd))
bCbar1 = mean(apply(bar1_climate$BIC,1,sd))
bCbar2 = mean(apply(bar2_climate$BIC,1,sd))

#all MSE
x=c(1:3)
plot(x, m_0_bp, xlab="DataSet Number", ylab="Average MSE", main="Average MSE for Climate Data", col="red", ylim=c(0,10),  cex = 1, pch=16)
#lines(c(x,x),c(m_0_bp-sd_0_bp*2, m_0_bp+sd_0_bp*2), col="red",lty=1)
points(x, m_1_bp, col="purple", cex = 1, pch=16)
#lines(c(1,1),c(mCbar0_bpno-sdCbar0_bpno*2,mCbar0_bpno+sdCbar0_bpno*2), col="purple",lty=1)
points(x, m_2_bp, col="blue", cex = 1, pch=16)
#lines(c(2,2),c(mCbar0_mid-sdCbar0_mid*2,mCbar0_mid+sdCbar0_mid*2), col="blue",lty=1)


#BIC
plot(0, mBICCbar0, xlab="DataSet Number", ylab="Average BIC", main="Average BIC for Climate Data", col="red", ylim=c(-450,0), xlim=c(0,2), cex = 1, pch=16)
lines(c(0,0),c(mBICCbar0-sdBICCbar0*2,mBICCbar0+sdBICCbar0*2), col="red",lty=1)
points(1, mBICCbar1, col="purple", cex = 1, pch=16)
lines(c(1,1),c(mBICCbar1-sdBICCbar1*2,mBICCbar1+sdBICCbar1*2), col="purple",lty=1)
points(2, mBICCbar2, col="blue", cex = 1, pch=16)
lines(c(2,2),c(mBICCbar2-sdBICCbar2*2,mBICCbar2+sdBICCbar2*2), col="blue",lty=1)






m_C_bp = c(mCbar0_bp, mCbar1_bp, mCbar2_bp)
m_C_bpno = c(mCbar0_bpno, mCbar1_bpno, mCbar2_bpno)
m_C_mid = c(mCbar0_mid, mCbar1_mid, mCbar2_mid)

sd_C_bp = c(sdCbar0_bp, sdCbar1_bp, sdCbar2_bp)
sd_C_bpno = c(sdCbar0_bpno, sdCbar1_bpno, sdCbar2_bpno)
sd_C_mid = c(sdCbar0_mid, sdCbar1_mid, sdCbar2_mid)

m_10_bp = c(m10bar0_bp, m10bar1_bp, m10bar2_bp)
m_10_bpno = c(m10bar0_bpno, m10bar1_bpno, m10bar2_bpno)
m_10_mid = c(m10bar0_mid, m10bar1_mid, m10bar2_mid)

sd_10_bp = c(sdCbar1_bp, sd10bar1_bp, sd2bar1_bp)
sd_10_bpno = c(sdCbar1_bpno, sd10bar1_bpno, sd2bar1_bpno)
sd_10_mid = c(sdCbar1_mid, sd10bar1_mid, sd2bar1_mid)

m_2_bp = c(m2bar0_bp, m2bar1_bp, m2bar2_bp)
m_2_bpno = c(m2bar0_bpno, m2bar1_bpno, m2bar2_bpno)
m_2_mid = c(m2bar0_mid, m2bar1_mid, m2bar2_mid)

sd_2_bp = c(sd2bar0_bp, sd2bar1_bp, sd2bar2_bp)
sd_2_bpno = c(sd2bar0_bpno, sd2bar1_bpno, sd2bar2_bpno)
sd_2_mid = c(sd2bar0_mid, sd2bar1_mid, sd2bar2_mid)

#Climate MSE
x=c(0:2)
plot(x, m_C_bp, xlab="BAR Number", ylab="Average MSE", main="Average MSE for Climate Data", col="red", ylim=c(0,.02),  cex = 1.5, pch=16)
points(x, m_C_bp+sd_C_bp*2, col="red", cex = 1, pch=16)
points(x, m_C_bp-sd_C_bp*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_C_bp[i+1]-sd_C_bp[i+1]*2,m_C_bp[i+1]+sd_C_bp[i+1]*2), col="red",lty=2)
}
points(x, m_C_bpno, col="purple", cex = 1.5, pch=16)
points(x, m_C_bpno+sd_C_bpno*2, col="purple", cex = 1, pch=16)
points(x, m_C_bpno-sd_C_bpno*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_C_bpno[i+1]-sd_C_bpno[i+1]*2,m_C_bpno[i+1]+sd_C_bpno[i+1]*2), col="purple",lty=2)
}

points(x, m_C_mid, col="blue", cex = 1.5, pch=16)
points(x, m_C_mid+sd_C_mid*2, col="blue", cex = 1, pch=16)
points(x, m_C_mid-sd_C_mid*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_C_mid[i+1]-sd_C_mid[i+1]*2,m_C_mid[i+1]+sd_C_mid[i+1]*2), col="blue",lty=2)
}

legend(1, .005, legend=c("Bai-Perron Constrains","Bai-Perron No Constrains","Middle"),
       col=c("red" ,"purple", "blue"), lty=1:1, cex=.5)


#data10 MSE
x=c(0:2)
plot(x, m_10_bp, xlab="BAR Number", ylab="Average MSE", main="Average MSE for Data 10", col="red", ylim=c(5,15),  cex = 1.5, pch=16)
points(x, m_10_bp+sd_10_bp*2, col="red", cex = 1, pch=16)
points(x, m_10_bp-sd_10_bp*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_10_bp[i+1]-sd_10_bp[i+1]*2,m_10_bp[i+1]+sd_10_bp[i+1]*2), col="red",lty=2)
}
points(x, m_10_bpno, col="purple", cex = 1.5, pch=16)
points(x, m_10_bpno+sd_10_bpno*2, col="purple", cex = 1, pch=16)
points(x, m_10_bpno-sd_10_bpno*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_10_bpno[i+1]-sd_10_bpno[i+1]*2,m_10_bpno[i+1]+sd_10_bpno[i+1]*2), col="purple",lty=2)
}

points(x, m_10_mid, col="blue", cex = 1.5, pch=16)
points(x, m_10_mid+sd_10_mid*2, col="blue", cex = 1, pch=16)
points(x, m_10_mid-sd_10_mid*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_10_mid[i+1]-sd_10_mid[i+1]*2,m_10_mid[i+1]+sd_10_mid[i+1]*2), col="blue",lty=2)
}

legend(1.5, 7, legend=c("Bai-Perron Constrains","Bai-Perron No Constrains","Middle"),
       col=c("red" ,"purple", "blue"), lty=1:1, cex=.5)


#Data 2
x=c(0:2)
plot(x, m_2_bp, xlab="BAR Number", ylab="Average MSE", main="Average MSE for Data2", col="red", ylim=c(0,2),  cex = 1.5, pch=16)
points(x, m_2_bp+sd_2_bp*2, col="red", cex = 1, pch=16)
points(x, m_2_bp-sd_2_bp*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_2_bp[i+1]-sd_2_bp[i+1]*2,m_2_bp[i+1]+sd_2_bp[i+1]*2), col="red",lty=2)
}
points(x, m_2_bpno, col="purple", cex = 1.5, pch=16)
points(x, m_2_bpno+sd_2_bpno*2, col="purple", cex = 1, pch=16)
points(x, m_2_bpno-sd_2_bpno*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_2_bpno[i+1]-sd_2_bpno[i+1]*2,m_2_bpno[i+1]+sd_2_bpno[i+1]*2), col="purple",lty=2)
}

points(x, m_2_mid, col="blue", cex = 1.5, pch=16)
points(x, m_2_mid+sd_2_mid*2, col="blue", cex = 1, pch=16)
points(x, m_2_mid-sd_2_mid*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(m_2_mid[i+1]-sd_2_mid[i+1]*2,m_2_mid[i+1]+sd_2_mid[i+1]*2), col="blue",lty=2)
}

legend(1, .5, legend=c("Bai-Perron Constrains","Bai-Perron No Constrains","Middle"),
       col=c("red" ,"purple", "blue"), lty=1:1, cex=.5)

