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
mCbar0_bp = mean(bar0_climate_bpconstraints$MSE[,1])
mCbar0_bpno = mean(bar0_climate_bpnoconstraints$MSE[,1])
mCbar0_mid = mean(bar0_climate_middle$MSE[,1])
                  
m10bar0_bp = mean(bar0_data10_bpconstraints$MSE[,1])
m10bar0_bpno = mean(bar0_data10_bpnoconstraints$MSE[,1])
m10bar0_mid = mean(bar0_data10_middle$MSE[,1])

m2bar0_bp = mean(bar0_data2_bpconstraints$MSE[,1])
m2bar0_bpno = mean(bar0_data2_bpnoconstraints$MSE[,1])
m2bar0_mid = mean(bar0_data2_middle$MSE[,1])

sdCbar0_bp = sd(bar0_climate_bpconstraints$MSE)
sdCbar0_bpno = sd(bar0_climate_bpnoconstraints$MSE[,1])
sdCbar0_mid = sd(bar0_climate_middle$MSE[,1])

sd10bar0_bp = sd(bar0_data10_bpconstraints$MSE[,1])
sd10bar0_bpno = sd(bar0_data10_bpnoconstraints$MSE[,1])
sd10bar0_mid = sd(bar0_data10_middle$MSE[,1])

sd2bar0_bp = sd(bar0_data2_bpconstraints$MSE[,1])
sd2bar0_bpno = sd(bar0_data2_bpnoconstraints$MSE[,1])
sd2bar0_mid = sd(bar0_data2_middle$MSE[,1])


#bar1
mCbar1_bp = mean(bar1_climate_bpconstraints$MSE[,1])
mCbar1_bpno = mean(bar1_climate_bpnoconstraints$MSE[,1])
mCbar1_mid = mean(bar1_climate_middle$MSE[,1])

m10bar1_bp = mean(bar1_data10_bpconstraints$MSE[,1])
m10bar1_bpno = mean(bar1_data10_bpnoconstraints$MSE[,1])
m10bar1_mid = mean(bar1_data10_middle$MSE[,1])

m2bar1_bp = mean(bar1_data2_bpconstraints$MSE[,1])
m2bar1_bpno = mean(bar1_data2_bpnoconstraints$MSE[,1])
m2bar1_mid = mean(bar1_data2_middle$MSE[,1])

sdCbar1_bp = sd(bar1_climate_bpconstraints$MSE[,1])
sdCbar1_bpno = sd(bar1_climate_bpnoconstraints$MSE[,1])
sdCbar1_mid = sd(bar1_climate_middle$MSE[,1])

sd10bar1_bp = sd(bar1_data10_bpconstraints$MSE[,1])
sd10bar1_bpno = sd(bar1_data10_bpnoconstraints$MSE[,1])
sd10bar1_mid = sd(bar1_data10_middle$MSE[,1])

sd2bar1_bp = sd(bar1_data2_bpconstraints$MSE[,1])
sd2bar1_bpno = sd(bar1_data2_bpnoconstraints$MSE[,1])
sd2bar1_mid = sd(bar1_data2_middle$MSE[,1])


#bar2
mCbar2_bp = mean(bar2_climate_bpconstraints$MSE[,1])
mCbar2_bpno = mean(bar2_climate_bpnoconstraints$MSE[,1])
mCbar2_mid = mean(bar2_climate_middle$MSE[,1])

m10bar2_bp = mean(bar2_data10_bpconstraints$MSE[,1])
m10bar2_bpno = mean(bar2_data10_bpnoconstraints$MSE[,1])
m10bar2_mid = mean(bar2_data10_middle$MSE[,1])

m2bar2_bp = mean(bar2_data2_bpconstraints$MSE[,1])
m2bar2_bpno = mean(bar2_data2_bpnoconstraints$MSE[,1])
m2bar2_mid = mean(bar2_data2_middle$MSE[,1])

sdCbar2_bp = sd(bar2_climate_bpconstraints$MSE[,1])
sdCbar2_bpno = sd(bar2_climate_bpnoconstraints$MSE[,1])
sdCbar2_mid = sd(bar2_climate_middle$MSE[,1])

sd10bar2_bp = sd(bar2_data10_bpconstraints$MSE[,1])
sd10bar2_bpno = sd(bar2_data10_bpnoconstraints$MSE[,1])
sd10bar2_mid = sd(bar2_data10_middle$MSE[,1])

sd2bar2_bp = sd(bar2_data2_bpconstraints$MSE[,1])
sd2bar2_bpno = sd(bar2_data2_bpnoconstraints$MSE[,1])
sd2bar2_mid = sd(bar2_data2_middle$MSE[,1])




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



#plots----------------------------

#bar0

#mCbar0_bp
plot(bar0_climate_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar0_bpno#mCbar0_bp
plot(bar0_climate_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar0_bpno
plot(bar0_climate_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar0_mid
plot(bar0_climate_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#m10bar0_bp
plot(bar0_data10_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m10bar0_bpno 
plot(bar0_data10_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m10bar0_mid 
plot(bar0_data10_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#m2bar0_bp 
plot(bar0_data2_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m2bar0_bpno 
plot(bar0_data2_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m2bar0_mid 
plot(bar0_data2_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")


#bar1

#mCbar1_bp 
plot(bar1_climate_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar1_bpno
plot(bar1_climate_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar1_mid 
plot(bar1_climate_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#m10bar1_bp 
plot(bar1_data10_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m10bar1_bpno 
plot(bar1_data10_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m10bar1_mid
plot(bar1_data10_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#m2bar1_bp 
plot(bar1_data2_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m2bar1_bpno
plot(bar1_data2_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m2bar1_mid 
plot(bar1_data2_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#bar2

#mCbar2_bp
plot(bar2_climate_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar2_bpno
plot(bar2_climate_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#mCbar2_mid
plot(bar2_climate_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#m10bar2_bp 
plot(bar2_data10_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m10bar2_bpno 
plot(bar2_data10_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m10bar2_mid 
plot(bar2_data10_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")

#m2bar2_bp
plot(bar2_data2_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m2bar2_bpno
plot(bar2_data2_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
#m2bar2_mid 
plot(bar2_data2_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")





#Bar 0 
#mCbar0_bp
plot(bar0_climate_bpconstraints$MSE[c(1:2000),1], ylab = "MSE", xlab="Number of Iterations")
    #about 500 
#mCbar0_bpno
plot(bar0_climate_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    #about 400
#mCbar0_mid
plot(bar0_climate_middle$MSE[c(1:10000),1], ylab = "MSE", xlab="Number of Iterations")
    #ABOut 2000

#m10bar0_bp
plot(bar0_data10_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##about 100
#m10bar0_bpno 
plot(bar0_data10_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    #ABout 200
#m10bar0_mid 
plot(bar0_data10_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##About 300

#m2bar0_bp 
plot(bar0_data2_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    #Anout 50
#m2bar0_bpno 
plot(bar0_data2_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##None
#m2bar0_mid 
plot(bar0_data2_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##About 50


#bar1

#mCbar1_bp 
plot(bar1_climate_bpconstraints$MSE[c(1:2000),1], ylab = "MSE", xlab="Number of Iterations")
    ##about 1000
#mCbar1_bpno
plot(bar1_climate_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##About 1500
#mCbar1_mid 
plot(bar1_climate_middle$MSE[c(0:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##About 1000

#m10bar1_bp 
plot(bar1_data10_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##About 100
#m10bar1_bpno 
plot(bar1_data10_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    #Maybe 50
#m10bar1_mid
plot(bar1_data10_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Maybe 20

#m2bar1_bp 
plot(bar1_data2_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    #Maybe 50
#m2bar1_bpno
plot(bar1_data2_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##None
#m2bar1_mid 
plot(bar1_data2_middle$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 100


#bar2

#mCbar2_bp
plot(bar2_climate_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 500
#mCbar2_bpno
plot(bar2_climate_bpnoconstraints$MSE[c(1:2000),1], ylab = "MSE", xlab="Number of Iterations")
    #All over the place 900
#mCbar2_mid
plot(bar2_climate_middle$MSE[c(1:3000),1], ylab = "MSE", xlab="Number of Iterations")
    #All over the place ##Around 1250

#m10bar2_bp 
plot(bar2_data10_bpconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 700
#m10bar2_bpno 
plot(bar2_data10_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 50 ish 
#m10bar2_mid 
plot(bar2_data10_middle$MSE[c(1:4000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 200 

#m2bar2_bp
plot(bar2_data2_bpconstraints$MSE[c(1:3000),1], ylab = "MSE", xlab="Number of Iterations")
    #Around 50 or 500
#m2bar2_bpno
plot(bar2_data2_bpnoconstraints$MSE[c(1:1000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 500
#m2bar2_mid 
plot(bar2_data2_middle$MSE[c(1:3000),1], ylab = "MSE", xlab="Number of Iterations")
    ##Around 50 or 500






#-------------------------------------BIC---------------------------------#
#bar0


#mCbar0_bpno
plot(bar0_climate_bpconstraints$BIC[c(1:25000),1], ylab = "BIC", xlab="Number of Iterations")
    ##About 6000
#mCbar0_bpno
plot(bar0_climate_bpnoconstraints$BIC[c(1:10000),1], ylab = "BIC", xlab="Number of Iterations")
    ##ABOUT 5000
#mCbar0_mid
plot(bar0_climate_middle$BIC[c(1:10000),1], ylab = "BIC", xlab="Number of Iterations")
    ##About 1000

#m10bar0_bp
plot(bar0_data10_bpconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    ##about 100
#m10bar0_bpno 
plot(bar0_data10_bpnoconstraints$BIC[c(1:2000),1], ylab = "BIC", xlab="Number of Iterations")
    ##about 400
#m10bar0_mid 
plot(bar0_data10_middle$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    ##about 200

#m2bar0_bp 
plot(bar0_data2_bpconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    ##ABout 400
#m2bar0_bpno 
plot(bar0_data2_bpnoconstraints$BIC[c(1:10000),1], ylab = "BIC", xlab="Number of Iterations")
    ##None 
#m2bar0_mid 
plot(bar0_data2_middle$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    ##About 400

#bar1

#mCbar1_bp 
plot(bar1_climate_bpconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
#mCbar1_bpno
plot(bar1_climate_bpnoconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
#mCbar1_mid 
plot(bar1_climate_middle$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")

#m10bar1_bp 
plot(bar1_data10_bpconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
#m10bar1_bpno 
plot(bar1_data10_bpnoconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
#m10bar1_mid
plot(bar1_data10_middle$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")

#m2bar1_bp 
plot(bar1_data2_bpconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
#m2bar1_bpno
plot(bar1_data2_bpnoconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
#m2bar1_mid 
plot(bar1_data2_middle$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")

#bar2

#mCbar2_bp
plot(bar2_climate_bpconstraints$BIC[c(1:3000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 1000
#mCbar2_bpno
plot(bar2_climate_bpnoconstraints$BIC[c(1:10000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 1000
#mCbar2_mid
plot(bar2_climate_middle$BIC[c(1:3000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 1000

#m10bar2_bp 
plot(bar2_data10_bpconstraints$BIC[c(1:3000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 1500
#m10bar2_bpno 
plot(bar2_data10_bpnoconstraints$BIC[c(1:4000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 1000
#m10bar2_mid 
plot(bar2_data10_middle$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 500

#m2bar2_bp
plot(bar2_data2_bpconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    ##about 500
#m2bar2_bpno
plot(bar2_data2_bpnoconstraints$BIC[c(1:1000),1], ylab = "BIC", xlab="Number of Iterations")
    ##about 500
#m2bar2_mid 
plot(bar2_data2_middle$BIC[c(1:2000),1], ylab = "BIC", xlab="Number of Iterations")
    #about 200



##----------------------more BIC------------------------------
#-----------BIC-----
#bar0
bCbar0_bp = mean(bar0_climate_bpconstraints$BIC[,1])
bCbar0_bpno = mean(bar0_climate_bpnoconstraints$BIC[,1])
bCbar0_mid = mean(bar0_climate_middle$BIC[,1])

b10bar0_bp = mean(bar0_data10_bpconstraints$BIC[,1])
b10bar0_bpno = mean(bar0_data10_bpnoconstraints$BIC[,1])
b10bar0_mid = mean(bar0_data10_middle$BIC[,1])

b2bar0_bp = mean(bar0_data2_bpconstraints$BIC[,1])
b2bar0_bpno = mean(bar0_data2_bpnoconstraints$BIC[,1])
b2bar0_mid = mean(bar0_data2_middle$BIC[,1])

sdBCbar0_bp = sd(bar0_climate_bpconstraints$BIC[,1])
sdBCbar0_bpno = sd(bar0_climate_bpnoconstraints$BIC[,1])
sdBCbar0_mid = sd(bar0_climate_middle$BIC[,1])

sdB10bar0_bp = sd(bar0_data10_bpconstraints$BIC[,1])
sdB10bar0_bpno = sd(bar0_data10_bpnoconstraints$BIC[,1])
sdB10bar0_mid = sd(bar0_data10_middle$BIC[,1])

sdB2bar0_bp = sd(bar0_data2_bpconstraints$BIC[,1])
sdB2bar0_bpno = sd(bar0_data2_bpnoconstraints$BIC[,1])
sdB2bar0_mid = sd(bar0_data2_middle$BIC[,1])


#bar1
bCbar1_bp = mean(bar1_climate_bpconstraints$BIC[,1])
bCbar1_bpno = mean(bar1_climate_bpnoconstraints$BIC[,1])
bCbar1_mid = mean(bar1_climate_middle$BIC[,1])

b10bar1_bp = mean(bar1_data10_bpconstraints$BIC[,1])
b10bar1_bpno = mean(bar1_data10_bpnoconstraints$BIC[,1])
b10bar1_mid = mean(bar1_data10_middle$BIC[,1])

b2bar1_bp = mean(bar1_data2_bpconstraints$BIC[,1])
b2bar1_bpno = mean(bar1_data2_bpnoconstraints$BIC[,1])
b2bar1_mid = mean(bar1_data2_middle$BIC[,1])

sdBCbar1_bp = sd(bar1_climate_bpconstraints$BIC[,1])
sdBCbar1_bpno = sd(bar1_climate_bpnoconstraints$BIC[,1])
sdBCbar1_mid = sd(bar1_climate_middle$BIC[,1])

sdB10bar1_bp = sd(bar1_data10_bpconstraints$BIC[,1])
sdB10bar1_bpno = sd(bar1_data10_bpnoconstraints$BIC[,1])
sdB10bar1_mid = sd(bar1_data10_middle$BIC[,1])

sdB2bar1_bp = sd(bar1_data2_bpconstraints$BIC[,1])
sdB2bar1_bpno = sd(bar1_data2_bpnoconstraints$BIC[,1])
sdB2bar1_mid = sd(bar1_data2_middle$BIC[,1])


#bar2
bCbar2_bp = mean(bar2_climate_bpconstraints$BIC[,1])
bCbar2_bpno = mean(bar2_climate_bpnoconstraints$BIC[,1])
bCbar2_mid = mean(bar2_climate_middle$BIC[,1])

b10bar2_bp = mean(bar2_data10_bpconstraints$BIC[,1])
b10bar2_bpno = mean(bar2_data10_bpnoconstraints$BIC[,1])
b10bar2_mid = mean(bar2_data10_middle$BIC[,1])

b2bar2_bp = mean(bar2_data2_bpconstraints$BIC[,1])
b2bar2_bpno = mean(bar2_data2_bpnoconstraints$BIC[,1])
b2bar2_mid = mean(bar2_data2_middle$BIC[,1])

sdBCbar2_bp = sd(bar2_climate_bpconstraints$BIC[,1])
sdBCbar2_bpno = sd(bar2_climate_bpnoconstraints$BIC[,1])
sdBCbar2_mid = sd(bar2_climate_middle$BIC[,1])

sdB10bar2_bp = sd(bar2_data10_bpconstraints$BIC[,1])
sdB10bar2_bpno = sd(bar2_data10_bpnoconstraints$BIC[,1])
sdB10bar2_mid = sd(bar2_data10_middle$BIC[,1])

sdB2bar2_bp = sd(bar2_data2_bpconstraints$BIC[,1])
sdB2bar2_bpno = sd(bar2_data2_bpnoconstraints$BIC[,1])
sdB2bar2_mid = sd(bar2_data2_middle$BIC[,1])


b_C_bp = c(bCbar0_bp, bCbar1_bp, bCbar2_bp)
b_C_bpno = c(bCbar0_bpno, bCbar1_bpno, bCbar2_bpno)
b_C_mid = c(bCbar0_mid, bCbar1_mid, bCbar2_mid)

sdB_C_bp = c(sdBCbar0_bp, sdBCbar1_bp, sdBCbar2_bp)
sdB_C_bpno = c(sdBCbar0_bpno, sdBCbar1_bpno, sdBCbar2_bpno)
sdB_C_mid = c(sdBCbar0_mid, sdBCbar1_mid, sdBCbar2_mid)

b_10_bp = c(b10bar0_bp, b10bar1_bp, b10bar2_bp)
b_10_bpno = c(b10bar0_bpno, b10bar1_bpno, b10bar2_bpno)
b_10_mid = c(b10bar0_mid, b10bar1_mid, b10bar2_mid)

sdB_10_bp = c(sdBCbar1_bp, sdB10bar1_bp, sdB2bar1_bp)
sdB_10_bpno = c(sdBCbar1_bpno, sdB10bar1_bpno, sdB2bar1_bpno)
sdB_10_mid = c(sdBCbar1_mid, sdB10bar1_mid, sdB2bar1_mid)

b_2_bp = c(b2bar0_bp, b2bar1_bp, b2bar2_bp)
b_2_bpno = c(b2bar0_bpno, b2bar1_bpno, b2bar2_bpno)
b_2_mid = c(b2bar0_mid, b2bar1_mid, b2bar2_mid)

sdB_2_bp = c(sdB2bar0_bp, sdB2bar1_bp, sdB2bar2_bp)
sdB_2_bpno = c(sdB2bar0_bpno, sdB2bar1_bpno, sdB2bar2_bpno)
sdB_2_mid = c(sdB2bar0_mid, sdB2bar1_mid, sdB2bar2_mid)

#Climate BIC
x=c(0:2)
plot(x, b_C_bp, xlab="BAR Number", ylab="Average BIC", main="Average BIC for Climate Data", col="red", ylim=c(-600,0),  cex = 1.5, pch=16)
points(x, b_C_bp+sdB_C_bp*2, col="red", cex = 1, pch=16)
points(x, b_C_bp-sdB_C_bp*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_C_bp[i+1]-sdB_C_bp[i+1]*2,b_C_bp[i+1]+sdB_C_bp[i+1]*2), col="red",lty=2)
}
points(x, b_C_bpno, col="purple", cex = 1.5, pch=16)
points(x, b_C_bpno+sdB_C_bpno*2, col="purple", cex = 1, pch=16)
points(x, b_C_bpno-sdB_C_bpno*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_C_bpno[i+1]-sdB_C_bpno[i+1]*2,b_C_bpno[i+1]+sdB_C_bpno[i+1]*2), col="purple",lty=2)
}

points(x, b_C_mid, col="blue", cex = 1.5, pch=16)
points(x, b_C_mid+sdB_C_mid*2, col="blue", cex = 1, pch=16)
points(x, b_C_mid-sdB_C_mid*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_C_mid[i+1]-sdB_C_mid[i+1]*2,b_C_mid[i+1]+sdB_C_mid[i+1]*2), col="blue",lty=2)
}

legend(1, -50, legend=c("Bai-Perron Constrains","Bai-Perron No Constrains","Middle"),
       col=c("red" ,"purple", "blue"), lty=1:1, cex=.5)


#data10 BIC
x=c(0:2)
plot(x, b_10_bp, xlab="BAR Number", ylab="Average BIC", main="Average BIC for Data 10", col="red", ylim=c(200,600),  cex = 1.5, pch=16)
points(x, b_10_bp+sdB_10_bp*2, col="red", cex = 1, pch=16)
points(x, b_10_bp-sdB_10_bp*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_10_bp[i+1]-sdB_10_bp[i+1]*2,b_10_bp[i+1]+sdB_10_bp[i+1]*2), col="red",lty=2)
}
points(x, b_10_bpno, col="purple", cex = 1.5, pch=16)
points(x, b_10_bpno+sdB_10_bpno*2, col="purple", cex = 1, pch=16)
points(x, b_10_bpno-sdB_10_bpno*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_10_bpno[i+1]-sdB_10_bpno[i+1]*2,b_10_bpno[i+1]+sdB_10_bpno[i+1]*2), col="purple",lty=2)
}

points(x, b_10_mid, col="blue", cex = 1.5, pch=16)
points(x, b_10_mid+sdB_10_mid*2, col="blue", cex = 1, pch=16)
points(x, b_10_mid-sdB_10_mid*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_10_mid[i+1]-sdB_10_mid[i+1]*2,b_10_mid[i+1]+sdB_10_mid[i+1]*2), col="blue",lty=2)
}

legend(1, 300, legend=c("Bai-Perron Constrains","Bai-Perron No Constrains","Middle"),
       col=c("red" ,"purple", "blue"), lty=1:1, cex=.5)


#Data 2
x=c(0:2)
plot(x, b_2_bp, xlab="BAR Number", ylab="Average BIC", main="Average BIC for Data2", col="red", ylim=c(200,400),  cex = 1.5, pch=16)
points(x, b_2_bp+sdB_2_bp*2, col="red", cex = 1, pch=16)
points(x, b_2_bp-sdB_2_bp*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_2_bp[i+1]-sdB_2_bp[i+1]*2,b_2_bp[i+1]+sdB_2_bp[i+1]*2), col="red",lty=2)
}
points(x, b_2_bpno, col="purple", cex = 1.5, pch=16)
points(x, b_2_bpno+sdB_2_bpno*2, col="purple", cex = 1, pch=16)
points(x, b_2_bpno-sdB_2_bpno*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_2_bpno[i+1]-sdB_2_bpno[i+1]*2,b_2_bpno[i+1]+sdB_2_bpno[i+1]*2), col="purple",lty=2)
}

points(x, b_2_mid, col="blue", cex = 1.5, pch=16)
points(x, b_2_mid+sdB_2_mid*2, col="blue", cex = 1, pch=16)
points(x, b_2_mid-sdB_2_mid*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(b_2_mid[i+1]-sdB_2_mid[i+1]*2,b_2_mid[i+1]+sdB_2_mid[i+1]*2), col="blue",lty=2)
}

legend(1, 250, legend=c("Bai-Perron Constrains","Bai-Perron No Constrains","Middle"),
       col=c("red" ,"purple", "blue"), lty=1:1, cex=.5)


