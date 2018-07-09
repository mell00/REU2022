#analysis data obtained from move simulations
bar0_climate = readRDS("bar0_climate.RData")
bar1_climate = readRDS("bar1_climate.RData")
bar2_climate = readRDS("bar2_climate.RData")


#---------------------------------MSE and BIC Averages --------------------------------#
mCbar0 = mean(colMeans(bar0_climate$MSE))
mCbar1 = mean(colMeans(bar1_climate$MSE))
mCbar2 = mean(colMeans(bar2_climate$MSE))

sdCbar0 = mean(apply(bar0_climate$MSE, 1, sd))
sdCbar1 = mean(apply(bar1_climate$MSE,1,sd))
sdCbar2 = mean(apply(bar2_climate$MSE,1,sd))

mBICCbar0 = mean(colMeans(bar0_climate$BIC))
mBICCbar1 = mean(colMeans(bar1_climate$BIC))
mBICCbar2 = mean(colMeans(bar2_climate$BIC))

sdBICCbar0 = mean(apply(bar0_climate$BIC,1,sd))
sdBICCbar1 = mean(apply(bar1_climate$BIC,1,sd))
sdBICCbar2 = mean(apply(bar2_climate$BIC,1,sd))

#all MSE
plot(0, mCbar0, xlab="DataSet Number", ylab="Average MSE", main="Average MSE for Climate Data", col="red", ylim=c(0,.02), xlim=c(0,2), cex = 1, pch=16)
lines(c(0,0),c(mCbar0-sdCbar0*2,mCbar0+sdCbar0*2), col="red",lty=1)
points(1, mCbar1, col="purple", cex = 1, pch=16)
lines(c(1,1),c(mCbar1-sdCbar1*2,mCbar1+sdCbar1*2), col="purple",lty=1)
points(2, mCbar2, col="blue", cex = 1, pch=16)
lines(c(2,2),c(mCbar2-sdCbar2*2,mCbar2+sdCbar2*2), col="blue",lty=1)


#BIC
plot(0, mBICCbar0, xlab="DataSet Number", ylab="Average BIC", main="Average BIC for Climate Data", col="red", ylim=c(-450,0), xlim=c(0,2), cex = 1, pch=16)
lines(c(0,0),c(mBICCbar0-sdBICCbar0*2,mBICCbar0+sdBICCbar0*2), col="red",lty=1)
points(1, mBICCbar1, col="purple", cex = 1, pch=16)
lines(c(1,1),c(mBICCbar1-sdBICCbar1*2,mBICCbar1+sdBICCbar1*2), col="purple",lty=1)
points(2, mBICCbar2, col="blue", cex = 1, pch=16)
lines(c(2,2),c(mBICCbar2-sdBICCbar2*2,mBICCbar2+sdBICCbar2*2), col="blue",lty=1)

x.y = -250
y.y = -150
par(mfrow=c(3,3))
plot(bar0_climate$BIC[[1]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[2]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[3]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[4]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[5]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[6]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[7]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[8]], ylim=c(x.y,y.y), ylab="BIC")
plot(bar0_climate$BIC[[9]], ylim=c(x.y,y.y), ylab="BIC")

x.y = .005
y.y = .015
plot(bar0_climate$MSE[[1]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[2]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[3]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[4]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[5]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[6]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[7]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[8]], ylim=c(x.y,y.y), ylab="MSE")
plot(bar0_climate$MSE[[9]], ylim=c(x.y,y.y), ylab="MSE")

