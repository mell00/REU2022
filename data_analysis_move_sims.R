#analysis data obtained from move simulations

bar0_data0a = readRDS("bar0_data0a.RData")
bar0_data0b = readRDS("bar0_data0b.RData")
bar0_data1 = readRDS("bar0_data1.RData")
bar0_data2 = readRDS("bar0_data2.RData")
bar0_data3 = readRDS("bar0_data3.RData")
bar0_data4 = readRDS("bar0_data4.RData")
bar0_data5 = readRDS("bar0_data5.RData")
bar0_data6 = readRDS("bar0_data6.RData")
bar0_data7 = readRDS("bar0_data7.RData")
bar0_data8 = readRDS("bar0_data8.RData")
bar0_data9 = readRDS("bar0_data9.RData")
bar0_data10 = readRDS("bar0_data10.RData")

barA_data0a = readRDS("barA_data0a.RData")
barA_data0b = readRDS("barA_data0b.RData")
barA_data1 = readRDS("barA_data1.RData")
barA_data2 = readRDS("barA_data2.RData")
barA_data3 = readRDS("barA_data3.RData")
barA_data4 = readRDS("barA_data4.RData")
barA_data5 = readRDS("barA_data5.RData")
barA_data6 = readRDS("barA_data6.RData")
barA_data7 = readRDS("barA_data7.RData")
barA_data8 = readRDS("barA_data8.RData")
barA_data9 = readRDS("barA_data9.RData")
barA_data10 = readRDS("barA_data10.RData")

barB_data0a = readRDS("barB_data0a.RData")
barB_data0b = readRDS("barB_data0b.RData")
barB_data1 = readRDS("barB_data1.RData")
barB_data2 = readRDS("barB_data2.RData")
barB_data3 = readRDS("barB_data3.RData")
barB_data4 = readRDS("barB_data4.RData")
barB_data5 = readRDS("barB_data5.RData")
barB_data6 = readRDS("barB_data6.RData")
barB_data7 = readRDS("barB_data7.RData")
barB_data8 = readRDS("barB_data8.RData")
barB_data9 = readRDS("barB_data9.RData")
barB_data10 = readRDS("barB_data10.RData")

x = c(1:2500)
y.low = .6
y.high = 1.5

par(mfrow=c(3,3))
plot(x, bar0_data0a$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data0a$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data1$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))


par(mfrow=c(3,3))
plot(x, bar0_data2$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 16
y.high = 33
par(mfrow=c(3,3))
plot(x, bar0_data3$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data3$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data4$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data5$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data5$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data6$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data6$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data7$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data7$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data8$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data8$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data9$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data9$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

par(mfrow=c(3,3))
plot(x, bar0_data10$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))


fit.1 = lm(bar0_data0a$MSE[1]$`current_result$MSE[, 1]`~x)
plot(x, bar0_data1$MSE[1]$`current_result$MSE[, 1]`, main = 1, ylab="MSE", xlab="Time")
abline(lm(bar0_data0a$MSE[1]$`current_result$MSE[, 1]`~(x)^2) ,col="red",lwd=2)
lines(lowess(x,bar0_data0a$MSE[1]$`current_result$MSE[, 1]`), col="blue",lwd=2)
lines(smooth.spline(x,bar0_data0a$MSE[1]$`current_result$MSE[, 1]`), col="green", lwd=2)
smoothed25 <- predict(loess(bar0_data0a$MSE[1]$`current_result$MSE[, 1]`~x , data=bar0_data0a, span=0.25)) 
lines(smoothed25, x=x, col="orange", lwd=3)

#----------------------------BAR A--------------------------------#
x = c(1:2500)
y.low = .6
y.high = 1.5

#Data 0a
par(mfrow=c(3,3))
plot(x, barA_data0a$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data0a$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 1
par(mfrow=c(3,3))
plot(x, barA_data1$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 2
par(mfrow=c(3,3))
plot(x, barA_data2$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 16
y.high = 33
#Data 3
par(mfrow=c(3,3))
plot(x, barA_data3$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data3$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 4
par(mfrow=c(3,3))
plot(x, barA_data4$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 5
par(mfrow=c(3,3))
plot(x, barA_data5$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data5$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 6
par(mfrow=c(3,3))
plot(x, barA_data6$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data6$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 7
par(mfrow=c(3,3))
plot(x, barA_data7$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data7$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 8
par(mfrow=c(3,3))
plot(x, barA_data8$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data8$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 9
par(mfrow=c(3,3))
plot(x, barA_data9$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data9$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 10
par(mfrow=c(3,3))
plot(x, barA_data10$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#-------------------------------------BAR B ---------------------------------#
x = c(1:2500)
y.low = .6
y.high = 1.5

#Data 0a
par(mfrow=c(3,3))
plot(x, barB_data0a$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data0a$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 1
par(mfrow=c(3,3))
plot(x, barB_data1$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 2
par(mfrow=c(3,3))
plot(x, barB_data2$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 3
par(mfrow=c(3,3))
plot(x, barB_data3$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data3$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 20
y.high = 60
#Data 4
par(mfrow=c(3,3))
plot(x, barB_data4$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 5
par(mfrow=c(3,3))
plot(x, barB_data5$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data5$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 6
par(mfrow=c(3,3))
plot(x, barB_data6$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data6$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#Data 7
y.low = 0
y.high = 2
par(mfrow=c(3,3))
plot(x, barB_data7$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data7$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 0
y.high = 3
#Data 8
par(mfrow=c(3,3))
plot(x, barB_data8$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data8$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 5
y.high = 20
#Data 9
par(mfrow=c(3,3))
plot(x, barB_data9$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data9$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 3
y.high = 17
#Data 10
par(mfrow=c(3,3))
plot(x, barB_data10$MSE[[1]], main = 1, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[2]], main = 2, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[3]], main = 3, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[4]], main = 4, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[5]], main = 5, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[6]], main = 6, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[7]], main = 7, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[8]], main = 8, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$MSE[[9]], main = 9, ylab="MSE", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))



#-----------------------------------------BIC---------------------------------#
y.low = 200
y.high = 370
#bar0 Data 1
par(mfrow=c(3,3))
plot(x, bar0_data1$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data1$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barA Data 1
par(mfrow=c(3,3))
plot(x, barA_data1$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data1$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barB Data 1
par(mfrow=c(3,3))
plot(x, barB_data1$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data1$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 230
y.high = 350
#bar0 Data 2
par(mfrow=c(3,3))
plot(x, bar0_data2$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data2$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barA Data 2
par(mfrow=c(3,3))
plot(x, barA_data2$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data2$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barB Data 2
par(mfrow=c(3,3))
plot(x, barB_data2$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data2$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

y.low = 550
y.high = 700
#bar0 Data 2
par(mfrow=c(3,3))
plot(x, bar0_data4$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data4$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barA Data 2
par(mfrow=c(3,3))
plot(x, barA_data4$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data4$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barB Data 2
par(mfrow=c(3,3))
plot(x, barB_data4$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data4$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))



y.low = 330
y.high = 470
#bar0 Data 10
par(mfrow=c(3,3))
plot(x, bar0_data10$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, bar0_data10$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barA Data 10
par(mfrow=c(3,3))
plot(x, barA_data10$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barA_data10$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))

#barB Data 10
par(mfrow=c(3,3))
plot(x, barB_data10$BIC[[1]], main = 1, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[2]], main = 2, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[3]], main = 3, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[4]], main = 4, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[5]], main = 5, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[6]], main = 6, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[7]], main = 7, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[8]], main = 8, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
plot(x, barB_data10$BIC[[9]], main = 9, ylab="BIC", xlab="Time", ylim=c(y.low,y.high))
par(mfrow=c(1,1))


#---------------------------------MSE Averages --------------------------------#
m0abar0 = 0
m1bar0 = 0
m2bar0 = 0
m3bar0 = 0
m4bar0 = 0
m5bar0 = 0
m6bar0 = 0
m7bar0 = 0
m8bar0 = 0
m9bar0 = 0
m10barA = 0
m0abarA = 0
m1barA = 0
m2barA = 0
m3barA = 0
m4barA = 0
m5barA = 0
m6barA = 0
m7barA = 0
m8barA = 0
m9barA = 0
m10barA = 0
m0abarB = 0
m1barB = 0
m2barB = 0
m3barB = 0
m4barB = 0
m5barB = 0
m6barB = 0
m7barB = 0
m8barB = 0
m9barB = 0
m10barB = 0
for(i in 1:9) {
  avg0 = mean(bar0_data0a$MSE[[i]])
  m0abar0 =+ avg0
  avg1 = mean(bar0_data1$MSE[[i]])
  m1bar0 =+   avg1
  avg2 = mean(bar0_data2$MSE[[i]])
  m2bar0 =+   avg2
  avg3 = mean(bar0_data3$MSE[[i]])
  m3bar0 =+   avg3
  avg4 = mean(bar0_data4$MSE[[i]])
  m4bar0 =+   avg4
  avg5 = mean(bar0_data5$MSE[[i]])
  m5bar0 =+   avg5
  avg6 = mean(bar0_data6$MSE[[i]])
  m6bar0 =+   avg6
  avg7 = mean(bar0_data7$MSE[[i]])
  m7bar0 =+   avg7
  avg8 = mean(bar0_data8$MSE[[i]])
  m8bar0 =+   avg8
  avg9 = mean(bar0_data9$MSE[[i]])
  m9bar0 =+   avg9
  avg10 = mean(bar0_data10$MSE[[i]])
  m10bar0 =+   avg10
}

for(i in 1:9) {
  avg0 = mean(barA_data0a$MSE[[i]])
  m0abarA =+   avg0
  avg1 = mean(barA_data1$MSE[[i]])
  m1barA =+   avg1
  avg2 = mean(barA_data2$MSE[[i]])
  m2barA =+   avg2
  avg3 = mean(barA_data3$MSE[[i]])
  m3barA =+   avg3
  avg4 = mean(barA_data4$MSE[[i]])
  m4barA =+   avg4
  avg5 = mean(barA_data5$MSE[[i]])
  m5barA =+   avg5
  avg6 = mean(barA_data6$MSE[[i]])
  m6barA =+   avg6
  avg7 = mean(barA_data7$MSE[[i]])
  m7barA =+   avg7
  avg8 = mean(barA_data8$MSE[[i]])
  m8barA =+   avg8
  avg9 = mean(barA_data9$MSE[[i]])
  m9barA =+   avg9
  avg10 = mean(barA_data10$MSE[[i]])
  m10barA =+   avg10
}

for(i in 1:9) {
  avg0 = mean(barB_data0a$MSE[[i]])
  m0abarB =+   avg0
  avg1 = mean(barB_data1$MSE[[i]])
  m1barB =+   avg1
  avg2 = mean(barB_data2$MSE[[i]])
  m2barB =+   avg2
  avg3 = mean(barB_data3$MSE[[i]])
  m3barB =+   avg3
  avg4 = mean(barB_data4$MSE[[i]])
  m4barB =+   avg4
  avg5 = mean(barB_data5$MSE[[i]])
  m5barB =+   avg5
  avg6 = mean(barB_data6$MSE[[i]])
  m6barB =+   avg6
  avg7 = mean(barB_data7$MSE[[i]])
  m7barB =+   avg7
  avg8 = mean(barB_data8$MSE[[i]])
  m8barB =+   avg8
  avg9 = mean(barB_data9$MSE[[i]])
  m9barB =+   avg9
  avg10 = mean(barB_data10$MSE[[i]])
  m10barB =+   avg10
}

avgbar0 = c(m0abar0 / 9, m1bar0 / 9, m2bar0 / 9, m3bar0 / 9 , m4bar0 / 9, m5bar0 / 9, m6bar0 / 9, m7bar0 / 9, 
            m8bar0 / 9, m9bar0 / 9, m10barA /9)
avgbarA = c(m0abarA / 9, m1barA / 9, m2barA / 9, m3barA / 9, m4barA / 9,
            m5barA / 9, m6barA / 9, m7barA / 9, m8barA / 9, m9barA / 9, m10barA / 9)
avgbarB = c(m0abarB / 9, m1barB / 9, m2barB / 9, m3barB / 9, m4barB / 9, m5barB / 9, 
            m6barB / 9, m7barB / 9, m8barB / 9, m9barB /9, m10barB / 9 )

x_time = c(0:10)
plot(x_time, avgbar0, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE over all Datasets", col="red", ylim=c(0,5), cex = 1.5, pch=16)
lines(c(0,10),c(mean(avgbar0),mean(avgbar0)) , col="red")
points(x_time, avgbarA, col="blue", cex = 1.5, pch=16)
lines(c(0,10),c(mean(avgbarA),mean(avgbarA)) , col="blue")
points(x_time, avgbarB, col="green", cex = 1.5, pch=16)
lines(c(0,10),c(mean(avgbarB),mean(avgbarB)) , col="green")
legend(7.5, 4.5, legend=c("bar0", "barA", "barB"),
       col=c("red", "blue", "green"), lty=1:1, cex=1)



#------------------------------------BIC mean-----------------------------#
mBIC0abar0 = 0
mBIC1bar0 = 0
mBIC2bar0 = 0
mBIC3bar0 = 0
mBIC4bar0 = 0
mBIC5bar0 = 0
mBIC6bar0 = 0
mBIC7bar0 = 0
mBIC8bar0 = 0
mBIC9bar0 = 0
mBIC10barA = 0
mBIC0abarA = 0
mBIC1barA = 0
mBIC2barA = 0
mBIC3barA = 0
mBIC4barA = 0
mBIC5barA = 0
mBIC6barA = 0
mBIC7barA = 0
mBIC8barA = 0
mBIC9barA = 0
mBIC10barA = 0
mBIC0abarB = 0
mBIC1barB = 0
mBIC2barB = 0
mBIC3barB = 0
mBIC4barB = 0
mBIC5barB = 0
mBIC6barB = 0
mBIC7barB = 0
mBIC8barB = 0
mBIC9barB = 0
mBIC10barB = 0
for(i in 1:9) {
  avg0 = mean(bar0_data0a$BIC[[i]])
  mBIC0abar0 =+ avg0
  avg1 = mean(bar0_data1$BIC[[i]])
  mBIC1bar0 =+   avg1
  avg2 = mean(bar0_data2$BIC[[i]])
  mBIC2bar0 =+   avg2
  avg3 = mean(bar0_data3$BIC[[i]])
  mBIC3bar0 =+   avg3
  avg4 = mean(bar0_data4$BIC[[i]])
  mBIC4bar0 =+   avg4
  avg5 = mean(bar0_data5$BIC[[i]])
  mBIC5bar0 =+   avg5
  avg6 = mean(bar0_data6$BIC[[i]])
  mBIC6bar0 =+   avg6
  avg7 = mean(bar0_data7$BIC[[i]])
  mBIC7bar0 =+   avg7
  avg8 = mean(bar0_data8$BIC[[i]])
  mBIC8bar0 =+   avg8
  avg9 = mean(bar0_data9$BIC[[i]])
  mBIC9bar0 =+   avg9
  avg10 = mean(bar0_data10$BIC[[i]])
  mBIC10bar0 =+   avg10
}

for(i in 1:9) {
  avg0 = mean(barA_data0a$BIC[[i]])
  mBIC0abarA =+   avg0
  avg1 = mean(barA_data1$BIC[[i]])
  mBIC1barA =+   avg1
  avg2 = mean(barA_data2$BIC[[i]])
  mBIC2barA =+   avg2
  avg3 = mean(barA_data3$BIC[[i]])
  mBIC3barA =+   avg3
  avg4 = mean(barA_data4$BIC[[i]])
  mBIC4barA =+   avg4
  avg5 = mean(barA_data5$BIC[[i]])
  mBIC5barA =+   avg5
  avg6 = mean(barA_data6$BIC[[i]])
  mBIC6barA =+   avg6
  avg7 = mean(barA_data7$BIC[[i]])
  mBIC7barA =+   avg7
  avg8 = mean(barA_data8$BIC[[i]])
  mBIC8barA =+   avg8
  avg9 = mean(barA_data9$BIC[[i]])
  mBIC9barA =+   avg9
  avg10 = mean(barA_data10$BIC[[i]])
  mBIC10barA =+   avg10
}

for(i in 1:9) {
  avg0 = mean(barB_data0a$BIC[[i]])
  mBIC0abarB =+   avg0
  avg1 = mean(barB_data1$BIC[[i]])
  mBIC1barB =+   avg1
  avg2 = mean(barB_data2$BIC[[i]])
  mBIC2barB =+   avg2
  avg3 = mean(barB_data3$BIC[[i]])
  mBIC3barB =+   avg3
  avg4 = mean(barB_data4$BIC[[i]])
  mBIC4barB =+   avg4
  avg5 = mean(barB_data5$BIC[[i]])
  mBIC5barB =+   avg5
  avg6 = mean(barB_data6$BIC[[i]])
  mBIC6barB =+   avg6
  avg7 = mean(barB_data7$BIC[[i]])
  mBIC7barB =+   avg7
  avg8 = mean(barB_data8$BIC[[i]])
  mBIC8barB =+   avg8
  avg9 = mean(barB_data9$BIC[[i]])
  mBIC9barB =+   avg9
  avg10 = mean(barB_data10$BIC[[i]])
  mBIC10barB =+   avg10
}

avgBICbar0 = c(mBIC0abar0 / 9, mBIC1bar0 / 9, mBIC2bar0 / 9, mBIC3bar0 / 9 , mBIC4bar0 / 9, mBIC5bar0 / 9, mBIC6bar0 / 9, mBIC7bar0 / 9, 
               mBIC8bar0 / 9, mBIC9bar0 / 9, mBIC10barA /9)
avgBICbarA = c(mBIC0abarA / 9, mBIC1barA / 9, mBIC2barA / 9, mBIC3barA / 9, mBIC4barA / 9,
               mBIC5barA / 9, mBIC6barA / 9, mBIC7barA / 9, mBIC8barA / 9, mBIC9barA / 9, mBIC10barA / 9)
avgBICbarB = c(mBIC0abarB / 9, mBIC1barB / 9, mBIC2barB / 9, mBIC3barB / 9, mBIC4barB / 9, mBIC5barB / 9, 
               mBIC6barB / 9, mBIC7barB / 9, mBIC8barB / 9, mBIC9barB /9, mBIC10barB / 9 )

x_time = c(0:10)
plot(x_time, avgBICbar0, xlab="DataSet Number", ylab="Average BIC of 9 trials", main="Average BIC over all Datasets", col="red", cex = 1.5, pch=16)
lines(c(0,10),c(mean(avgBICbar0),mean(avgBICbar0)) , col="red")
points(x_time, avgBICbarA, col="blue", cex = 1.5, pch=16)
lines(c(0,10),c(mean(avgBICbarA),mean(avgBICbarA)) , col="blue")
points(x_time, avgBICbarB, col="green", cex = 1.5, pch=16)
lines(c(0,10),c(mean(avgBICbarB),mean(avgBICbarB)) , col="green")
legend(7.5, 60, legend=c("bar0", "barA", "barB"),
       col=c("red", "blue", "green"), lty=1:1, cex=1)
