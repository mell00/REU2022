#analysis data obtained from move simulations
bar0_climate = readRDS("bar0_climate.RData")

bar1_climate = readRDS("bar1_climate.RData")

bar2_climate = readRDS("bar2_climate.RData")


#---------------------------------MSE Averages --------------------------------#
mCbar0 = mean(colMeans(bar0_climate$MSE))
mCbar1 = mean(colMeans(bar1_climate$MSE))
mCbar2 = mean(colMeans(bar2_climate$MSE))

sdCbar0 = mean(apply(bar0_climate$MSE, 1, sd))
sdCbar1 = mean(apply(bar1_climate$MSE,1,sd))
sdCbar2 = mean(apply(bar2_climate$MSE,1,sd))


#all!
x_time = c(0:10)
plot(0, mCbar0, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE over all Datasets", col="red", ylim=c(0,.02), xlim=c(0,2), cex = 1, pch=16)

lines(c(0,0),c(mCbar0-sdCbar0*2,mCbar0+sdCbar0*2), col="red",lty=1)
points(1, mCbar1, col="orange", cex = 1, pch=16)
lines(c(1,1),c(mCbar1-sdCbar1*2,mCbar1+sdCbar1*2), col="orange",lty=1)
points(2, mCbar2, col="yellow2", cex = 1, pch=16)
lines(c(2,2),c(mCbar2-sdCbar2*2,mCbar2+sdCbar2*2), col="yellow2",lty=1)



#confidence intervals 
plot(NULL, ylim=c(0,15), xlim=c(0,10), xlab="DataSet Number", ylab="Average MSE", main="Average MSE over all Datasets")
lines(c(0,10),c(mean(avgbar0) + mean(sdbar0*2),mean(avgbar0) + mean(sdbar0*2)) , col="red", lty=2)
lines(c(0,10),c(mean(avgbar0),mean(avgbar0)) , col="red")
lines(c(0,10),c(mean(avgbar0)- mean(sdbar0*2),mean(avgbar0)- mean(sdbar0*2)) , col="red",lty=2)

lines(c(0,10),c(mean(avgbar1)+mean(sdbar1*2) ,mean(avgbar1)+mean(sdbar1*2)) , col="orange", lty=2)
lines(c(0,10),c(mean(avgbar1),mean(avgbar1)) , col="orange")
lines(c(0,10),c(mean(avgbar1)-mean(sdbar1*2) ,mean(avgbar1)-mean(sdbar1*2)) , col="orange", lty=2)

lines(c(0,10),c(mean(avgbar2)+mean(sdbar2*2),mean(avgbar2)+mean(sdbar2*2)) , col="yellow2", lty=2)
lines(c(0,10),c(mean(avgbar2),mean(avgbar2)) , col="yellow2")
lines(c(0,10),c(mean(avgbar2)-mean(sdbar2*2),mean(avgbar2)-mean(sdbar2*2)) , col="yellow2", lty=2)

lines(c(0,10),c(mean(avgbar3)+mean(sdbar3*2),mean(avgbar3)+mean(sdbar3*2)) , col="green", lty=2)
lines(c(0,10),c(mean(avgbar3),mean(avgbar3)) , col="green")
lines(c(0,10),c(mean(avgbar3)-mean(sdbar3*2),mean(avgbar3)-mean(sdbar3*2)) , col="green", lty=2)

lines(c(0,10),c(mean(avgbar4)+mean(sdbar4*2),mean(avgbar4)+mean(sdbar4*2)) , col="green3", lty=2)
lines(c(0,10),c(mean(avgbar4),mean(avgbar4)) , col="green3")
lines(c(0,10),c(mean(avgbar4)-mean(sdbar4*2),mean(avgbar4)-mean(sdbar4*2)) , col="green3", lty=2)

lines(c(0,10),c(mean(avgbar5)+mean(sdbar5*2),mean(avgbar5)+mean(sdbar5*2)) , col="lightblue",lty=2)
lines(c(0,10),c(mean(avgbar5),mean(avgbar5)) , col="lightblue")
lines(c(0,10),c(mean(avgbar5)-mean(sdbar5*2),mean(avgbar5)-mean(sdbar5*2)) , col="lightblue",lty=2)

lines(c(0,10),c(mean(avgbar6)+mean(sdbar6*2),mean(avgbar6)+mean(sdbar6*2)) , col="blue", lty=2)
lines(c(0,10),c(mean(avgbar6),mean(avgbar6)) , col="blue")
lines(c(0,10),c(mean(avgbar6)-mean(sdbar6*2),mean(avgbar6)-mean(sdbar6*2)) , col="blue", lty=2)

lines(c(0,10),c(mean(avgbar7)+mean(sdbar7*2),mean(avgbar7)+mean(sdbar7*2)) , col="purple",lty=2)
lines(c(0,10),c(mean(avgbar7),mean(avgbar7)) , col="purple")
lines(c(0,10),c(mean(avgbar7)-mean(sdbar7*2),mean(avgbar7)-mean(sdbar7*2)) , col="purple",lty=2)

lines(c(0,10),c(mean(avgbar8)+mean(sdbar8*2),mean(avgbar8)+mean(sdbar8*2)) , col="pink",lty=2)
lines(c(0,10),c(mean(avgbar8),mean(avgbar8)) , col="pink")
lines(c(0,10),c(mean(avgbar8)-mean(sdbar8*2),mean(avgbar8)-mean(sdbar8*2)) , col="pink",lty=2)

legend(8.5, 10.5, legend=c("bar0", "bar1", "bar2","bar3","bar4","bar5","bar6","bar7","bar8"),
       col=c("red","orange","yellow2","green","green3","lightblue" ,"blue","purple","pink"), lty=1:1, cex=.7)


#confidence intervals 
plot(NULL, ylim=c(0,15), xlim=c(0,10), xlab="DataSet Number", ylab="Average MSE", main="Average MSE over all Datasets")
lines(c(0,10),c(mean(avgbar0) + mean(sdbar0*2),mean(avgbar0) + mean(sdbar0*2)) , col="red", lty=2)
lines(c(0,10),c(mean(avgbar0),mean(avgbar0)) , col="red")
lines(c(0,10),c(mean(avgbar0)- mean(sdbar0*2),mean(avgbar0)- mean(sdbar0*2)) , col="red",lty=2)

lines(c(0,10),c(mean(avgbar5)+mean(sdbar5*2),mean(avgbar5)+mean(sdbar5*2)) , col="lightblue",lty=2)
lines(c(0,10),c(mean(avgbar5),mean(avgbar5)) , col="lightblue")
lines(c(0,10),c(mean(avgbar5)-mean(sdbar5*2),mean(avgbar5)-mean(sdbar5*2)) , col="lightblue",lty=2)

legend(8.5, 4, legend=c("bar0","bar5"),
       col=c("red","lightblue" ), lty=1:1, cex=.7)





#Random Addition
x_time = c(0:10)
par(mfrow=c(1,3))
plot(x_time, avgbar0, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE Random Addition", col="red", ylim=c(0,55), cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar0),median(avgbar0)) , col="red")
points(x_time, avgbar3, col="green", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar3),median(avgbar3)) , col="green")
points(x_time, avgbar4, col="green3", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar4),median(avgbar4)) , col="green3")
legend(7.5, 50, legend=c("bar0", "bar3","bar4"),
       col=c("red","green","green3"), lty=1:1, cex=.5)

#Interval Addition 
x_time = c(0:10)
plot(x_time, avgbar1, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE Interval Addition", col="orange", ylim=c(0,55), cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar1),median(avgbar1)) , col="orange")
points(x_time, avgbar5, col="lightblue", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar5),median(avgbar5)) , col="lightblue")
points(x_time, avgbar6, col="blue", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar6),median(avgbar6)) , col="blue")
legend(7.5, 50, legend=c( "bar1","bar5","bar6"),
       col=c("orange","lightblue" ,"blue"), lty=1:1, cex=.5)


#Scoring Addition
x_time = c(0:10)
plot(x_time, avgbar2, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE Scoring Addition", col="yellow2", ylim=c(0,55), cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar2),median(avgbar2)) , col="yellow2")
points(x_time, avgbar7, col="purple", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar7),median(avgbar7)) , col="purple")
points(x_time, avgbar8, col="pink", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar8),median(avgbar8)) , col="pink")
legend(7.5, 50, legend=c( "bar2","bar7","bar8"),
       col=c("yellow2","purple","pink"), lty=1:1, cex=.5)

#Random Subtraction 
x_time = c(0:10)
plot(x_time, avgbar0, xlab="DataSet Number", ylab="Average MSE Random Subtraction", main="Average MSE Random Subtraction", col="red", ylim=c(0,55), cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar0),median(avgbar0)) , col="red")
points(x_time, avgbar1, col="orange", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar1),median(avgbar1)) , col="orange")
points(x_time, avgbar2, col="yellow2", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar2),median(avgbar2)) , col="yellow2")
legend(7.5, 50, legend=c("bar0", "bar1", "bar2"),
       col=c("red","orange","yellow2"), lty=1:1, cex=.5)


#Interval Subtraction
x_time = c(0:10)
plot(x_time, avgbar3, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE Interval Subtraction", col="green", ylim=c(0,55), cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar3),median(avgbar3)) , col="green")
points(x_time, avgbar5, col="lightblue", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar5),median(avgbar5)) , col="lightblue")
points(x_time, avgbar8, col="pink", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar8),median(avgbar8)) , col="pink")
legend(7.5, 50, legend=c("bar3","bar5","bar8"),
       col=c("green","lightblue","pink"), lty=1:1, cex=.5)


#Scoring Subtraction
x_time = c(0:10)
plot(x_time, avgbar4, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE Scoring Subtraction", col="green3", ylim=c(0,55), cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar4),median(avgbar4)) , col="green3")
points(x_time, avgbar6, col="blue", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar6),median(avgbar6)) , col="blue")
points(x_time, avgbar7, col="purple", cex = 1.5, pch=16)
lines(c(0,10),c(median(avgbar7),median(avgbar7)) , col="purple")
legend(7.5, 50, legend=c("bar4","bar6","bar7"),
       col=c("green3" ,"blue","purple"), lty=1:1, cex=.5)

par(mfrow=c(1,1))


par(mfrow=c(3,3))
plot(x_time, avgbar0, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar0", col="red", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar0+sdbar0*2, col="red", cex = 1, pch=16)
points(x_time, avgbar0-sdbar0*2, col="red", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar0[i+1]-sdbar0[i+1]*2,avgbar0[i+1]+sdbar0[i+1]*2), col="red",lty=2)
}
plot(x_time, avgbar1, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar1",col="orange", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar1+sdbar1*2, col="orange", cex = 1, pch=16)
points(x_time, avgbar1-sdbar1*2, col="orange", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar1[i+1]-sdbar1[i+1]*2,avgbar1[i+1]+sdbar1[i+1]*2), col="orange",lty=2)
}
plot(x_time, avgbar2, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar2",col="yellow2", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar2+sdbar2*2, col="yellow2", cex = 1, pch=16)
points(x_time, avgbar2-sdbar2*2, col="yellow2", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar2[i+1]-sdbar2[i+1]*2,avgbar2[i+1]+sdbar2[i+1]*2), col="yellow2",lty=2)
}

plot(x_time, avgbar3, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar3",col="green", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar3+sdbar3*2, col="green", cex = 1, pch=16)
points(x_time, avgbar3-sdbar3*2, col="green", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar3[i+1]-sdbar3[i+1]*2,avgbar3[i+1]+sdbar3[i+1]*2), col="green",lty=2)
}

plot(x_time, avgbar4, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar4",col="green3", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar4+sdbar4*2, col="green3", cex = 1, pch=16)
points(x_time, avgbar4-sdbar4*2, col="green3", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar4[i+1]-sdbar4[i+1]*2,avgbar4[i+1]+sdbar4[i+1]*2), col="green3",lty=2)
}

plot(x_time, avgbar5, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar5",col="lightblue", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar5+sdbar5*2, col="lightblue", cex = 1, pch=16)
points(x_time, avgbar5-sdbar5*2, col="lightblue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar5[i+1]-sdbar5[i+1]*2,avgbar5[i+1]+sdbar5[i+1]*2), col="lightblue",lty=2)
}
plot(x_time, avgbar6, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar6",col="blue", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar6+sdbar6*2, col="blue", cex = 1, pch=16)
points(x_time, avgbar6-sdbar6*2, col="blue", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar6[i+1]-sdbar6[i+1]*2,avgbar6[i+1]+sdbar6[i+1]*2), col="blue",lty=2)
}
plot(x_time, avgbar7, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar7",col="purple", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar7+sdbar7*2, col="purple", cex = 1, pch=16)
points(x_time, avgbar7-sdbar7*2, col="purple", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar7[i+1]-sdbar7[i+1]*2,avgbar7[i+1]+sdbar7[i+1]*2), col="purple",lty=2)
}
plot(x_time, avgbar8, xlab="DataSet Number", ylab="Average MSE of 9 trials", main="Average MSE bar8",col="pink", ylim=c(0,70), cex = 1.5, pch=16)
points(x_time, avgbar8+sdbar8*2, col="pink", cex = 1, pch=16)
points(x_time, avgbar8-sdbar8*2, col="pink", cex = 1, pch=16)
for(i in 0:10) {
  lines(c(i,i),c(avgbar8[i+1]-sdbar8[i+1]*2,avgbar8[i+1]+sdbar8[i+1]*2), col="pink",lty=2)
}

#-------------------------------each dataset--------------------------#
par(mfrow=c(3,4))
plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 0a")
points(0,avgbar0[1],  col="red", cex = 1, pch=16)
points(1,avgbar1[1],  col="orange", cex = 1, pch=16)
points(2,avgbar2[1],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[1],  col="green", cex = 1, pch=16)
points(4,avgbar4[1],  col="green2", cex = 1, pch=16)
points(5,avgbar5[1],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[1],  col="blue", cex = 1, pch=16)
points(7,avgbar7[1],  col="purple", cex = 1, pch=16)
points(8,avgbar8[1],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[1]-sdbar0[1]*2,avgbar0[1]+sdbar0[1]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[1]-sdbar1[1]*2,avgbar1[1]+sdbar1[1]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[1]-sdbar2[1]*2,avgbar2[1]+sdbar2[1]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[1]-sdbar3[1]*2,avgbar3[1]+sdbar3[1]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[1]-sdbar4[1]*2,avgbar4[1]+sdbar4[1]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[1]-sdbar5[1]*2,avgbar5[1]+sdbar5[1]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[1]-sdbar6[1]*2,avgbar6[1]+sdbar6[1]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[1]-sdbar7[1]*2,avgbar7[1]+sdbar7[1]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[1]-sdbar8[1]*2,avgbar8[1]+sdbar8[1]*2), col="pink",lty=1)


plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 1")
points(0,avgbar0[2],  col="red", cex = 1, pch=16)
points(1,avgbar1[2],  col="orange", cex = 1, pch=16)
points(2,avgbar2[2],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[2],  col="green", cex = 1, pch=16)
points(4,avgbar4[2],  col="green2", cex = 1, pch=16)
points(5,avgbar5[2],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[2],  col="blue", cex = 1, pch=16)
points(7,avgbar7[2],  col="purple", cex = 1, pch=16)
points(8,avgbar8[2],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[2]-sdbar0[2]*2,avgbar0[2]+sdbar0[2]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[2]-sdbar1[2]*2,avgbar1[2]+sdbar1[2]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[2]-sdbar2[2]*2,avgbar2[2]+sdbar2[2]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[2]-sdbar3[2]*2,avgbar3[2]+sdbar3[2]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[2]-sdbar4[2]*2,avgbar4[2]+sdbar4[2]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[2]-sdbar5[2]*2,avgbar5[2]+sdbar5[2]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[2]-sdbar6[2]*2,avgbar6[2]+sdbar6[2]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[2]-sdbar7[2]*2,avgbar7[2]+sdbar7[2]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[2]-sdbar8[2]*2,avgbar8[2]+sdbar8[2]*2), col="pink",lty=1)

plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 2")
points(0,avgbar0[3],  col="red", cex = 1, pch=16)
points(1,avgbar1[3],  col="orange", cex = 1, pch=16)
points(2,avgbar2[3],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[3],  col="green", cex = 1, pch=16)
points(4,avgbar4[3],  col="green2", cex = 1, pch=16)
points(5,avgbar5[3],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[3],  col="blue", cex = 1, pch=16)
points(7,avgbar7[3],  col="purple", cex = 1, pch=16)
points(8,avgbar8[3],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[3]-sdbar0[3]*2,avgbar0[3]+sdbar0[3]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[3]-sdbar1[3]*2,avgbar1[3]+sdbar1[3]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[3]-sdbar2[3]*2,avgbar2[3]+sdbar2[3]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[3]-sdbar3[3]*2,avgbar3[3]+sdbar3[3]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[3]-sdbar4[3]*2,avgbar4[3]+sdbar4[3]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[3]-sdbar5[3]*2,avgbar5[3]+sdbar5[3]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[3]-sdbar6[3]*2,avgbar6[3]+sdbar6[3]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[3]-sdbar7[3]*2,avgbar7[3]+sdbar7[3]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[3]-sdbar8[3]*2,avgbar8[3]+sdbar8[3]*2), col="pink",lty=1)


plot(NULL, ylim=c(0,55), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 3")
points(0,avgbar0[4],  col="red", cex = 1, pch=16)
points(1,avgbar1[4],  col="orange", cex = 1, pch=16)
points(2,avgbar2[4],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[4],  col="green", cex = 1, pch=16)
points(4,avgbar4[4],  col="green2", cex = 1, pch=16)
points(5,avgbar5[4],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[4],  col="blue", cex = 1, pch=16)
points(7,avgbar7[4],  col="purple", cex = 1, pch=16)
points(8,avgbar8[4],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[4]-sdbar0[4]*2,avgbar0[4]+sdbar0[4]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[4]-sdbar1[4]*2,avgbar1[4]+sdbar1[4]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[4]-sdbar2[4]*2,avgbar2[4]+sdbar2[4]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[4]-sdbar3[4]*2,avgbar3[4]+sdbar3[4]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[4]-sdbar4[4]*2,avgbar4[4]+sdbar4[4]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[4]-sdbar5[4]*2,avgbar5[4]+sdbar5[4]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[4]-sdbar6[4]*2,avgbar6[4]+sdbar6[4]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[4]-sdbar7[4]*2,avgbar7[4]+sdbar7[4]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[4]-sdbar8[4]*2,avgbar8[4]+sdbar8[4]*2), col="pink",lty=1)

plot(NULL, ylim=c(0,70), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 4")
points(0,avgbar0[5],  col="red", cex = 1, pch=16)
points(1,avgbar1[5],  col="orange", cex = 1, pch=16)
points(2,avgbar2[5],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[5],  col="green", cex = 1, pch=16)
points(4,avgbar4[5],  col="green2", cex = 1, pch=16)
points(5,avgbar5[5],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[5],  col="blue", cex = 1, pch=16)
points(7,avgbar7[5],  col="purple", cex = 1, pch=16)
points(8,avgbar8[5],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[5]-sdbar0[5]*2,avgbar0[5]+sdbar0[5]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[5]-sdbar1[5]*2,avgbar1[5]+sdbar1[5]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[5]-sdbar2[5]*2,avgbar2[5]+sdbar2[5]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[5]-sdbar3[5]*2,avgbar3[5]+sdbar3[5]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[5]-sdbar4[5]*2,avgbar4[5]+sdbar4[5]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[5]-sdbar5[5]*2,avgbar5[5]+sdbar5[5]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[5]-sdbar6[5]*2,avgbar6[5]+sdbar6[5]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[5]-sdbar7[5]*2,avgbar7[5]+sdbar7[5]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[5]-sdbar8[5]*2,avgbar8[5]+sdbar8[5]*2), col="pink",lty=1)


plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Datasets 5")
points(0,avgbar0[6],  col="red", cex = 1, pch=16)
points(1,avgbar1[6],  col="orange", cex = 1, pch=16)
points(2,avgbar2[6],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[6],  col="green", cex = 1, pch=16)
points(4,avgbar4[6],  col="green2", cex = 1, pch=16)
points(5,avgbar5[6],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[6],  col="blue", cex = 1, pch=16)
points(7,avgbar7[6],  col="purple", cex = 1, pch=16)
points(8,avgbar8[6],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[6]-sdbar0[6]*2,avgbar0[6]+sdbar0[6]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[6]-sdbar1[6]*2,avgbar1[6]+sdbar1[6]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[6]-sdbar2[6]*2,avgbar2[6]+sdbar2[6]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[6]-sdbar3[6]*2,avgbar3[6]+sdbar3[6]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[6]-sdbar4[6]*2,avgbar4[6]+sdbar4[6]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[6]-sdbar5[6]*2,avgbar5[6]+sdbar5[6]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[6]-sdbar6[6]*2,avgbar6[6]+sdbar6[6]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[6]-sdbar7[6]*2,avgbar7[6]+sdbar7[6]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[6]-sdbar8[6]*2,avgbar8[6]+sdbar8[6]*2), col="pink",lty=1)

plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 6")
points(0,avgbar0[7],  col="red", cex = 1, pch=16)
points(1,avgbar1[7],  col="orange", cex = 1, pch=16)
points(2,avgbar2[7],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[7],  col="green", cex = 1, pch=16)
points(4,avgbar4[7],  col="green2", cex = 1, pch=16)
points(5,avgbar5[7],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[7],  col="blue", cex = 1, pch=16)
points(7,avgbar7[7],  col="purple", cex = 1, pch=16)
points(8,avgbar8[7],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[7]-sdbar0[7]*2,avgbar0[7]+sdbar0[7]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[7]-sdbar1[7]*2,avgbar1[7]+sdbar1[7]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[7]-sdbar2[7]*2,avgbar2[7]+sdbar2[7]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[7]-sdbar3[7]*2,avgbar3[7]+sdbar3[7]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[7]-sdbar4[7]*2,avgbar4[7]+sdbar4[7]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[7]-sdbar5[7]*2,avgbar5[7]+sdbar5[7]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[7]-sdbar6[7]*2,avgbar6[7]+sdbar6[7]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[7]-sdbar7[7]*2,avgbar7[7]+sdbar7[7]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[7]-sdbar8[7]*2,avgbar8[7]+sdbar8[7]*2), col="pink",lty=1)


plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 7")
points(0,avgbar0[8],  col="red", cex = 1, pch=16)
points(1,avgbar1[8],  col="orange", cex = 1, pch=16)
points(2,avgbar2[8],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[8],  col="green", cex = 1, pch=16)
points(4,avgbar4[8],  col="green2", cex = 1, pch=16)
points(5,avgbar5[8],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[8],  col="blue", cex = 1, pch=16)
points(7,avgbar7[8],  col="purple", cex = 1, pch=16)
points(8,avgbar8[8],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[8]-sdbar0[8]*2,avgbar0[8]+sdbar0[8]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[8]-sdbar1[8]*2,avgbar1[8]+sdbar1[8]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[8]-sdbar2[8]*2,avgbar2[8]+sdbar2[8]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[8]-sdbar3[8]*2,avgbar3[8]+sdbar3[8]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[8]-sdbar4[8]*2,avgbar4[8]+sdbar4[8]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[8]-sdbar5[8]*2,avgbar5[8]+sdbar5[8]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[8]-sdbar6[8]*2,avgbar6[8]+sdbar6[8]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[8]-sdbar7[8]*2,avgbar7[8]+sdbar7[8]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[8]-sdbar8[8]*2,avgbar8[8]+sdbar8[8]*2), col="pink",lty=1)


plot(NULL, ylim=c(0,2), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 8")
points(0,avgbar0[9],  col="red", cex = 1, pch=16)
points(1,avgbar1[9],  col="orange", cex = 1, pch=16)
points(2,avgbar2[9],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[9],  col="green", cex = 1, pch=16)
points(4,avgbar4[9],  col="green2", cex = 1, pch=16)
points(5,avgbar5[9],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[9],  col="blue", cex = 1, pch=16)
points(7,avgbar7[9],  col="purple", cex = 1, pch=16)
points(8,avgbar8[9],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[9]-sdbar0[9]*2,avgbar0[9]+sdbar0[9]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[9]-sdbar1[9]*2,avgbar1[9]+sdbar1[9]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[9]-sdbar2[9]*2,avgbar2[9]+sdbar2[9]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[9]-sdbar3[9]*2,avgbar3[9]+sdbar3[9]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[9]-sdbar4[9]*2,avgbar4[9]+sdbar4[9]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[9]-sdbar5[9]*2,avgbar5[9]+sdbar5[9]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[9]-sdbar6[9]*2,avgbar6[9]+sdbar6[9]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[9]-sdbar7[9]*2,avgbar7[9]+sdbar7[9]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[9]-sdbar8[9]*2,avgbar8[9]+sdbar8[9]*2), col="pink",lty=1)

plot(NULL, ylim=c(0,30), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 9")
points(0,avgbar0[10],  col="red", cex = 1, pch=16)
points(1,avgbar1[10],  col="orange", cex = 1, pch=16)
points(2,avgbar2[10],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[10],  col="green", cex = 1, pch=16)
points(4,avgbar4[10],  col="green2", cex = 1, pch=16)
points(5,avgbar5[10],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[10],  col="blue", cex = 1, pch=16)
points(7,avgbar7[10],  col="purple", cex = 1, pch=16)
points(8,avgbar8[10],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[10]-sdbar0[10]*2,avgbar0[10]+sdbar0[10]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[10]-sdbar1[10]*2,avgbar1[10]+sdbar1[10]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[10]-sdbar2[10]*2,avgbar2[10]+sdbar2[10]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[10]-sdbar3[10]*2,avgbar3[10]+sdbar3[10]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[10]-sdbar4[10]*2,avgbar4[10]+sdbar4[10]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[10]-sdbar5[10]*2,avgbar5[10]+sdbar5[10]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[10]-sdbar6[10]*2,avgbar6[10]+sdbar6[10]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[10]-sdbar7[10]*2,avgbar7[10]+sdbar7[10]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[10]-sdbar8[10]*2,avgbar8[10]+sdbar8[10]*2), col="pink",lty=1)

plot(NULL, ylim=c(0,20), xlim=c(0,8), xlab="Bar Number", ylab="Average MSE", main="Average MSE Dataset 10")
points(0,avgbar0[11],  col="red", cex = 1, pch=16)
points(1,avgbar1[11],  col="orange", cex = 1, pch=16)
points(2,avgbar2[11],  col="yellow2", cex = 1, pch=16)
points(3,avgbar3[11],  col="green", cex = 1, pch=16)
points(4,avgbar4[11],  col="green2", cex = 1, pch=16)
points(5,avgbar5[11],  col="lightblue", cex = 1, pch=16)
points(6,avgbar6[11],  col="blue", cex = 1, pch=16)
points(7,avgbar7[11],  col="purple", cex = 1, pch=16)
points(8,avgbar8[11],  col="pink", cex = 1, pch=16)
lines(c(0,0),c(avgbar0[11]-sdbar0[11]*2,avgbar0[11]+sdbar0[11]*2), col="red",lty=1)
lines(c(1,1),c(avgbar1[11]-sdbar1[11]*2,avgbar1[11]+sdbar1[11]*2), col="orange",lty=1)
lines(c(2,2),c(avgbar2[11]-sdbar2[11]*2,avgbar2[11]+sdbar2[11]*2), col="yellow2",lty=1)
lines(c(3,3),c(avgbar3[11]-sdbar3[11]*2,avgbar3[11]+sdbar3[11]*2), col="green",lty=1)
lines(c(4,4),c(avgbar4[11]-sdbar4[11]*2,avgbar4[11]+sdbar4[11]*2), col="green2",lty=1)
lines(c(5,5),c(avgbar5[11]-sdbar5[11]*2,avgbar5[11]+sdbar5[11]*2), col="lightblue",lty=1)
lines(c(6,6),c(avgbar6[11]-sdbar6[11]*2,avgbar6[11]+sdbar6[11]*2), col="blue",lty=1)
lines(c(7,7),c(avgbar7[11]-sdbar7[11]*2,avgbar7[11]+sdbar7[11]*2), col="purple",lty=1)
lines(c(8,8),c(avgbar8[11]-sdbar8[11]*2,avgbar8[11]+sdbar8[11]*2), col="pink",lty=1)









