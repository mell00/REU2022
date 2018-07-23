j1j0 = readRDS("pelican_jump100.RData")
j25j75 = readRDS("pelican_jump25.RData")

mean(j1j0$RunTimes)
mean(j25j75$RunTimes)

c(mean(j1j0$RunTimes)- 2*sd(j1j0$RunTimes),mean(j1j0$RunTimes)+ 2*sd(j1j0$RunTimes))
c(mean(j25j75$RunTimes) - 2*sd(j25j75$RunTimes), mean(j25j75$RunTimes) + 2*sd(j25j75$RunTimes))

plot( c(mean(j1j0$RunTimes),mean(j25j75$RunTimes) ), ylim=c(0,3))
points(c(1,1),c(mean(j1j0$RunTimes)- 2*sd(j1j0$RunTimes),mean(j1j0$RunTimes)+ 2*sd(j1j0$RunTimes)))
points(c(2,2),c(mean(j25j75$RunTimes) - 2*sd(j25j75$RunTimes), mean(j25j75$RunTimes) + 2*sd(j25j75$RunTimes)))


all = readRDS("pelican_alljump.RData")
plot(all$RunTimes, ylim=c(1,2))

par(mfrow=c(3,4))
hist(all$NumBkpts[[1]], breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[2]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[3]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[4]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[5]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[6]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[7]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[8]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[9]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[10]],breaks=c(.5,1.5,2.5,3.5,4.5))
hist(all$NumBkpts[[11]],breaks=c(.5,1.5,2.5,3.5,4.5))

