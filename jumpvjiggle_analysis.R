setwd("/Users/sarah/REU2018/test_Cases")
pelican_alljump = readRDS("pelican_alljump.RData")

lim = 5000

par(mfrow=c(3,4))

for(a in 1:11){

pelican_finbkpts<-NULL

for(i in 1:ncol(pelican_alljump$Breakpoints[[a]])){

	pelican_finbkpts<-c(pelican_finbkpts, na.omit(pelican_alljump$Breakpoints[[a]][1:lim,i]), recursive=T)

}

hist(pelican_finbkpts, breaks=seq(0.5,78.5,1), xlim=c(7,19), ylim=c(0,lim), right=F, xlab="Years Since 1939", ylab="Number of Iterations",
main=pelican_alljump$JumpProportion[[a]], col="brown")

}

library(plyr)
unique_jump0<-ddply(pelican_alljump$Breakpoints[[1]][1:500,],.(X1,X2,X3),nrow)
unique_jump0[which.max(unique_jump0[,4]),]
unique_jump100<-ddply(pelican_alljump$Breakpoints[[11]][1:500,],.(X1,X2,X3,X4),nrow)
unique_jump100[which.max(unique_jump100[,5]),]