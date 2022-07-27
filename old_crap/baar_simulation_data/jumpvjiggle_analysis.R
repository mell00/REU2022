
data3_alljump = readRDS("data3_alljump.RData")
alljump = data3_alljump

lim = 500

par(mfrow=c(3,4))

for(a in 1:11){

finbkpts<-NULL

if(is.atomic(alljump$Breakpoints[[a]]) == T){

finbkpts = alljump$Breakpoints[[a]][1:lim]

}else {
	for(i in 1:ncol(alljump$Breakpoints[[a]])){

		finbkpts<-c(finbkpts, na.omit(alljump$Breakpoints[[a]][1:lim,i]), recursive=T)

	}
}

hist(finbkpts, breaks=seq(96.5,104.5,1), xlim=c(96,105), ylim=c(0,lim), right=F, xlab="Location", ylab="Number of Iterations",
main=alljump$JumpProportion[[a]], col="brown")

}




# setwd("/Users/khaglich/Desktop/Edited REU Main/test_Cases/Old")
# pelican_alljump = readRDS("pelican_alljump.RData")
# 
# library(plyr)
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[1]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[2]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[3]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[4]][1:500,],.(X1,X2),nrow)
# unique_jump0[which.max(unique_jump0[,3]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[5]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[6]][1:500,],.(X1,X2),nrow)
# unique_jump0[which.max(unique_jump0[,3]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[7]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[8]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[9]][1:500,],.(X1,X2),nrow)
# unique_jump0[which.max(unique_jump0[,3]),]
# 
# unique_jump0<-ddply(pelican_alljump$Breakpoints[[10]][1:500,],.(X1,X2,X3),nrow)
# unique_jump0[which.max(unique_jump0[,4]),]
# 
# unique_jump100<-ddply(pelican_alljump$Breakpoints[[11]][1:500,],.(X1,X2,X3,X4),nrow)
# unique_jump100[which.max(unique_jump100[,5]),]

