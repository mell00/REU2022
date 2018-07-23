setwd("/Users/sarah/REU2018/test_Cases")
cpi<-read.csv("CPIAUCSL.csv")
means<-list()
for(i in 1:floor(length(cpi$CPIAUCSL)/3)){

means<-c(means, mean(cpi$CPIAUCSL[(i*3-2):(i*3)]), recursive=T)

}

rates<-list()

for(i in 2:length(means)){

rates<-c(rates, ((((means[[i]]/means[[i-1]])^4)-1)*100), recursive=T)

}

bp_cpi<-bai_perron.ar(c(1:length(rates)), rates, order=1, max_breaks=3)
baar_cpi<-baar(bp_cpi$Breakpoints, c(1:length(rates)), rates, 10000, 500, jump=0.25, ar=1, progress=T)

finbkpts<-NULL

for(i in 1:ncol(baar_cpi$Breakpoints)){

	finbkpts<-c(finbkpts, na.omit(baar_cpi$Breakpoints[,i]), recursive=T)

}

hist(finbkpts, breaks=254, ylim=c(0,10000), right=F, xlab="Quarters (since 1947)", ylab="Number of Iterations (Out of 10000)", main="Distribution of Breakpoint Locations", col="green3")

