pelican<-read.csv("pacificBrownPelican.csv")
pelican<-pelican[-which(pelican$NumberByPartyHours == 0),]

plot(pelican$NumberByPartyHours~pelican$Count_yr)

library("strucchange")

iteration_n = 3000

pelican_bkpts<-breakpoints(pelican$NumberByPartyHours~pelican$Count_yr, breaks=10, h=0.05)

pelican_result<-baar(pelican_bkpts$breakpoints, pelican$Count_yr, pelican$NumberByPartyHours, iteration_n, 500)

pelican_finbkpts<-NULL

for(i in 1:ncol(pelican_result$Breakpoints)){

	pelican_finbkpts<-c(pelican_finbkpts, na.omit(pelican_result$Breakpoints[,i]), recursive=T)

}

hist(pelican_finbkpts, breaks=78, xlim=c(1,78), ylim=c(0,3000), right=F, xlab="Years Since 1939", ylab=paste("Number of Occurences Out of ", iteration_n, " Iterations"), main="Breakpoint Locations for Brown Pelicans")