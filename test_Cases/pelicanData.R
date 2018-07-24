setwd("/Users/sarah/REU2018/test_Cases")
pelican<-read.csv("pacificBrownPelican.csv")
pelican<-pelican[-which(pelican$NumberByPartyHours == 0),]

cols1<-c('#006e82', '#8214a0', '#005ac8', '#00a0fa', '#fa78fa', '#14d2dc', '#aa0a3c', '#fa7850', '#0ab45a', '#f0f032', '#a0fa82', '#fae6be')

pelican$Count_yr = pelican$Count_yr + 1899
plot(pelican$NumberByPartyHours~pelican$Count_yr, col="#aa0a3c", xaxp=c(1940, 2015, 5), pch=19, main="Pacific Brown Pelican Population: 1938 to 2016", xlab="Year", ylab="Individuals per Party Hour")
lines(pelican$Count_yr, pelican$NumberByPartyHours, col="#aa0a3c")
library(“forecast”)
points(c(1938:2016), fitted(arima(pelican$NumberByPartyHours, order=c(3,0,0))), col="#8214a0", pch=15)
lines(c(1938:2016), fitted(arima(pelican$NumberByPartyHours, order=c(3,0,0))), col="#8214a0", lty=3)
BIC(arima(pelican$NumberByPartyHours, order=c(3,0,0)))

pelican_bkpts<-bai_perron.ar(pelican$Count_yr, pelican$NumberByPartyHours, order=3, max_breaks=1)
pelican_bkpts<-breakpoints(pelican$NumberByPartyHours~pelican$Count_yr)

pelican_result<-baar(pelican_bkpts$Breakpoints, pelican$Count_yr, pelican$NumberByPartyHours, 10000, 1500, jump=0.25, ar=3)
pelican_result<-balr(pelican_bkpts$breakpoints, pelican$Count_yr, pelican$NumberByPartyHours, 10000, 1500, jump=0.25)
#saveRDS(pelican_result, file="casestudy_data.RData")

pelican_result<-readRDS("casestudy_data.RData")
length(which(pelican_result$Breakpoints[,1] == 11)) +
length(which(pelican_result$Breakpoints[,1] == 12)) +
length(which(pelican_result$Breakpoints[,1] == 13)) +
length(which(pelican_result$Breakpoints[,1] == 14))
length(which(pelican_result$Breakpoints[,2] == 70)) +
length(which(pelican_result$Breakpoints[,2] == 71)) +
length(which(pelican_result$Breakpoints[,2] == 72))

pelican_finbkpts<-NULL

for(i in 1:ncol(pelican_result$Breakpoints)){

	pelican_finbkpts<-c(pelican_finbkpts, na.omit(pelican_result$Breakpoints[,i]), recursive=T)

}

pelican_finbkpts = pelican_finbkpts+1937

hist(pelican_result$NumBkpts, breaks=c(0.5,1.5,2.5), xlim=c(0.5,2.5), ylim=c(0,10000), right=F, xlab="Number of Breaks", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Number", col="#aa0a3c")
hist(pelican_finbkpts, breaks=seq(1938.5,2016.5,1), xlim=c(1940,2015), xaxp=c(1940, 2015, 5), ylim=c(0,10000), right=F, xlab="Year", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Locations", col="#aa0a3c")

fits_to_use = pelican_result$BIC[which(pelican_result$Breakpoints[,1] == 11 & pelican_result$NumBkpts == 1),]
points(c(1938:1948),colMeans(fits_to_use)[1:11], col="#00a0fa", pch=17)
lines(c(1938:1948),colMeans(fits_to_use)[1:11], col="#00a0fa", lty=2)
points(c(1949:2016),colMeans(fits_to_use)[12:79], col="#0ab45a", pch=18)
lines(c(1949:2016),colMeans(fits_to_use)[12:79], col="#0ab45a", lty=2)
mean(pelican_result$BIC[which(pelican_result$Breakpoints[,1] == 11 & pelican_result$NumBkpts == 1),])
