setwd("/Users/sarah/REU2018/test_Cases")
pelican<-read.csv("pacificBrownPelican.csv")
pelican<-pelican[-which(pelican$NumberByPartyHours == 0),]

plot(pelican$NumberByPartyHours~pelican$Count_y, col="brown", pch=19, main="Pacific Brown Pelican Population: 1939 to 2017", ylab="Year (since 1900", xlab="Individuals per Party Hour")
lines(pelican$Count_y, pelican$NumberByPartyHours, col="brown")
points(c(39:117), fitted(arima(pelican$NumberByPartyHours, order=c(3,0,0))), col="green3", pch=19)
lines(c(39:117), fitted(arima(pelican$NumberByPartyHours, order=c(3,0,0))), col="green3")

pelican_bkpts<-bai_perron(pelican$NumberByPartyHours, pelican$Count_yr, "ar", "order=3", 5, 0.1, 3)
pelican_bkpts

pelican_result<-baar(pelican_bkpts$Breakpoints, pelican$Count_yr, pelican$NumberByPartyHours, 3000, 500, jump=0.25, ar=3)


pelican_finbkpts<-NULL

for(i in 1:ncol(pelican_result$Breakpoints)){

	pelican_finbkpts<-c(pelican_finbkpts, na.omit(pelican_result$Breakpoints[,i]), recursive=T)

}

hist(pelican_result$NumBkpts, xlim=c(1,6), breaks=c(1:6), ylim=c(0,3000), right=F, xlab="Number of Breaks", ylab="Number of Iterations (Out of 3000)", main="Number of Breakpoints for Brown Pelicans", col="brown")
hist(pelican_finbkpts, breaks=78, xlim=c(1,78), ylim=c(0,3000), right=F, xlab="Years Since 1939", ylab="Number of Iterations (Out of 3000)", main="Breakpoint Locations for Brown Pelicans", col="brown")

plot(pelican$NumberByPartyHours~pelican$Count_y, xlab="Year (since 1900)", ylab="Individuals per Party Hour", main="Pacific Brown Pelican Population: 1939 to 2017", col="brown", pch=19)

fits_to_use = pelican_result$Fits[which(pelican_result$Breakpoints[,1] == 11 & pelican_result$NumBkpts == 1),]
points(c(39:49),colMeans(fits_to_use)[1:11],col="green3", pch=19)

points(c(50:117),colMeans(fits_to_use)[12:79],col="blue", pch=19)
