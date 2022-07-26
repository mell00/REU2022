library(devtools)
#install_version("FitAR", version = "1.94", repos = "http://cran.us.r-project.org")
#library(FitAR)
library(strucchange)
library(stats)
library(MASS)
par(mar=c(1,1,1,1))
setwd("/Users/mellm/github/REU2022/test_Cases")
pelican<-read.csv("pacificBrownPelican.csv")
pelican<-pelican[-which(pelican$NumberByPartyHours == 0),]
pelican$Count_yr = pelican$Count_yr + 1899

cols1<-c('#006e82', '#8214a0', '#005ac8', '#00a0fa', '#fa78fa', '#14d2dc', '#aa0a3c', '#fa7850', '#0ab45a', '#f0f032', '#a0fa82', '#fae6be')

library("forecast")
setwd("/Users/mellm/github/REU2022/")
source("ar_bai_perron.r")
source("data_for_trials.r")
single_model <- arima(pelican$NumberByPartyHours, order=c(3,0,0))
single_fitted <- fitted(single_model)
single_BIC <- BIC(single_model)

pelican_bkpts<-bai_perron.ar(pelican$Count_yr, pelican$NumberByPartyHours, order=3, max_breaks=1)
pelican_bkpts<-breakpoints(pelican$NumberByPartyHours~pelican$Count_yr, breaks = 1, h = 0.1)

pelican_result<-baar(pelican_bkpts$Breakpoints, pelican$Count_yr, pelican$NumberByPartyHours, 10000, 1500, jump=0.25, ar=3)
#pelican_result<-balr(pelican_bkpts$breakpoints, pelican$Count_yr, pelican$NumberByPartyHours, 10000, 1500, jump=0.25)
#saveRDS(pelican_result, file="casestudy_data2.RData")

#pelican_result<-readRDS("casestudy_data.RData")
pelican_result<-readRDS("casestudy_data2.RData")
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

beta_to_use = pelican_result$Beta[which(pelican_result$Breakpoints[,1] == 11 & pelican_result$NumBkpts == 1)]
sigma_to_use = pelican_result$Sigma[which(pelican_result$Breakpoints[,1] == 11 & pelican_result$NumBkpts == 1)]
fits_to_use = pelican_result$Fits[which(pelican_result$Breakpoints[,1] == 11 & pelican_result$NumBkpts == 1),]

lower = apply(fits_to_use, 2, quantile, probs = 0.025, na.rm = T)
upper = apply(fits_to_use, 2, quantile, probs = 0.975, na.rm = T)

par(mfrow=c(1,2))
plot(pelican$NumberByPartyHours~pelican$Count_yr, ylim=c(-1,2.5), col="#aa0a3c", xaxp=c(1940, 2015, 5), pch=19, main="A. Pacific Brown Pelican Population: Single AR(3) Fit", xlab="Year", ylab="Individuals per Party Hour")
lines(pelican$Count_yr, pelican$NumberByPartyHours, col="#aa0a3c")
points(c(1938:2016), single_fitted, col="#8214a0", pch=15)
lines(c(1938:2016), single_fitted, col="#8214a0", lty=1)

plot(pelican$NumberByPartyHours~pelican$Count_yr, ylim=c(-1,2.5), col="#aa0a3c", xaxp=c(1940, 2015, 5), pch=19, main="B. Pacific Brown Pelican Population: Fit from BAAR", xlab="Year", ylab="Individuals per Party Hour")
lines(pelican$Count_yr, pelican$NumberByPartyHours, col="#aa0a3c")
points(c(1938:1948),colMeans(fits_to_use)[1:11], col="#00a0fa", pch=17)
lines(c(1938:1948),colMeans(fits_to_use)[1:11], col="#00a0fa", lty=1)
points(c(1949:2016),colMeans(fits_to_use)[12:79], col="#0ab45a", pch=18)
lines(c(1949:2016),colMeans(fits_to_use)[12:79], col="#0ab45a", lty=1)
lines(c(1938:1948),lower[1:11], col="#00a0fa", lty=3)
lines(c(1949:2016),lower[12:79], col="#0ab45a", lty=3)
lines(c(1938:1948),upper[1:11], col="#00a0fa", lty=3)
lines(c(1949:2016),upper[12:79], col="#0ab45a", lty=3)

