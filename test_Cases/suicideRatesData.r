par(mar=c(1,1,1,1))
setwd("/Users/mellm/github/REU2022/test_Cases")
suicide<-read.csv("suicideRatesData.csv")
attach(suicide)

cols1<-c('#006e82', '#8214a0', '#005ac8', '#00a0fa', '#fa78fa', '#14d2dc', '#aa0a3c', '#fa7850', '#0ab45a', '#f0f032', '#a0fa82', '#fae6be')

library("forecast")
setwd("/Users/mellm/github/REU2022/")
source("ar_bai_perron.r")
source("data_for_trials.r")
source("Variations_of_bars/fixedbaar1.r")
single_model <- arima(suicide$X15.24.years, order=c(3,0,0))
single_fitted <- fitted(single_model)
single_BIC <- BIC(single_model)

suicide_bkpts<-bai_perron.ar(suicide$Year, suicide$X15.24.years, order=3, max_breaks=1)
suicide_bkpts<-all_breakpoints(suicide$X15.24.years~suicide$Year)

suicide_result<-baar(suicide_bkpts$Breakpoints, suicide$Year, suicide$X15.24.years, 10000, 1500, jump=0.25, ar=3)
#suicide_result<-bama(suicide_bkpts$Breakpoints, suicide$Year, suicide$X15.24.years, 10000, 1500, jump=0.25)
saveRDS(suicide_result, file="casestudy_data3.RData")

#suicide_result<-readRDS("casestudy_data4.RData")
suicide_result<-readRDS("test_Cases/casestudy_data3.RData")
length(which(suicide_result$Breakpoints[,1] == 11)) +
  length(which(suicide_result$Breakpoints[,1] == 12)) +
  length(which(suicide_result$Breakpoints[,1] == 13)) +
  length(which(suicide_result$Breakpoints[,1] == 14))
length(which(suicide_result$Breakpoints[,2] == 70)) +
  length(which(suicide_result$Breakpoints[,2] == 71)) +
  length(which(suicide_result$Breakpoints[,2] == 72))

suicide_finbkpts<-NULL

for(i in 1:ncol(suicide_result$Breakpoints)){
  
  suicide_finbkpts<-c(suicide_finbkpts, na.omit(suicide_result$Breakpoints[,i]), recursive=T)
  
}

suicide_finbkpts = suicide_finbkpts #NEEDS TO BE CHANGED

hist(suicide_result$NumBkpts, breaks=c(0.5,1.5,2.5), xlim=c(0.5,2.5), ylim=c(0,10000), right=F, xlab="Number of Breaks", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Number", col="#aa0a3c")
hist(suicide_finbkpts, breaks=seq(1977.5,2016.5,1), xlim=c(1979,2015), xaxp=c(1979, 2015, 5), ylim=c(0,10000), right=F, xlab="Year", ylab="Number of Iterations (out of 10,000)", main="Distribution of Breakpoint Locations", col="#aa0a3c")

beta_to_use = suicide_result$Beta[which(suicide_result$Breakpoints[,1] == 11 & suicide_result$NumBkpts == 1)]
sigma_to_use = suicide_result$Sigma[which(suicide_result$Breakpoints[,1] == 11 & suicide_result$NumBkpts == 1)]
fits_to_use = suicide_result$Fits[which(suicide_result$Breakpoints[,1] == 11 & suicide_result$NumBkpts == 1),]

lower = apply(fits_to_use, 2, quantile, probs = 0.025, na.rm = T)
upper = apply(fits_to_use, 2, quantile, probs = 0.975, na.rm = T)

par(mfrow=c(1,2))
plot(suicide$Year, suicide$X15.24.years, ylim=c(-1,2.5), col="#aa0a3c", xaxp=c(1979, 2015, 5), pch=19, main="A. Recorded Suicides in American 15-24 year olds between 1979 and 2015: Single AR(3) Fit", xlab="Year", ylab="Count")
lines(suicide$Year, suicide$X15.24.years, col="#aa0a3c")
points(c(1979:2015), single_fitted, col="#8214a0", pch=15)
lines(c(1979:2015), single_fitted, col="#8214a0", lty=1)

plot(suicide$X15.24.years~suicide$Year, ylim=c(-1,2.5), col="#aa0a3c", xaxp=c(1940, 2015, 5), pch=19, main="B. Recorded Suicides in American 15-24 year olds between 1979 and 2015: Fit from BAAR", xlab="Year", ylab="Count")
lines(suicide$Year, suicide$X15.24.years, col="#aa0a3c")
points(c(1938:1948),colMeans(fits_to_use)[1:11], col="#00a0fa", pch=17)
lines(c(1938:1948),colMeans(fits_to_use)[1:11], col="#00a0fa", lty=1)
points(c(1949:2016),colMeans(fits_to_use)[12:79], col="#0ab45a", pch=18)
lines(c(1949:2016),colMeans(fits_to_use)[12:79], col="#0ab45a", lty=1)
lines(c(1938:1948),lower[1:11], col="#00a0fa", lty=3)
lines(c(1949:2016),lower[12:79], col="#0ab45a", lty=3)
lines(c(1938:1948),upper[1:11], col="#00a0fa", lty=3)
lines(c(1949:2016),upper[12:79], col="#0ab45a", lty=3)

