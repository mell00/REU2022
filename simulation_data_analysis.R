#read in all data (set correct working directory prior to this)
bar0_data0a = readRDS("bar0_data0a.RData")
bar0_data0b = readRDS("bar0_data0b.RData")
bar0_data1 = readRDS("bar0_data1.RData")
bar0_data2 = readRDS("bar0_data2.RData")
bar0_data3 = readRDS("bar0_data3.RData")
bar0_data4 = readRDS("bar0_data4.RData")
bar0_data5 = readRDS("bar0_data5.RData")
bar0_data6 = readRDS("bar0_data6.RData")
bar0_data7 = readRDS("bar0_data7.RData")
bar0_data8 = readRDS("bar0_data8.RData")
bar0_data9 = readRDS("bar0_data9.RData")
bar0_data10 = readRDS("bar0_data10.RData")
barA_data0a = readRDS("barA_data0a.RData")
barA_data0b = readRDS("barA_data0b.RData")
barA_data1 = readRDS("barA_data1.RData")
barA_data2 = readRDS("barA_data2.RData")
barA_data3 = readRDS("barA_data3.RData")
barA_data4 = readRDS("barA_data4.RData")
barA_data5 = readRDS("barA_data5.RData")
barA_data6 = readRDS("barA_data6.RData")
barA_data7 = readRDS("barA_data7.RData")
barA_data8 = readRDS("barA_data8.RData")
barA_data9 = readRDS("barA_data9.RData")
barA_data10 = readRDS("barA_data10.RData")
barB_data0a = readRDS("barB_data0a.RData")
barB_data0b = readRDS("barB_data0b.RData")
barB_data1 = readRDS("barB_data1.RData")
barB_data2 = readRDS("barB_data2.RData")
barB_data3 = readRDS("barB_data3.RData")
barB_data4 = readRDS("barB_data4.RData")
barB_data5 = readRDS("barB_data5.RData")
barB_data6 = readRDS("barB_data6.RData")
barB_data7 = readRDS("barB_data7.RData")
barB_data8 = readRDS("barB_data8.RData")
barB_data9 = readRDS("barB_data9.RData")
barB_data10 = readRDS("barB_data10.RData")

#names for different training data sets
datanames = c("0 Breaks, Low SD", "0 Breaks, High SD", "1 Break, Clean SD",
"2 Breaks, Clean SD", "1 Break, Messy SD", "2 Breaks, Messy SD", "1 Break, Large Slopes",
"2 Breaks, Large Slopes", "1 Break, Small Slopes", "2 Breaks, Small Slopes",
"1 Break, Diff. Var.", "2 Break, Diff. Var.")

#mean and SD for acceptance rate from all simulations
acceptrate_00a = c(mean(bar0_data0a$AcceptRate), sd(bar0_data0a$AcceptRate))
acceptrate_00b = c(mean(bar0_data0b$AcceptRate), sd(bar0_data0b$AcceptRate))
acceptrate_01 = c(mean(bar0_data1$AcceptRate), sd(bar0_data1$AcceptRate))
acceptrate_02 = c(mean(bar0_data2$AcceptRate), sd(bar0_data2$AcceptRate))
acceptrate_03 = c(mean(bar0_data3$AcceptRate), sd(bar0_data3$AcceptRate))
acceptrate_04 = c(mean(bar0_data4$AcceptRate), sd(bar0_data4$AcceptRate))
acceptrate_05 = c(mean(bar0_data5$AcceptRate), sd(bar0_data5$AcceptRate))
acceptrate_06 = c(mean(bar0_data6$AcceptRate), sd(bar0_data6$AcceptRate))
acceptrate_07 = c(mean(bar0_data7$AcceptRate), sd(bar0_data8$AcceptRate))
acceptrate_08 = c(mean(bar0_data8$AcceptRate), sd(bar0_data7$AcceptRate))
acceptrate_09 = c(mean(bar0_data9$AcceptRate), sd(bar0_data9$AcceptRate))
acceptrate_010 = c(mean(bar0_data10$AcceptRate), sd(bar0_data10$AcceptRate))
acceptrate_A0a = c(mean(barA_data0a$AcceptRate), sd(barA_data0a$AcceptRate))
acceptrate_A0b = c(mean(barA_data0b$AcceptRate), sd(barA_data0b$AcceptRate))
acceptrate_A1 = c(mean(barA_data1$AcceptRate), sd(barA_data1$AcceptRate))
acceptrate_A2 = c(mean(barA_data2$AcceptRate), sd(barA_data2$AcceptRate))
acceptrate_A3 = c(mean(barA_data3$AcceptRate), sd(barA_data3$AcceptRate))
acceptrate_A4 = c(mean(barA_data4$AcceptRate), sd(barA_data4$AcceptRate))
acceptrate_A5 = c(mean(barA_data5$AcceptRate), sd(barA_data5$AcceptRate))
acceptrate_A6 = c(mean(barA_data6$AcceptRate), sd(barA_data6$AcceptRate))
acceptrate_A7 = c(mean(barA_data7$AcceptRate), sd(barA_data8$AcceptRate))
acceptrate_A8 = c(mean(barA_data8$AcceptRate), sd(barA_data7$AcceptRate))
acceptrate_A9 = c(mean(barA_data9$AcceptRate), sd(barA_data9$AcceptRate))
acceptrate_A10 = c(mean(barA_data10$AcceptRate), sd(barA_data10$AcceptRate))
acceptrate_B0a = c(mean(barB_data0a$AcceptRate), sd(barB_data0a$AcceptRate))
acceptrate_B0b = c(mean(barB_data0b$AcceptRate), sd(barB_data0b$AcceptRate))
acceptrate_B1 = c(mean(barB_data1$AcceptRate), sd(barB_data1$AcceptRate))
acceptrate_B2 = c(mean(barB_data2$AcceptRate), sd(barB_data2$AcceptRate))
acceptrate_B3 = c(mean(barB_data3$AcceptRate), sd(barB_data3$AcceptRate))
acceptrate_B4 = c(mean(barB_data4$AcceptRate), sd(barB_data4$AcceptRate))
acceptrate_B5 = c(mean(barB_data5$AcceptRate), sd(barB_data5$AcceptRate))
acceptrate_B6 = c(mean(barB_data6$AcceptRate), sd(barB_data6$AcceptRate))
acceptrate_B7 = c(mean(barB_data7$AcceptRate), sd(barB_data8$AcceptRate))
acceptrate_B8 = c(mean(barB_data8$AcceptRate), sd(barB_data7$AcceptRate))
acceptrate_B9 = c(mean(barB_data9$AcceptRate), sd(barB_data9$AcceptRate))
acceptrate_B10 = c(mean(barB_data10$AcceptRate), sd(barB_data10$AcceptRate))

#putting together the acceptance rate means and SD by data set
acceptrate_data0a = c(acceptrate_00a, acceptrate_A0a, acceptrate_B0a, recursive=T)
acceptrate_data0b = c(acceptrate_00b, acceptrate_A0b, acceptrate_B0b, recursive=T)
acceptrate_data1 = c(acceptrate_01, acceptrate_A1, acceptrate_B1, recursive=T)
acceptrate_data2 = c(acceptrate_02, acceptrate_A2, acceptrate_B2, recursive=T)
acceptrate_data3 = c(acceptrate_03, acceptrate_A3, acceptrate_B3, recursive=T)
acceptrate_data4 = c(acceptrate_04, acceptrate_A4, acceptrate_B4, recursive=T)
acceptrate_data5 = c(acceptrate_05, acceptrate_A5, acceptrate_B5, recursive=T)
acceptrate_data6 = c(acceptrate_06, acceptrate_A6, acceptrate_B6, recursive=T)
acceptrate_data7 = c(acceptrate_07, acceptrate_A7, acceptrate_B7, recursive=T)
acceptrate_data8 = c(acceptrate_08, acceptrate_A8, acceptrate_B8, recursive=T)
acceptrate_data9 = c(acceptrate_09, acceptrate_A9, acceptrate_B9, recursive=T)
acceptrate_data10 = c(acceptrate_010, acceptrate_A10, acceptrate_B10, recursive=T)

#creating a single data frame with all summary statistics for acceptance rate
acceptrate = rbind(acceptrate_data0a, acceptrate_data0b, acceptrate_data1, acceptrate_data2,
acceptrate_data3, acceptrate_data4, acceptrate_data5, acceptrate_data6, acceptrate_data7,
acceptrate_data8, acceptrate_data9, acceptrate_data10)
acceptrate = rbind(acceptrate, colMeans(acceptrate))
rownames(acceptrate) = c(datanames, "Total", recursive=T)
colnames(acceptrate) = c("Rand. Mean", "Rand. SD", "Jig. Mean", "Jig. SD", "M+J Mean", "M+J SD")
round(acceptrate, 5)

#function for finding number of accepted breakpoints in each row
k_dist = function(breakpoints){

	all_k = list()

	for(i in 1:nrow(breakpoints)){

		current_k = length(breakpoints[i,][!is.na(breakpoints[i,])])
		all_k = c(all_k, current_k, recursive=T)

	}

	return(all_k)

}

#specify which simulation you want to look at right now
current_sim = barB_data2$Breakpoints
#specify correct number of breakpoints for this training set
current_true = 2

#get distributions for number of breakpoints
k_dist_1 = k_dist(current_sim[[1]])
k_dist_2 = k_dist(current_sim[[2]])
k_dist_3 = k_dist(current_sim[[3]])
k_dist_4 = k_dist(current_sim[[4]])
k_dist_5 = k_dist(current_sim[[5]])
k_dist_6 = k_dist(current_sim[[6]])
k_dist_7 = k_dist(current_sim[[7]])
k_dist_8 = k_dist(current_sim[[8]])
k_dist_9 = k_dist(current_sim[[9]])
k_dist_10 = k_dist(current_sim[[10]])

#plot distributions of number of breakpoints
par(mfrow=c(4,3))
hist(k_dist_1, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_2, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_3, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_4, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_5, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_6, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_7, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_8, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_9, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)
hist(k_dist_10, breaks=10, xlim=c(0,10), ylim=c(0,2000))
lines(c(current_true+0.5,current_true+0.5), c(current_true+0.5,2000), col="red", lty=1, lwd=2)