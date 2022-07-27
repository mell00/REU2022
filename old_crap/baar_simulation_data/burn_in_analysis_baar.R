data100_bpnocon<-readRDS("baar_data100_bpnocon.RData")
data100_bpcon1<-readRDS("baar_data100_bpcon1.RData")
data100_bpcon2<-readRDS("baar_data100_bpcon2.RData")
data100_middle<-readRDS("baar_data100_middle.RData")

#MSE Half-Life Analysis
bpnocon = c(min(which(data100_bpnocon$MSE[,1] < (data100_bpnocon$MSE[1,1]/2))),
min(which(data100_bpnocon$MSE[,2] < (data100_bpnocon$MSE[1,2]/2))),
min(which(data100_bpnocon$MSE[,3] < (data100_bpnocon$MSE[1,3]/2))),
min(which(data100_bpnocon$MSE[,4] < (data100_bpnocon$MSE[1,4]/2))),
min(which(data100_bpnocon$MSE[,5] < (data100_bpnocon$MSE[1,5]/2))),
min(which(data100_bpnocon$MSE[,6] < (data100_bpnocon$MSE[1,6]/2))),
min(which(data100_bpnocon$MSE[,7] < (data100_bpnocon$MSE[1,7]/2))),
min(which(data100_bpnocon$MSE[,8] < (data100_bpnocon$MSE[1,8]/2))),
min(which(data100_bpnocon$MSE[,9] < (data100_bpnocon$MSE[1,9]/2))),
min(which(data100_bpnocon$MSE[,10] < (data100_bpnocon$MSE[1,10]/2))), recursive=T)
bpcon1 = c(min(which(data100_bpcon1$MSE[,1] < (data100_bpcon1$MSE[1,1]/2))),
min(which(data100_bpcon1$MSE[,2] < (data100_bpcon1$MSE[1,2]/2))),
min(which(data100_bpcon1$MSE[,3] < (data100_bpcon1$MSE[1,3]/2))),
min(which(data100_bpcon1$MSE[,4] < (data100_bpcon1$MSE[1,4]/2))),
min(which(data100_bpcon1$MSE[,5] < (data100_bpcon1$MSE[1,5]/2))),
min(which(data100_bpcon1$MSE[,6] < (data100_bpcon1$MSE[1,6]/2))),
min(which(data100_bpcon1$MSE[,7] < (data100_bpcon1$MSE[1,7]/2))),
min(which(data100_bpcon1$MSE[,8] < (data100_bpcon1$MSE[1,8]/2))),
min(which(data100_bpcon1$MSE[,9] < (data100_bpcon1$MSE[1,9]/2))),
min(which(data100_bpcon1$MSE[,10] < (data100_bpcon1$MSE[1,10]/2))), recursive=T)
bpcon2 = c(min(which(data100_bpcon2$MSE[,1] < (data100_bpcon2$MSE[1,1]/2))),
min(which(data100_bpcon2$MSE[,2] < (data100_bpcon2$MSE[1,2]/2))),
min(which(data100_bpcon2$MSE[,3] < (data100_bpcon2$MSE[1,3]/2))),
min(which(data100_bpcon2$MSE[,4] < (data100_bpcon2$MSE[1,4]/2))),
min(which(data100_bpcon2$MSE[,5] < (data100_bpcon2$MSE[1,5]/2))),
min(which(data100_bpcon2$MSE[,6] < (data100_bpcon2$MSE[1,6]/2))),
min(which(data100_bpcon2$MSE[,7] < (data100_bpcon2$MSE[1,7]/2))),
min(which(data100_bpcon2$MSE[,8] < (data100_bpcon2$MSE[1,8]/2))),
min(which(data100_bpcon2$MSE[,9] < (data100_bpcon2$MSE[1,9]/2))),
min(which(data100_bpcon2$MSE[,10] < (data100_bpcon2$MSE[1,10]/2))), recursive=T)
middle = c(min(which(data100_middle$MSE[,1] < (data100_middle$MSE[1,1]/2))),
min(which(data100_middle$MSE[,2] < (data100_middle$MSE[1,2]/2))),
min(which(data100_middle$MSE[,3] < (data100_middle$MSE[1,3]/2))),
min(which(data100_middle$MSE[,4] < (data100_middle$MSE[1,4]/2))),
min(which(data100_middle$MSE[,5] < (data100_middle$MSE[1,5]/2))),
min(which(data100_middle$MSE[,6] < (data100_middle$MSE[1,6]/2))),
min(which(data100_middle$MSE[,7] < (data100_middle$MSE[1,7]/2))),
min(which(data100_middle$MSE[,8] < (data100_middle$MSE[1,8]/2))),
min(which(data100_middle$MSE[,9] < (data100_middle$MSE[1,9]/2))),
min(which(data100_middle$MSE[,10] < (data100_middle$MSE[1,10]/2))), recursive=T)

bpnocon[!is.finite(bpnocon)] <- 0
bpcon1[!is.finite(bpcon1)] <- 0
bpcon2[!is.finite(bpcon2)] <- 0
middle[!is.finite(middle)] <- 0
bpnocon_values = c(mean(bpnocon), sd(bpnocon), mean(bpnocon)+2*sd(bpnocon),
mean(bpnocon)-2*sd(bpnocon), recursive=T)
bpcon1_values = c(mean(bpcon1), sd(bpcon1), mean(bpcon1)+2*sd(bpcon1),
mean(bpcon1)-2*sd(bpcon1), recursive=T)
bpcon2_values = c(mean(bpcon2), sd(bpcon2), mean(bpcon2)+2*sd(bpcon2),
mean(bpcon2)-2*sd(bpcon2), recursive=T)
middle_values = c(mean(middle), sd(middle), mean(middle)+2*sd(middle),
mean(middle)-2*sd(middle), recursive=T)
all_burns = rbind(bpnocon_values, bpcon2_values, bpcon1_values, middle_values)
rownames(all_burns) = c("No Constraints", "Constrained to 2 Bkpts",
"Constrained to 1 Bkpt", "Middle Placement")
colnames(all_burns) = c("Mean", "SD", "Upper CI", "Lower CI")


#Graphing
current_run = data100_bpcon2
lower = 1
upper = 2000
y_lower = 0
y_upper = 700
interval = 100
col1 = "red"
col2 = "green"

lineplot = function(){

	for(i in 1:(upper/interval)){
		lines(c(interval*i, interval*i), c(y_lower, y_upper), col=col2)
	}
	
}

#Error: Error in plot.xy(xy.coords(x, y), type = type, ...) : plot.new has not been called yet
par(mfrow=c(4,3))
plot(current_run$MSE[lower:upper,1])
lines(c(lower, upper), 
      c(lm(current_run$MSE[1:10000,1]~c(1:10000))[[1]][[1]], 
        lm(current_run$MSE[1:10000,1]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,2])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,2]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,2]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,3])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,3]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,3]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,4])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,4]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,4]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,5])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,5]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,5]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,6])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,6]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,6]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,7])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,7]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,7]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,8])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,8]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,8]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,9])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,9]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,9]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()
plot(current_run$MSE[lower:upper,10])
lines(c(lower, upper), c(lm(current_run$MSE[1:10000,10]~c(1:10000))[[1]][[1]], lm(current_run$MSE[1:10000,10]~c(1:10000))[[1]][[1]]), col=col1)
lineplot()


