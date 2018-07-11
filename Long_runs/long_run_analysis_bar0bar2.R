#read in all data (set correct working directory prior to this)
bar0_data3_bpcon = readRDS("bar0_data3_bpconstraints.RData")
bar0_data4_bpcon = readRDS("bar0_data4_bpconstraints.RData")
bar0_data7_bpcon = readRDS("bar0_data7_bpconstraints.RData")
bar0_data8_bpcon = readRDS("bar0_data8_bpconstraints.RData")
bar2_data3_bpcon = readRDS("bar2_data3_bpconstraints.RData")
bar2_data4_bpcon = readRDS("bar2_data4_bpconstraints.RData")
bar2_data7_bpcon = readRDS("bar2_data7_bpconstraints.RData")
bar2_data8_bpcon = readRDS("bar2_data8_bpconstraints.RData")

#looking at acceptance rate

##creating a single data frame with all summary statistics for acceptance rate
acceptrate_bar0 = rbind(bar0_data2_bpnocon$AcceptRate, bar0_data2_bpcon$AcceptRate,
bar0_data2_middle$AcceptRate, bar0_data10_bpnocon$AcceptRate, bar0_data10_bpcon$AcceptRate,
bar0_data10_middle$AcceptRate, bar0_climate_bpnocon$AcceptRate, bar0_climate_bpcon$AcceptRate,
bar0_climate_middle$AcceptRate)
acceptrate_bar1 = rbind(bar1_data2_bpnocon$AcceptRate, bar1_data2_bpcon$AcceptRate,
bar1_data2_middle$AcceptRate, bar1_data10_bpnocon$AcceptRate, bar1_data10_bpcon$AcceptRate,
bar1_data10_middle$AcceptRate, bar1_climate_bpnocon$AcceptRate, bar1_climate_bpcon$AcceptRate,
bar1_climate_middle$AcceptRate)
acceptrate_bar2 = rbind(bar2_data2_bpnocon$AcceptRate, bar2_data2_bpcon$AcceptRate,
bar2_data2_middle$AcceptRate, bar2_data10_bpnocon$AcceptRate, bar2_data10_bpcon$AcceptRate,
bar2_data10_middle$AcceptRate, bar2_climate_bpnocon$AcceptRate, bar2_climate_bpcon$AcceptRate,
bar2_climate_middle$AcceptRate)
acceptrate = cbind(acceptrate_bar0, acceptrate_bar1, acceptrate_bar2)
rownames(acceptrate) = c("2bpn", "2bpw", "2mid", "10bpn", "10bpw", "10mid",
"climbpn", "climbpw", "climmid")
colnames(acceptrate) = c("bar0", "bar1", "bar2")
round(acceptrate, 5)

#looking at distribution of number of breakpoints (k)

##quantiatively

### getting mean and SD for numberof breakpoints for all files
num_bar0_3bpw = c(mean(bar0_data3_bpcon$NumBkpts), sd(bar0_data3_bpcon$NumBkpts))
num_bar0_4bpw = c(mean(bar0_data4_bpcon$NumBkpts), sd(bar0_data4_bpcon$NumBkpts))
num_bar0_7bpw = c(mean(bar0_data7_bpcon$NumBkpts), sd(bar0_data7_bpcon$NumBkpts))
num_bar0_8bpw = c(mean(bar0_data8_bpcon$NumBkpts), sd(bar0_data8_bpcon$NumBkpts))
num_bar2_3bpw = c(mean(bar2_data3_bpcon$NumBkpts), sd(bar2_data3_bpcon$NumBkpts))
num_bar2_4bpw = c(mean(bar2_data4_bpcon$NumBkpts), sd(bar2_data4_bpcon$NumBkpts))
num_bar2_7bpw = c(mean(bar2_data7_bpcon$NumBkpts), sd(bar2_data7_bpcon$NumBkpts))
num_bar2_8bpw = c(mean(bar2_data8_bpcon$NumBkpts), sd(bar2_data8_bpcon$NumBkpts))

###create single data frame which shows mean and SD for all variations
num_bar0 = rbind(num_bar0_3bpw, num_bar0_4bpw, num_bar0_7bpw, num_bar0_8bpw)
num_bar2 = rbind(num_bar2_3bpw, num_bar2_4bpw, num_bar2_7bpw, num_bar2_8bpw)
num = cbind(num_bar0, num_bar2)
rownames(num) = c("Data 3", "Data 4", "Data 7", "Data 8")
colnames(num) = c("bar0 Mean", "bar0 SD", "bar2 Mean", "bar2 SD")
round(num, 5)

###set graphical parameters
x_label = "Number of Breakpoints"
y_label = "Number of Iterations"
n_breaks = c(0:10)
x_limits = c(0,10)
y_lower = 0
y_upper = 2500
line_type = 1
line_width = 2

###plot distributions of number of breakpoints
par(mfrow=c(3,3))
hist(current_sim[[1]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[2]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[3]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[4]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[5]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[6]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[7]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[8]], main=current_title, xlab=x_label, ylab=y_label, breaks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)
hist(current_sim[[9]], main=current_title, xlab=x_label, ylab=y_label, breaacceptrate = rbind(acceptrate_bar0, acceptrate_bar1, acceptrate_bar2)
rownames(acceptrate) = c("bar0", "bar1", "bar2")
colnames(acceptrate) = c("Mean", "SD")
round(acceptrate, 5)ks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
lines(c(current_true+0.5,current_true+0.5), c(y_lower,y_upper), col="red", lty=line_type, lwd=line_width)

#looking at distribution of breakpoint locations (tau)

##quantitatively

###function for placing all breakpoints for one file into a single list
all_breakpoints = function(breakpoints){

final_breakpoints = NULL

if(length(breakpoints) == 0){
	return("")
}else{
	for(m in 1:length(breakpoints)){
		if(is.atomic(breakpoints[[m]]) == TRUE) {
			final_breakpoints = c(final_breakpoints, breakpoints[[m]], recursive=T)
		}else if(dim(breakpoints[[m]])[2] >= 2) {
			column_list = NULL
			for(i in 1:dim(breakpoints[[m]])[2]){
				column_list = c(column_list, breakpoints[[m]][,i], recursive=T)
			}
			final_breakpoints = c(final_breakpoints, column_list, recursive=T)	
		}
	}
return(final_breakpoints)
}

}

###getting breakpoints that occur more times than threshold for each file
bkpts_bar0 = as.data.frame(sort(table(na.omit(all_breakpoints(bar0_climate$Breakpoints))), decreasing=T))
bkpts_bar1 = as.data.frame(sort(table(na.omit(all_breakpoints(bar1_climate$Breakpoints))), decreasing=T))
bkpts_bar2 = as.data.frame(sort(table(na.omit(all_breakpoints(bar2_climate$Breakpoints))), decreasing=T))

###function to check if no breakpoints were ever found
breakpoint_check = function(breakpoints){

	threshold <<- 0.15 * 3000 * 9

	if(length(breakpoints) > 1){
		final_breakpoints = paste(breakpoints[breakpoints[,2] >= threshold, 1], sep="", collapse=", ")
		return(final_breakpoints)
	}else{
		return("")
	}

}

###putting breakpoints into rows based on data
bkpts_bar0 = breakpoint_check(bkpts_bar0)
bkpts_bar1 = breakpoint_check(bkpts_bar1)
bkpts_bar2 = breakpoint_check(bkpts_bar2)

##creating a single data frame with breakpoints above threshold
bkpts = rbind(bkpts_bar0, bkpts_bar1, bkpts_bar2)
rownames(bkpts) = c("bar0", "bar1", "bar2")
colnames(bkpts) = c("Breakpoints")
bkpts


##function to look at modes
modefunc <- function(x){
    tabresult <- tabulate(x)
    themode <- which(tabresult == max(tabresult))
    if(sum(tabresult == max(tabresult))>1) themode <- NA
    return(themode)
}

apply(na.omit(bar2_climate_bpnocon$Breakpoints), 2, modefunc)

##graphically

###function to plot frequency of breakpoints
plot_tau_hist <- function(breakpoints, title, x_axis_lab, y_axis_lab, color, num_breaks, x_axis_limits, y_axis_limits){
 
	if(is.atomic(breakpoints) == TRUE) {
		hist(breakpoints, main=title, xlab=x_axis_lab, ylab=y_axis_lab, col=color, breaks=num_breaks, xlim=x_axis_limits, ylim=y_axis_limits) 
	}else if(dim(breakpoints)[2] >= 2) {
		column_list = NULL
		for(i in 1:dim(breakpoints)[2]){
			column_list = c(column_list, breakpoints[,i], recursive=TRUE)
		}
		hist(column_list, main=title, xlab=x_axis_lab, ylab=y_axis_lab, col=color, breaks=num_breaks, xlim=x_axis_limits, ylim=y_axis_limits) 
	}

}
par(mfrow=c(2,4))
###specify which simulation you want to look at right now
current_sim = bar2_data8_bpcon$Breakpoints
current_title = "BAR 2 - Data 8"
###specify correct locations of breakpoints for this training set
current_true = c()

###set graphical parameters
x_label = "Location of Breakpoints"
y_label = "Number of Iterations"
n_breaks = c(seq(1,90,1))
x_limits = c(1,90)
y_lower = 0
y_upper = 100000
bar_color = "yellow2"
line_color = "red1"
line_type = 2
line_width = 1

###plot distributions of breakpoint locations
plot_tau_hist(current_sim, current_title, x_label, y_label, bar_color,
n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}