setwd("/Users/khaglich/Desktop/Edited REU Main/Presentations_Pictures_Old/old_crap/old_simulation_data/Addsub_simulation_data")

#read in all data (set correct working directory prior to this)
bar0_climate = readRDS("bar0_climate.RData")
bar1_climate = readRDS("bar1_climate.RData")
bar2_climate = readRDS("bar2_climate.RData")

#looking at acceptance rate

##mean and SD for acceptance rate from all simulations
acceptrate_bar0 = c(mean(bar0_climate$AcceptRate), sd(bar0_climate$AcceptRate))
acceptrate_bar1 = c(mean(bar1_climate$AcceptRate), sd(bar1_climate$AcceptRate))
acceptrate_bar2 = c(mean(bar2_climate$AcceptRate), sd(bar2_climate$AcceptRate))

##creating a single data frame with all summary statistics for acceptance rate
acceptrate = rbind(acceptrate_bar0, acceptrate_bar1, acceptrate_bar2)
rownames(acceptrate) = c("bar0", "bar1", "bar2")
colnames(acceptrate) = c("Mean", "SD")
round(acceptrate, 5)

#looking at distribution of number of breakpoints (k)

##quantiatively

### getting mean and SD for numberof breakpoints for all files
num_bar0 = c(mean(colMeans(bar0_climate$NumBkpts)), mean(sapply(bar0_climate$NumBkpts, sd)))
num_bar1 = c(mean(colMeans(bar1_climate$NumBkpts)), mean(sapply(bar1_climate$NumBkpts, sd)))
num_bar2 = c(mean(colMeans(bar2_climate$NumBkpts)), mean(sapply(bar2_climate$NumBkpts, sd)))

###create single data frame which shows mean and SD for all variations
num = rbind(num_bar0, num_bar1, num_bar2)
rownames(num) = c("bar0", "bar1", "bar2")
colnames(num) = c("Mean", "SD")
num

###set graphical parameters
x_label = "Number of Breakpoints"
y_label = "Number of Iterations"
n_breaks = c(0:10)
x_limits = c(0,10)
y_lower = 0
y_upper = 2500
line_type = 1
line_width = 2

#Is this supposed to go here?
# ###specify which simulation you want to look at right now
# current_sim = bar2_climate$Breakpoints
# current_title = "bar2 - Climate Data"
# ###specify correct locations of breakpoints for this training set
# current_true = c()

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
hist(current_sim[[9]], main=current_title, xlab=x_label, ylab=y_label, 
     breaacceptrate = rbind(acceptrate_bar0, acceptrate_bar1, acceptrate_bar2),
     rownames(acceptrate) = c("bar0", "bar1", "bar2"),
     colnames(acceptrate) = c("Mean", "SD"),
     round(acceptrate, 5), ks=n_breaks, xlim=x_limits, ylim=c(y_lower,y_upper))
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

# ###specify which simulation you want to look at right now
# current_sim = bar2_climate$Breakpoints
# current_title = "bar2 - Climate Data"
# ###specify correct locations of breakpoints for this training set
# current_true = c()

###set graphical parameters
x_label = "Location of Breakpoints"
y_label = "Number of Iterations"
n_breaks = c(1:138)
x_limits = c(1,138)
y_lower = 0
y_upper = 3000
bar_color = "yellow2"
line_color = "red1"
line_type = 2
line_width = 1

###plot distributions of breakpoint locations
par(mfrow=c(3,3))
plot_tau_hist(current_sim[[1]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[2]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[3]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[4]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[5]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[6]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[7]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[8]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
plot_tau_hist(current_sim[[9]], current_title, x_label, y_label, bar_color, n_breaks, x_limits, c(y_lower, y_upper))
if(length(current_true)>=1){lines(c(current_true[[1]],current_true[[1]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}
if(length(current_true)>=2){lines(c(current_true[[2]],current_true[[2]]),c(y_lower,y_upper),col=line_color, lty=line_type, lwd=line_width)}

