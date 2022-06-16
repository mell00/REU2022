temp_data = read.csv(url("https://www.ncdc.noaa.gov/cag/global/time-series/globe/land_ocean/ytd/12/1880-2021.csv"))
temp_data = temp_data[-c(1,2,3,4),]
temp_data[,1] = as.numeric(as.character(temp_data[,1]))
temp_data[,2] = as.numeric(as.character(temp_data[,2]))

setwd("/Users/khaglich/Desktop/Edited REU Main/baar_simulation_data")
source("analysis_baar.R")
## Not sure where these functions are? 
temp_bkpts = breakpoints(temp_data[,2]~temp_data[,1], h=0.05, breaks=10)

temp_results = barB(temp_bkpts$breakpoints, temp_data[,1], temp_data[,2], 2500, 0.5, 0.02)
hist(temp_results$NumBkpts)

title = "barB"

if(is.atomic(temp_results$Breakpoints) == TRUE) {
	hist(temp_results$Breakpoints, main=title, col="red", breaks=138) 
}else if(dim(temp_results$Breakpoints)[2] >= 2) {
	column_list = NULL
	for(i in 1:dim(temp_results$Breakpoints)[2]){
		column_list = c(column_list, temp_results$Breakpoints[,i], recursive=TRUE)
	}
	hist(column_list, main=title, col="red", breaks=138, xlim=c(0,138)) 
}
