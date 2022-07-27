#setwd("/Users/sarah/REU2018/baar_simulation_data")
baar_data1_jump0jigg1<-readRDS("baar_data1_jump0jigg1.RData")
baar_data1_jump2jigg7<-readRDS("baar_data1_jump25jigg75.RData")
baar_data1_jump5jigg5<-readRDS("baar_data1_jump50jigg50.RData")
baar_data1_jump7jigg2<-readRDS("baar_data1_jump75jiggle25.RData")
baar_data1_jump1jigg0<-readRDS("baar_data1_jump1jigg0.RData")
baar_data3_jump0jigg1<-readRDS("baar_data3_jump0jigg1.RData")
baar_data3_jump2jigg7<-readRDS("baar_data3_jump25jigg75.RData")
baar_data3_jump5jigg5<-readRDS("baar_data3_jump50jigg50.RData")
baar_data3_jump7jigg2<-readRDS("baar_data3_jump75jiggle25.RData")
baar_data3_jump1jigg0<-readRDS("baar_data3_jump1jigg0.RData")
baar_data9_jump0jigg1<-readRDS("baar_data9_jump0jigg1.RData")
baar_data9_jump2jigg7<-readRDS("baar_data9_jump25jigg75.RData")
baar_data9_jump5jigg5<-readRDS("baar_data9_jump50jigg50.RData")
baar_data9_jump7jigg2<-readRDS("baar_data9_jump75jiggle25.RData")
baar_data9_jump1jigg0<-readRDS("baar_data9_jump1jigg0.RData")
baar_data11_jump0jigg1<-readRDS("baar_data11_jump0jigg1.RData")
baar_data11_jump2jigg7<-readRDS("baar_data11_jump25jigg75.RData")
baar_data11_jump5jigg5<-readRDS("baar_data11_jump50jigg50.RData")
baar_data11_jump7jigg2<-readRDS("baar_data11_jump75jiggle25.RData")
baar_data11_jump1jigg0<-readRDS("baar_data11_jump1jigg0.RData")

data1 = rbind(mean(baar_data1_jump0jigg1$RunTime), mean(baar_data1_jump2jigg7$RunTime),
mean(baar_data1_jump5jigg5$RunTime), mean(baar_data1_jump7jigg2$RunTime),
mean(baar_data1_jump1jigg0$RunTime))
data3 = rbind(mean(baar_data3_jump0jigg1$RunTime), mean(baar_data3_jump2jigg7$RunTime),
mean(baar_data3_jump5jigg5$RunTime), mean(baar_data3_jump7jigg2$RunTime),
mean(baar_data3_jump1jigg0$RunTime))
data9 = rbind(mean(baar_data9_jump0jigg1$RunTime), mean(baar_data9_jump2jigg7$RunTime),
mean(baar_data9_jump5jigg5$RunTime), mean(baar_data9_jump7jigg2$RunTime),
mean(baar_data9_jump1jigg0$RunTime))
data11 = rbind(mean(baar_data11_jump0jigg1$RunTime), mean(baar_data11_jump2jigg7$RunTime),
mean(baar_data11_jump5jigg5$RunTime), mean(baar_data11_jump7jigg2$RunTime),
mean(baar_data11_jump1jigg0$RunTime))
all = cbind(data1, data3, data9, data11)
rownames(all) = c("J1J0", "J7J2", "J5J5", "J2J7", "J0J1")
colnames(all) = c("Data1", "Data3", "Data9", "Data11")
all
rowMeans(all)

cutoff = 2500

all_breakpoints = function(breakpoints){

final_breakpoints = NULL

if(length(breakpoints) == 0){
	return("")
}else{
	for(m in 1:length(breakpoints)){
		if(is.atomic(breakpoints[[m]]) == TRUE) {
			final_breakpoints = c(final_breakpoints, breakpoints[[m]][c(1:cutoff)], recursive=T)
		}else if(dim(breakpoints[[m]])[2] >= 2) {
			column_list = NULL
			for(i in 1:dim(breakpoints[[m]])[2]){
				column_list = c(column_list, breakpoints[[m]][c(1:cutoff),i], recursive=T)
			}
			final_breakpoints = c(final_breakpoints, column_list, recursive=T)	
		}
	}
return(final_breakpoints)
}
}

bkpts_data1_jump1jigg0 = na.omit(all_breakpoints(baar_data1_jump1jigg0$Breakpoints))
bkpts_data1_jump7jigg2 = na.omit(all_breakpoints(baar_data1_jump7jigg2$Breakpoints))
bkpts_data1_jump5jigg5 = na.omit(all_breakpoints(baar_data1_jump5jigg5$Breakpoints))
bkpts_data1_jump2jigg7 = na.omit(all_breakpoints(baar_data1_jump2jigg7$Breakpoints))
bkpts_data1_jump0jigg1 = na.omit(all_breakpoints(baar_data1_jump0jigg1$Breakpoints))
bkpts_data3_jump1jigg0 = na.omit(all_breakpoints(baar_data3_jump1jigg0$Breakpoints))
bkpts_data3_jump7jigg2 = na.omit(all_breakpoints(baar_data3_jump7jigg2$Breakpoints))
bkpts_data3_jump5jigg5 = na.omit(all_breakpoints(baar_data3_jump5jigg5$Breakpoints))
bkpts_data3_jump2jigg7 = na.omit(all_breakpoints(baar_data3_jump2jigg7$Breakpoints))
bkpts_data3_jump0jigg1 = na.omit(all_breakpoints(baar_data3_jump0jigg1$Breakpoints))
bkpts_data9_jump1jigg0 = na.omit(all_breakpoints(baar_data9_jump1jigg0$Breakpoints))
bkpts_data9_jump7jigg2 = na.omit(all_breakpoints(baar_data9_jump7jigg2$Breakpoints))
bkpts_data9_jump5jigg5 = na.omit(all_breakpoints(baar_data9_jump5jigg5$Breakpoints))
bkpts_data9_jump2jigg7 = na.omit(all_breakpoints(baar_data9_jump2jigg7$Breakpoints))
bkpts_data9_jump0jigg1 = na.omit(all_breakpoints(baar_data9_jump0jigg1$Breakpoints))
bkpts_data11_jump1jigg0 = na.omit(all_breakpoints(baar_data11_jump1jigg0$Breakpoints))
bkpts_data11_jump7jigg2 = na.omit(all_breakpoints(baar_data11_jump7jigg2$Breakpoints))
bkpts_data11_jump5jigg5 = na.omit(all_breakpoints(baar_data11_jump5jigg5$Breakpoints))
bkpts_data11_jump2jigg7 = na.omit(all_breakpoints(baar_data11_jump2jigg7$Breakpoints))
bkpts_data11_jump0jigg1 = na.omit(all_breakpoints(baar_data11_jump0jigg1$Breakpoints))

lower = 30
upper = 60
center_data1_jump1jigg0 = bkpts_data1_jump1jigg0[bkpts_data1_jump1jigg0 > lower & bkpts_data1_jump1jigg0 < upper]
center_data1_jump7jigg2 = bkpts_data1_jump7jigg2[bkpts_data1_jump7jigg2 > lower & bkpts_data1_jump7jigg2 < upper]
center_data1_jump5jigg5 = bkpts_data1_jump5jigg5[bkpts_data1_jump5jigg5 > lower & bkpts_data1_jump5jigg5 < upper]
center_data1_jump2jigg7 = bkpts_data1_jump2jigg7[bkpts_data1_jump2jigg7 > lower & bkpts_data1_jump2jigg7 < upper]
center_data1_jump0jigg1 = bkpts_data1_jump0jigg1[bkpts_data1_jump0jigg1 > lower & bkpts_data1_jump0jigg1 < upper]
center_data3_jump1jigg0 = bkpts_data3_jump1jigg0[bkpts_data3_jump1jigg0 > lower & bkpts_data3_jump1jigg0 < upper]
center_data3_jump7jigg2 = bkpts_data3_jump7jigg2[bkpts_data3_jump7jigg2 > lower & bkpts_data3_jump7jigg2 < upper]
center_data3_jump5jigg5 = bkpts_data3_jump5jigg5[bkpts_data3_jump5jigg5 > lower & bkpts_data3_jump5jigg5 < upper]
center_data3_jump2jigg7 = bkpts_data3_jump2jigg7[bkpts_data3_jump2jigg7 > lower & bkpts_data3_jump2jigg7 < upper]
center_data3_jump0jigg1 = bkpts_data3_jump0jigg1[bkpts_data3_jump0jigg1 > lower & bkpts_data3_jump0jigg1 < upper]
center_data9_jump1jigg0 = bkpts_data9_jump1jigg0[bkpts_data9_jump1jigg0 > lower & bkpts_data9_jump1jigg0 < upper]
center_data9_jump7jigg2 = bkpts_data9_jump7jigg2[bkpts_data9_jump7jigg2 > lower & bkpts_data9_jump7jigg2 < upper]
center_data9_jump5jigg5 = bkpts_data9_jump5jigg5[bkpts_data9_jump5jigg5 > lower & bkpts_data9_jump5jigg5 < upper]
center_data9_jump2jigg7 = bkpts_data9_jump2jigg7[bkpts_data9_jump2jigg7 > lower & bkpts_data9_jump2jigg7 < upper]
center_data9_jump0jigg1 = bkpts_data9_jump0jigg1[bkpts_data9_jump0jigg1 > lower & bkpts_data9_jump0jigg1 < upper]
center_data11_jump1jigg0 = bkpts_data11_jump1jigg0[bkpts_data11_jump1jigg0 > lower & bkpts_data11_jump1jigg0 < upper]
center_data11_jump7jigg2 = bkpts_data11_jump7jigg2[bkpts_data11_jump7jigg2 > lower & bkpts_data11_jump7jigg2 < upper]
center_data11_jump5jigg5 = bkpts_data11_jump5jigg5[bkpts_data11_jump5jigg5 > lower & bkpts_data11_jump5jigg5 < upper]
center_data11_jump2jigg7 = bkpts_data11_jump2jigg7[bkpts_data11_jump2jigg7 > lower & bkpts_data11_jump2jigg7 < upper]
center_data11_jump0jigg1 = bkpts_data11_jump0jigg1[bkpts_data11_jump0jigg1 > lower & bkpts_data11_jump0jigg1 < upper]

data1 = rbind(sd(center_data1_jump1jigg0), sd(center_data1_jump7jigg2), sd(center_data1_jump5jigg5),
sd(center_data1_jump2jigg7), sd(center_data1_jump0jigg1))
data3 = rbind(sd(center_data3_jump1jigg0), sd(center_data3_jump7jigg2), sd(center_data3_jump5jigg5),
sd(center_data3_jump2jigg7), sd(center_data3_jump0jigg1))
data9 = rbind(sd(center_data9_jump1jigg0), sd(center_data9_jump7jigg2), sd(center_data9_jump5jigg5),
sd(center_data9_jump2jigg7), sd(center_data9_jump0jigg1))
data11 = rbind(sd(center_data11_jump1jigg0), sd(center_data11_jump7jigg2), sd(center_data11_jump5jigg5),
sd(center_data11_jump2jigg7), sd(center_data11_jump0jigg1))
all = cbind(data1, data3, data9, data11)
rownames(all) = c("J1J0", "J7J2", "J5J5", "J2J7", "J0J1")
colnames(all) = c("Data1", "Data3", "Data9", "Data11")
all
rowMeans(all)

lower = 30
upper = 60

finalSDs = function(breakpoints){

final_SDs = NULL

if(length(breakpoints) == 0){
	return("")
}else{
	for(m in 1:length(breakpoints)){
		if(is.atomic(breakpoints[[m]]) == TRUE) {
			current_breakpoints = na.omit(c(breakpoints[[m]][c(1:cutoff)], recursive=T))
			current_breakpoints = current_breakpoints[current_breakpoints > lower & current_breakpoints < upper]
			final_SDs = c(final_SDs, sd(current_breakpoints), recursive=T)
		}else if(dim(breakpoints[[m]])[2] >= 2) {
			column_list = NULL
			for(i in 1:dim(breakpoints[[m]])[2]){
				column_list = na.omit(c(column_list, breakpoints[[m]][c(1:cutoff),i], recursive=T))
			}
			column_list = column_list[column_list > lower & column_list < upper]
			final_SDs = c(final_SDs, sd(column_list), recursive=T)	
		}
	}
return(final_SDs)

}

}

SD_data1_jump1jigg0 = finalSDs(baar_data1_jump1jigg0$Breakpoints)
SD_data1_jump7jigg2 = finalSDs(baar_data1_jump7jigg2$Breakpoints)
SD_data1_jump5jigg5 = finalSDs(baar_data1_jump5jigg5$Breakpoints)
SD_data1_jump2jigg7 = finalSDs(baar_data1_jump2jigg7$Breakpoints)
SD_data1_jump0jigg1 = finalSDs(baar_data1_jump0jigg1$Breakpoints)
SD_data3_jump1jigg0 = finalSDs(baar_data3_jump1jigg0$Breakpoints)
SD_data3_jump7jigg2 = finalSDs(baar_data3_jump7jigg2$Breakpoints)
SD_data3_jump5jigg5 = finalSDs(baar_data3_jump5jigg5$Breakpoints)
SD_data3_jump2jigg7 = finalSDs(baar_data3_jump2jigg7$Breakpoints)
SD_data3_jump0jigg1 = finalSDs(baar_data3_jump0jigg1$Breakpoints)
SD_data9_jump1jigg0 = finalSDs(baar_data9_jump1jigg0$Breakpoints)
SD_data9_jump7jigg2 = finalSDs(baar_data9_jump7jigg2$Breakpoints)
SD_data9_jump5jigg5 = finalSDs(baar_data9_jump5jigg5$Breakpoints)
SD_data9_jump2jigg7 = finalSDs(baar_data9_jump2jigg7$Breakpoints)
SD_data9_jump0jigg1 = finalSDs(baar_data9_jump0jigg1$Breakpoints)
SD_data11_jump1jigg0 = finalSDs(baar_data11_jump1jigg0$Breakpoints)
SD_data11_jump7jigg2 = finalSDs(baar_data11_jump7jigg2$Breakpoints)
SD_data11_jump5jigg5 = finalSDs(baar_data11_jump5jigg5$Breakpoints)
SD_data11_jump2jigg7 = finalSDs(baar_data11_jump2jigg7$Breakpoints)
SD_data11_jump0jigg1 = finalSDs(baar_data11_jump0jigg1$Breakpoints)

data1 = cbind(mean(SD_data1_jump1jigg0), sd(SD_data1_jump1jigg0),
mean(SD_data1_jump7jigg2), sd(SD_data1_jump7jigg2),
mean(SD_data1_jump5jigg5), sd(SD_data1_jump5jigg5),
mean(SD_data1_jump2jigg7), sd(SD_data1_jump2jigg7),
mean(SD_data1_jump0jigg1), sd(SD_data1_jump0jigg1))
data3 = cbind(mean(SD_data3_jump1jigg0), sd(SD_data3_jump1jigg0),
mean(SD_data3_jump7jigg2), sd(SD_data3_jump7jigg2),
mean(SD_data3_jump5jigg5), sd(SD_data3_jump5jigg5),
mean(SD_data3_jump2jigg7), sd(SD_data3_jump2jigg7),
mean(SD_data3_jump0jigg1), sd(SD_data3_jump0jigg1))