baar_data3_jump0jigg1<-readRDS("baarlr_data3_jump0jigg1.RData")
baar_data3_jump2jigg7<-readRDS("baarlr_data3_jump25jigg75.RData")
baar_data3_jump5jigg5<-readRDS("baarlr_data3_jump50jigg50.RData")
baar_data3_jump7jigg2<-readRDS("baarlr_data3_jump75jiggle25.RData")
baar_data3_jump1jigg0<-readRDS("baarlr_data3_jump1jigg0.RData")

data3 = rbind(mean(baar_data3_jump1jigg0$RunTime), mean(baar_data3_jump7jigg2$RunTime),
mean(baar_data3_jump5jigg5$RunTime), mean(baar_data3_jump2jigg7$RunTime),
mean(baar_data3_jump0jigg1$RunTime))
all = cbind(data3)
rownames(all) = c("J1J0", "J7J2", "J5J5", "J2J7", "J0J1")
colnames(all) = c("Data3")
all

cutoff = 100000

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

bkpts_data3_jump1jigg0 = na.omit(all_breakpoints(baar_data3_jump1jigg0$Breakpoints))
bkpts_data3_jump7jigg2 = na.omit(all_breakpoints(baar_data3_jump7jigg2$Breakpoints))
bkpts_data3_jump5jigg5 = na.omit(all_breakpoints(baar_data3_jump5jigg5$Breakpoints))
bkpts_data3_jump2jigg7 = na.omit(all_breakpoints(baar_data3_jump2jigg7$Breakpoints))
bkpts_data3_jump0jigg1 = na.omit(all_breakpoints(baar_data3_jump0jigg1$Breakpoints))

lower = 30
upper = 60
center_data3_jump1jigg0 = bkpts_data3_jump1jigg0[bkpts_data3_jump1jigg0 > lower & bkpts_data3_jump1jigg0 < upper]
center_data3_jump7jigg2 = bkpts_data3_jump7jigg2[bkpts_data3_jump7jigg2 > lower & bkpts_data3_jump7jigg2 < upper]
center_data3_jump5jigg5 = bkpts_data3_jump5jigg5[bkpts_data3_jump5jigg5 > lower & bkpts_data3_jump5jigg5 < upper]
center_data3_jump2jigg7 = bkpts_data3_jump2jigg7[bkpts_data3_jump2jigg7 > lower & bkpts_data3_jump2jigg7 < upper]
center_data3_jump0jigg1 = bkpts_data3_jump0jigg1[bkpts_data3_jump0jigg1 > lower & bkpts_data3_jump0jigg1 < upper]

data3 = rbind(sd(center_data3_jump1jigg0), sd(center_data3_jump7jigg2), sd(center_data3_jump5jigg5),
sd(center_data3_jump2jigg7), sd(center_data3_jump0jigg1))
all = cbind(data3)
rownames(all) = c("J1J0", "J7J2", "J5J5", "J2J7", "J0J1")
colnames(all) = c("Data3")
all
