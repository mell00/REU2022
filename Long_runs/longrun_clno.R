data = temp_data[,2]
time = c(1:length(data))
library("strucchange")
baiperron = breakpoints(data ~ time, breaks = 10, h = 0.05)
bkpts = baiperron$breakpoints

start_time <- Sys.time()
results = bar0(bkpts, time, data, 1000000, 0.5, 0.02, 1)
end_time <- Sys.time()

end_time - start_time
saveRDS(results, file="bar0_climate_bpnoconstraints.RData")