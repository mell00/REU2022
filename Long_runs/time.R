#bar0_data2_bpno | bar1_data2_bpno | bar2_data2_bpno
#2.775513 hours | 2.240894 hours | 2.934325 hours
#bar0_data2_bpwith | bar1_data2_bpwith | bar2_data2_bpwith
#3.464557 hours | 2.915682 hours | 2.305141 hours
#bar0_data2_middle | bar1_data2_middle | bar2_data2_middle
#3.561302 hours | 2.583613 hours | 2.695248 hours

#bar0_data10_bpno | bar1_data10_bpno | bar2_data10_bpno
#3.560775 hours | 2.34281 hours | 2.823501 hours
#bar0_data10_bpwith | bar1_data10_bpno | bar2_data10_bpwith
#3.676524 hours | 2.394353 hours | 2.292334 hours
#bar0_data10_middle | bar1_data10_middle | bar2_data10_middle
#2.899285 hours | 2.82572 hours | 3.13553 hours

#bar0_climate_bpno | bar1_climate_bpno | bar2_climate_bpno
#5.537377 hours | 3.766292 hours | 4.452221 hours
#bar0_climate_bpwith | bar1_climate_bpwith | bar2_climate_bpwith
#5.412965 hours | 3.820345 hours | 4.503763 hours
#bar0_climate_middle | bar1_climate_middle | bar2_climate_middle
#5.534174 hours | 3.820418 hours | 4.266711 hours

time_data2_bpno = cbind(2.775513, 2.240894, 2.934325)
time_data2_bpwith = cbind(3.464557, 2.915682, 2.305141)
time_data_middle = cbind(3.561302, 2.583613, 2.695248)

time_data10_bpno = cbind(3.560775, 2.34281, 2.823501)
time_data10_bpwith = cbind(3.676524, 2.394353, 2.292334)
time_data10_middle = cbind(2.899285, 2.82572, 3.13553)

time_climate_bpno = cbind(5.537377, 3.766292, 4.452221)
time_climate_bpwith = cbind(5.412965, 3.820345, 4.503763)
time_climate_middle = cbind(5.534174, 3.820418, 4.266711)

time = rbind(time_data2_bpno, time_data2_bpwith, time_data_middle,
time_data10_bpno, time_data10_bpwith, time_data10_middle,
time_climate_bpno, time_climate_bpwith, time_climate_middle)

time_means = colMeans(time)
time_sd = apply(time, 2, sd)
time_ci = NULL

for(i in 1:3){
current = rbind(time_means[i]+2*time_sd[i], time_means[i], time_means[i]-2*time_sd[i])
time_ci = cbind(time_ci, current)
}
rownames(time_ci)=c("Upper", "Mean", "Lower")
colnames(time_ci)=c("bar0", "bar1", "bar2")
time_ci