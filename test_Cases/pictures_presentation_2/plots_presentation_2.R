#plots for presenation 2
set.seed(2)
first = rnorm(30, mean = 5, sd = 1) 
second = rnorm(30, mean = 15, sd = 1) 
third = rnorm(30, mean = 30, sd = 1) 
time = c(1:90)
data_2 = c(first, second, third) 
test_data_2 = data.frame(time, data_2) 
bkpts_2 = breakpoints(test_data_2$data_2 ~ test_data_2$time, breaks = 5, h = 0.1) 


plot(test_data_2, main="Simulated Data", ylab="Data", xlab="Time")
points(30, test_data_2$data_2[30], col="red", pch= 16, cex = 2)
points(60, test_data_2$data_2[60], col="red", pch= 16, cex = 2)

present_2 = bar0(bkpts_2$breakpoints, test_data_2[,1], test_data_2[,2], 20, 0.4, 0.4, "yes")

d = read.csv("sheet1.csv")
#f is a cleaned up version of d 
hist(f, breaks  = 90, ylab="Number of Times Chosen in 50 Interations", main = "Distribution of Accepted Breakpoint Locations", xlab="X-Value of Breakpoint", col="red", xlim = c(0,90))
