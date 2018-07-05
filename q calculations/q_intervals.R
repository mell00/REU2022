#--------q for the interval based addition---------
#for add 
i = which(k_ends_new == sum(k_ends_new) - sum(k_ends))
d = diff(k_ends)
q1 = ( ( ( (d[i-1])^4  / sum(d)^4) ) * ( 1 / ( d[i-1] - 4 ) ) )

#for sub
i = which(k_ends == sum(k_ends) - sum(k_ends_new) )
print(i)
d = diff(k_ends_new)
q2 = murder_k * ( ( ( (d[i-1])^4  / sum(d)^4) ) * ( 1 / ( d[i-1] - 4 ) ) )


#--------q for interval based subtraction-------------
#for add 
#setting up qs for ratio
all_intv = diff(k_ends_new) #finds all of the intervals
intv_1 = all_intv[-1] #takes the first number off
intv_2 = all_intv[-length(all_intv)] #takes the last number off
sum_intv = intv_1 + intv_2 #finds the sums of the adjacent intervals
i = which(k_ends_new == abs(sum(k_ends_new) - sum(k_ends))) #finds the location of the point that was deleted
q2 = (1/sum_intv[i-1] ) / (sum(sum_intv))

#for sub
all_intv = diff(k_ends) #finds all of the intervals
intv_1 = all_intv[-1] #takes the first number off
intv_2 = all_intv[-length(all_intv)] #takes the last number off
sum_intv = intv_1 + intv_2 #finds the sums of the adjacent intervals
i = which(k_ends == abs(sum(k_ends_new) - sum(k_ends))) #finds the location of the point that was deleted
q2 = (1/sum_intv[i-1] ) / (sum(sum_intv))


log(0.000496537) - log(0.000140056)
log(3.925862e-06) - log(8.334722e-08)




