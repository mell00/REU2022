#Bai-Perron Test for AR Models

#Key
#x_values = x variable for all observations
#y_values = y variable for all observations
#order = order of AR model to be fitted
#interval = minimum proportion of observations in each subsection (i.e. 0.15)
#max_breaks = maximum number of breaks to be considered
#progress = whether or not to show progress bar, TRUE/FALSE

if(file.exists("data_for_trials.R")){
        source("data_for_trials.R")
}

bai_perron.ar<-function(x_values, y_values, order=1, interval=0.15, max_breaks=3, progress=T){

	n = length(x_values) #Number of observations
	x_values = 1:n #Turn x values into observations
	int = floor(n*interval) #Set minimum interval between breaks

	#Checking to make sure interval is larger than 3 points
	if(int < 3){
		print(paste("Interval is too small. Try with ", round(3/n, 2), ".", sep=""))
		return()
	}

	#Checking to make sure max breaks works with interval
	if(int > (n-1)/(max_breaks+1)){
		print(paste("Max breaks is too high. Try with ",floor((n-1)/int)-1,".", sep=""))
		return()
	}

	#Null model
	null_mod = ar(y_values, order=order)
	null_SSR = sum((null_mod$res[!is.na(null_mod$res)])^2)
	null_BIC = n + n*log(2*pi) + n*log(as.numeric(null_SSR)/n) + log(n)*(order+2)

        # Pre-compute sum of squared residuals (SSRs) for every admissible segment
        ssr_matrix = matrix(Inf, nrow = n, ncol = n)

        if(progress == TRUE){
                writeLines("\nFinding all SSRs.")
                SSR_progress <- txtProgressBar(min = 0, max = (n-int+1), style = 3)
        }

        for(i in 1:(n-int+1)){#Select starting observation of each subsection (constrained by interval size)

                for(j in (i+int-1):n){#Select end observation of each subsection (constrained by interval size)

                        subsect_y = y_values[i:j]
                        subsect_mod = ar(subsect_y, order=order)
                        subsect_SSR = sum((subsect_mod$res[!is.na(subsect_mod$res)])^2)
                        ssr_matrix[i, j] = subsect_SSR

                }

                if(progress == TRUE){
                        setTxtProgressBar(SSR_progress, i)
                }

        }

        if(progress == TRUE){
                close(SSR_progress)
        }

        # Dynamic programming tables for optimal segmentation
        max_segments = max_breaks + 1
        dp = matrix(Inf, nrow = max_segments, ncol = n)
        prev_idx = matrix(NA_integer_, nrow = max_segments, ncol = n)

        valid_ends = int:n
        dp[1, valid_ends] = ssr_matrix[1, valid_ends]

        if(progress == TRUE && max_breaks > 0){
                writeLines("\nOptimizing break combinations.")
                combo_progress <- txtProgressBar(min = 2, max = max_segments, style = 3)
        }

        if(max_breaks > 0){
                for(seg in 2:max_segments){
                        min_end = seg * int
                        if(min_end > n){
                                next
                        }
                        for(end_idx in min_end:n){
                                best_value = Inf
                                best_split = NA_integer_
                                split_min = (seg - 1) * int
                                split_max = end_idx - int
                                if(split_min > split_max){
                                        next
                                }
                                for(split_idx in split_min:split_max){
                                        if(!is.finite(dp[seg-1, split_idx])){
                                                next
                                        }
                                        candidate = dp[seg-1, split_idx] + ssr_matrix[split_idx + 1, end_idx]
                                        if(candidate < best_value){
                                                best_value = candidate
                                                best_split = split_idx
                                        }
                                }
                                dp[seg, end_idx] = best_value
                                prev_idx[seg, end_idx] = best_split
                        }
                        if(progress == TRUE){
                                setTxtProgressBar(combo_progress, seg)
                        }
                }
        }

        if(progress == TRUE && max_breaks > 0){
                close(combo_progress)
        }

        # Extract optimal breakpoints for each number of breaks
        all_breakpoints = vector("list", max_breaks)
        SSRs = rep(NA_real_, max_breaks)

        for(breaks in 1:max_breaks){
                seg = breaks + 1
                if(seg <= nrow(dp) && is.finite(dp[seg, n])){
                        SSRs[breaks] = dp[seg, n]
                        current_end = n
                        current_seg = seg
                        bp = integer(0)
                        while(current_seg > 1){
                                split_point = prev_idx[current_seg, current_end]
                                if(is.na(split_point)){
                                        bp = integer(0)
                                        break
                                }
                                bp = c(split_point, bp)
                                current_end = split_point
                                current_seg = current_seg - 1
                        }
                        all_breakpoints[[breaks]] = bp
                        if(length(bp) > 0){
                                names(all_breakpoints[[breaks]]) = seq_along(bp)
                        }
                }else{
                        all_breakpoints[[breaks]] = NA
                }
        }

        SSRs = c(null_SSR, SSRs)
        names(SSRs) = 0:(length(SSRs)-1)

        BICs = rep(NA_real_, length(SSRs))
        BICs[1] = null_BIC
        if(length(SSRs) > 1){
                for(breaks in 1:max_breaks){
                        if(!is.na(SSRs[breaks + 1])){
                                BICs[breaks + 1] = n + n*log(2*pi) + n*log(SSRs[breaks + 1]/n) + log(n)*breaks*(order+2)
                        }
                }
        }
        names(BICs) = 0:(length(BICs)-1)

        bic_values = BICs
        bic_values[is.na(bic_values)] = Inf
        best_index = which.min(bic_values)
        if(best_index == 1){
                best_breakpoints = NA
        }else{
                best_breakpoints = all_breakpoints[[best_index - 1]]
        }

        final_list = list(best_breakpoints, all_breakpoints, SSRs, BICs)
	names(final_list) = c("Breakpoints", "AllBreakpoints", "SSRs", "BICs")

	return(final_list)

}

#calling the function
if(exists("test_data_2")){
        test_data = test_data_2()
        bp_test = bai_perron.ar(test_data[,1], test_data[,2])
}
