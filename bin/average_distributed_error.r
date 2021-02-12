

# average_error_per_value
average_error_per_value <- function(chosen_value, actual_nums, predicted_nums) 
{

    errorSum <- 0

    cat("length(actual_nums) = ", length(actual_nums), "\n")
    cat("chosen_value = ", chosen_value, "\n")
    
    for(j in 1:length(actual_nums)) {
    
      #      cat("j = ", j, "\n")
    
            if(actual_nums[j] == chosen_value) {
            
                    thisDiff <- 0
                    thisDiff <- abs(actual_nums[j] - predicted_nums[j])
                    errorSum <- errorSum + thisDiff
                    }
    }
    
    cat("average_error_per_value() output = ", errorSum, "\n")
    return(errorSum)
}

# average_distributed_error_function
average_distributed_error_function <- function(actual_numbers, predicted_numbers) 
{
   values_and_frequencies <- as.data.frame(table(actual_numbers), stringsAsFactors=FALSE)
   values_and_frequencies$"actual_numbers" <- as.numeric(values_and_frequencies$"actual_numbers")
   
   averageErrorSum<- 0

   for(i in 1:nrow(values_and_frequencies)) {
   
        thisValue <- values_and_frequencies[i,]$"actual_numbers"
        thisValueFrequency <- values_and_frequencies[i,]$"Freq"
        cat("\nthisValue =", thisValue, "\t")
        
        thisValueErrorSum <- average_error_per_value(thisValue, actual_numbers, predicted_numbers) 
        cat("thisValueErrorSum =", thisValueErrorSum, "\t")
        
        averageErrorSum <- averageErrorSum +  thisValueErrorSum / thisValueFrequency
        cat("averageErrorSum =", averageErrorSum, "\n")
   }
   
    cat("average_distributed_error_function() output = ", averageErrorSum, "\n")
    return(averageErrorSum/length(actual_numbers))
}


source("confusion_matrix_and_regression_rates.r")

actual_values <- c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4)
# predicted_values <-sample.int(max(actual_values), length(actual_values), replace = TRUE)
predicted_values <- c(5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
regression_rates(actual_values, predicted_values)

final_average_distributed_error <- average_distributed_error_function(actual_values, predicted_values)
cat("final_average_distributed_error = ", final_average_distributed_error, "\n")