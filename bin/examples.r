
options(stringsAsFactors = FALSE)
# library("clusterSim")

list.of.packages <- c("easypackages", "PRROC", "e1071", "Metrics", "MLmetrics", "rcompanion", "irr" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("easypackages")
libraries(new.packages)
# script_dir <- dirname(sys.frame(1)$ofile)
# cat("script_dir: ", script_dir, "\n", sep="")
source("utils.r")

source("confusion_matrix_and_regression_rates.r")

MAX_NUM_ELEMENTS <- 5
MAX_NUM_POSSIBLE_VALUES <- 5

cat("actual \t predictions \t nonNegRsquared \t cnSMAPE \t |delta| \t SMAPE \n", sep="")
cat("actual \t predictions \t [0, 1] \t\t [0, 1] \t\t [0, 1] \t [0, 2] \n")

for(a in 1:MAX_NUM_POSSIBLE_VALUES) {
  for(b in 1:MAX_NUM_POSSIBLE_VALUES) {
      for(c in 1:MAX_NUM_POSSIBLE_VALUES) {
        for(d in 1:MAX_NUM_POSSIBLE_VALUES) {
            for(e in 1:MAX_NUM_POSSIBLE_VALUES) {
                        
                    actual_values <- c(1:MAX_NUM_ELEMENTS) 
                    predicted_values <- c(a,b,c,d,e)
                    
                    thisR2scoreAlternativeComputation <- MLmetrics::R2_Score(predicted_values, actual_values) 
                    
                    # Let's do only if the R-squared is positive
                    if( thisR2scoreAlternativeComputation >= 0) {
                            
        #                     cat("actual_values\n")
                            cat(actual_values)
                            cat("\t")
        #                     cat("predicted_values\n")
                            cat(predicted_values)
                            cat("\t")
                            
                            regression_rates_results <- regression_rates(actual_values, predicted_values)
                    }
                }
            }
        }
    }
}