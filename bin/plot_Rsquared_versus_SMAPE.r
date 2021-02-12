
options(stringsAsFactors = FALSE)
# library("clusterSim")

list.of.packages <- c("easypackages", "ggplot2" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("easypackages")
libraries(new.packages)
# script_dir <- dirname(sys.frame(1)$ofile)
# cat("script_dir: ", script_dir, "\n", sep="")
source("utils.r")


datafile <- read.csv(file="../results/results_examples.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
datafile$ID <- paste0("ID", rownames(datafile))
rownames(datafile) <- datafile$"ID"


p <- ggplot(datafile, aes(x=nonNegRsquared, y=cnSMAPE)) + xlim(0,1) + ylim(0,1) + geom_point() + geom_smooth()
p
ggsave("../results/example.pdf")
dev.off()