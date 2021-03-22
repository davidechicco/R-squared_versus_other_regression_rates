
options(stringsAsFactors = FALSE)
# library("clusterSim")

list.of.packages <- c("easypackages",  "Metrics", "MLmetrics", "polynom", "polynomF" )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library("easypackages")
libraries(list.of.packages)
# script_dir <- dirname(sys.frame(1)$ofile)
# cat("script_dir: ", script_dir, "\n", sep="")
source("utils.r")

MST  <- function(data){return(mean((data-mean(data))**2))}
MSE <- function(real,pred){return(mean((real-pred)**2))}

r2 <- function(real,pred){return( 1 - MSE(real,pred)/MST(real))}
SMAPE <- function(real,pred){return( 100*mean( ( abs(real-pred) )/( (abs(real)+abs(pred))/2   ))) }
cnSMAPE <- function(real,pred){return(1-SMAPE(real,pred)/200)}
delta <- function(real,pred) {return(abs(r2(real,pred)-cnSMAPE(real,pred)))}

MAX_NUM <- 100

real <- 1:MAX_NUM
pred <- 1:MAX_NUM

cat(" : : : : : : : : : : : : : : : : : : :\n")
cat("\tR^2 cnSMAPE\n")
for(j in 5:20){
    pred[seq(from=1,by=5, length.out=j)] <- 0

#     cat("real ")
#     print(real)
#     cat("\n")
#     
#     cat("pred ")
#     print(pred)
#     cat("\n")
    
    print(c(r2(real=real,pred=pred), cnSMAPE(real=real,pred=pred)))
}
cat(" : : : : : : : : : : : : : : : : : : :\n")


########## Ex 2 #######################

cat(" : : : : : : : : : : : : : : : : : : :\n")
cat("\tR^2   cnSMAPE\n")
pred[1:10] <- 0
print(c(r2(real=real,pred=pred),cnSMAPE(real=real,pred=pred)))

pred[51:60] <- 0
print(c(r2(real=real,pred=pred),cnSMAPE(real=real,pred=pred)))

pred[91:100] <- 0
print(c(r2(real=real,pred=pred),cnSMAPE(real=real,pred=pred)))
cat(" : : : : : : : : : : : : : : : : : : :\n")


########## Ex 3 #######################

cat(" : : : : : : : : : : : : : : : : : : :\n")
cat("\tR^2   cnSMAPE\n")
print(c(r2(real=real,pred=pred-1),cnSMAPE(real=real,pred=pred-1)))
print(c(r2(real=real,pred=pred-10),cnSMAPE(real=real,pred=pred-10)))
print(c(r2(real=real,pred=pred-30),cnSMAPE(real=real,pred=pred-30)))


########## Ex 4 #######################

cat(" : : : : : : : : : : : : : : : : : : :\n")
cat("\tR^2   cnSMAPE\n")
pred[1:25] <- 25
print(c(r2(real=real,pred=pred),cnSMAPE(real=real,pred=pred)))

pred[76:100] <- 76
print(c(r2(real=real,pred=pred),cnSMAPE(real=real,pred=pred)))

########## Ex 5 #######################



line <- 1:20
real <- jitter(line,amount=2)
plot(1:20,1:20,type="l")
points(1:20,real)
f <- as.function(poly.calc(1:10, real[1:10]))
t <- seq(1,10,0.01)
points(t,f(t),type="l",col="blue")
pred1 <- 1:20
pred2 <- f(1:20)
points(1:20,pred2)

cat(" : : : : : : : : : : : : : : : : : : :\n")
cat("\tR^2   cnSMAPE\n")

for(j in 2:20){
  print(c(j,r2(real=real[1:j],pred=pred1[1:j]),cnSMAPE(real=real[1:j],pred=pred1[1:j])))
  print(c(j,r2(real=real[1:j],pred=pred2[1:j]),cnSMAPE(real=real[1:j],pred=pred2[1:j])))
}


########## Ex 6 #######################

N <- 10
real <- runif(N,0,100)
pred <- rep(mean(real),N)-runif(10,0,5)
newpred <- pred
diff <- 10**6
tol <- 10**-4
while (diff>tol) {
newpred[N] <- pred[N]-runif(1,-5,5)
newdiff <- abs(MST(real)-MSE(real=real,pred=newpred))
if(newdiff<diff)
  {pred <- newpred
  print(newdiff)
  diff <- newdiff
}

}
print(real)
print(pred)
MST(real)
MSE(real=real,pred=pred)
r2(real=real,pred=pred)
