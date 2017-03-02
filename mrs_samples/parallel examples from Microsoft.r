#------------------------------------------------------------------------------
# Denver R User Group, 8/31/2016 - Parallel Computing
# There are a number of different parallel backends and a number of ways to 
# use them.  This example covers a few of these options
#------------------------------------------------------------------------------

# load the packages we'll be using
if (!require("doParallel")) install.packages("doParallel")

library(parallel)
library(doParallel)

# figure out how many cores are available
(computeCores <- detectCores())

## First, let's look at some of the available ways to set the parallel backends
# just specify the number of cores
registerDoParallel(cores=computeCores)	#register the backend
stopImplicitCluster()					#stop the backend

# register a || backend
#cl <- makeForkCluster(computeCores)	# On Linux forking is available
cl <- makePSOCKcluster(computeCores)	
registerDoParallel(cl)
stopCluster(cl)

## Other things to check out
# additional packages: doFuture, doMC, doMPI, doRedis, doRNG, doSNOW
# additional useful commands:
getDoParName()		# Currently registered doPar backend
getDoParWorkers()	# How many workers will be used
registerDoSEQ()		# Stopping the cluster is normally sufficient but this clears
# the registered backend
??parallel			# the parallel package, has really useful documentation

## let's try some examples (roughly from https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf)
evalVect <- 1:1e4

# sequential
(timeSeq <- system.time(
  sqrtSeq <- foreach(i=evalVect) %do%  sqrt(i)))

# parallel
registerDoParallel(cores=computeCores)
(timePar <- system.time(
  sqrtPar <- foreach(i=evalVect) %dopar%  sqrt(i)))
stopImplicitCluster()

# confirm the output is the same
identical(sqrtSeq, sqrtPar)


#-----
## Bootstrap example, based on the example in vignette("gettingstartedParallel")
x <- iris[which(iris[,5] != "setosa"), c(1,5)]	# Create a subset of the iris data set 
# (col's 1 and 5) for all species other than setosa

trials <- 10000				# Number of bootstrap trials
registerDoParallel(cores=computeCores)

system.time(
  r <- foreach(icount(trials), .combine=cbind) %dopar% {
    ind <- sample(100, 100, replace=TRUE)	# randomly sample 1 of the 100 records in x
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  })

system.time(
  r <- foreach(icount(trials), .combine=cbind) %do% {
    ind <- sample(100, 100, replace=TRUE)	# randomly sample 1 of the 100 records in x
    result1 <- glm(x[ind,2]~x[ind,1], family=binomial(logit))
    coefficients(result1)
  })
stopImplicitCluster()


# Revolution has additional parallel backends that can be registered
registerDoParallel(cores=computeCores)
rxSetComputeContext(RxForeachDoPar())
timeRxExec <- system.time(
  rxExec(sqrt, rxElemArg(evalVect), taskChunkSize=length(evalVect)/computeCores))
stopImplicitCluster()


rbind(timeSeq, timePar, timeRxExec)

