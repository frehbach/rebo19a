###
### Main file for running a single optimization with spot
### The file takes a set of configuration parameters via the command line and applies them to alter 
### the optimization run
###

library(SPOT)
source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")
source("funRobots.R")

args = commandArgs(trailingOnly=TRUE)

### RUN Parameters #########################################
# 1) Instance ID
# 2) Seed / Repeat
# 3) Function ID of BBOB set
# 4) nDim - Amount of dimensions 
# 5) Algo Setup ID

### Recieve Seed
### 
instance <- as.numeric(args[1])
seed <- as.numeric(args[2])
set.seed(seed)

### Recieve BUDGET
TOTAL_FUN_BUDGET = 100

### Recieve Function ID
### 
### 1-24 bbob functions
funID <- as.numeric(args[3])

### NDIM
### 
nDim <- as.numeric(args[4])

###### Algorithm Setup:
###### 
algoID <- as.numeric(args[5])

######################################################################################################

solver <- function(fun,lower,upper,solverParameterList){
  for(i in 1:TOTAL_FUN_BUDGET){
    fun(runif(length(lower), min = lower, max = upper))
  }
}

if(funID > 24){
    if(funID == 25){
        robotFun <- createRobotFitness(paste("robot",paste(args,collapse="_"),sep="_"),"gBase.yaml")
    }
    if(funID == 26){
        robotFun <- createRobotFitness(paste("robot",paste(args,collapse="_"),sep="_"),"spBase.yaml")
    }
    if(funID == 27){
        robotFun <- createRobotFitness(paste("robot",paste(args,collapse="_"),sep="_"),"snBase.yaml")
    }
    wrappedRobotFun <- bbobr::wrapToBBOBFunction(fun = robotFun, functionID = funID, nDim = nDim, 
                                                 algoID = paste("RandomSearch",paste(args,collapse="_"),sep="_")
                                                 , instanceID = instance, 
                                                 experimentPath = "exdata")
    solver(wrappedRobotFun,lower = c(rep(c(0.01,0.01),nDim/2)), upper = c(rep(c(10,2*3.14159),nDim/2)), list())
}else{
    runCOCO(solver,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
            functions = funID,solver_name = paste("RandomSearch",paste(args,collapse="_"),sep="_"))
}



