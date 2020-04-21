library(SPOT)
source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")
library(dplyr)

require(DiceKriging)
require(DiceOptim)
require(mnormt)

source("funRobots.R")

args = commandArgs(trailingOnly=TRUE)

### RUN Parameters #########################################
# 1) Instance ID
# 2) Seed / Repeat
# 3) Function ID of BBOB set
# 4) nDim - Amount of dimensions 
# 5) Algo Setup ID

### Recieve Seed + Instance
instance <- as.numeric(args[1])
seed <- as.numeric(args[2])
set.seed(seed)

### BUDGET
TOTAL_FUN_BUDGET <- 100

## Parallel Factor
N_CPUS <- 6

### Recieve Function ID
### 1-24 bbob functions
funID <- as.numeric(args[3])

### NDIM
nDim <- as.numeric(args[4])

###### Algorithm Setup:
###### 
algoID <- as.numeric(args[5])
## 2-3) BO - PM
## 4-5) BO - EI
## 6-7) BO - LCB
## 8) BO-Multi (6 internen)

solver <- function(fun,lower,upper,solverParameterList){
    ########target function wrapper for SPOT
    tfun <- function(x){
        as.numeric(apply(x,1,fun))
    }
    
    if(nDim == 12){
        X <- designLHD(,lower,upper,control = list(size = 16))
    }else if(nDim == 16){
        X <- designLHD(,lower,upper,control = list(size = 22))
    }else{
        X <- designLHD(,lower,upper)
    }
    y <- tfun(X)
    
    optimizerControl = list(funEvals = nDim * 300, 
                            populationSize = 5 * nDim)
    
    
    
    while(nrow(X) < TOTAL_FUN_BUDGET){
        model = km(~1, design=X, response=y,
                      covtype="gauss", control=list(pop.size=50,trace=FALSE), parinit=c(0.5, 0.5),
                      nugget = 0.000001,nugget.estim = T, iso = T)
        getQEI = function(x){-qEI(matrix(x,nrow = N_CPUS),model)}
        result <- optimDE(,fun = getQEI, rep(lower,N_CPUS),rep(upper,N_CPUS),optimizerControl)$xbest
        
        newX = matrix(result,nrow = N_CPUS)
        newY <- tfun(newX)
        
        X <- rbind(X, newX)
        y <- c(y,newY)
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
    wrappedRobotFun <- bbobr::wrapToBBOBFunction(fun = robotFun, functionID = funID, nDim = nDim, algoID = paste("QEI",paste(args,collapse="_"),sep="_")
                                                 , instanceID = instance, 
                                                 experimentPath = "exdata")
    solver(wrappedRobotFun,lower = c(rep(c(0.01,0.01),nDim/2)), upper = c(rep(c(10,2*3.14159),nDim/2)), list())
}else{
    runCOCO(solver,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
            functions = funID,solver_name = paste("QEI",paste(args,collapse="_"),sep="_"))
}