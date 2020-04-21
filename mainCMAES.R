library(SPOT)
source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")
library(reticulate)

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
TOTAL_FUN_BUDGET = 300

### Recieve Function ID
### 1-24 bbob functions
funID <- as.numeric(args[3])

### NDIM
nDim <- as.numeric(args[4])

###### Algorithm Setup:
###### 
algoID <- as.numeric(args[5])
## 1 CMAES
## 2-3) BO - PM
## 4-5) BO - EI
## 6-7) BO - LCB
## 8) RF
## 9) Interpolating Ranger RF
## 10) BO-Multi (6 internen)
## 11) BO-Mixed-Model (2Besten Infills+2xRF)

if(algoID != 1){
    stop("wrong algoID")
}

################# 
solver <- function(fun,lower,upper,solverParameterList){
    ########target function wrapper
    #tfun <- function(x){
    #    cat(paste("time=",Sys.time(),"samples=",nrow(x),"\n"), file = paste("timeRes/SPOT",paste(args,collapse="_"),sep="_"), append = T)
    #    fun(x)
    #}
    
    popSize <- 6
    
    cma<-import("cma")
    initial <- matrix(runif(length(lower),lower,upper), nrow = 1, ncol = length(lower)) 
    properties = list('bounds'= c(lower[1], upper[1]), 'popsize'= popSize) 
    maxiter = floor(TOTAL_FUN_BUDGET/popSize) - 1
    es<-cma$CMAEvolutionStrategy(initial, 0.5, properties) 
    while(es$countiter<=maxiter){
        x<-es$ask()
        xp<-r_to_py(x)
        es$tell(xp,sapply(x,fun))
    }
    return(es$result$fbest)
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
    wrappedRobotFun <- bbobr::wrapToBBOBFunction(fun = robotFun, functionID = funID, nDim = nDim, algoID = paste("CMAES",paste(args,collapse="_"),sep="_")
                                                 , instanceID = instance, 
                                                 experimentPath = "robotTest")
    solver(wrappedRobotFun,lower = c(rep(c(0.01,0.01),nDim/2)), upper = c(rep(c(10,2*3.14159),nDim/2)), list())
}else{
    runCOCO(solver,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
            functions = funID,solver_name = paste("CMAES",paste(args,collapse="_"),sep="_"))  
}
     
