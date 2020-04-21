###
### Main file for running a single optimization with mlrmbo
### The file takes a set of configuration parameters via the command line and applies them to alter 
### the optimization run
###

library(mlrMBO)
source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")

source("funRobots.R")

### RUN Parameters #########################################
# 1) Instance ID
# 2) Seed / Repeat
# 3) Function ID of BBOB set
# 4) nDim - Amount of dimensions 
# 5) Algo Setup ID
args = commandArgs(trailingOnly=TRUE)

#args = c(1,50,3,2,10)

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

solver <- function(fun,lower,upper,solverParameterList){
    configureMlr(show.learner.output = FALSE)
    
    obj.fun <- makeSingleObjectiveFunction(
        fn = fun,
        par.set = makeNumericParamSet(lower = lower, upper = upper, len = length(lower))
    )
    
    ctrl <- makeMBOControl(propose.points = 6)
    ctrl <- setMBOControlMultiPoint(ctrl, method = "moimbo",
                                    moimbo.objective = "mean.se.dist",
                                    moimbo.dist = "nearest.neighbor",
                                    moimbo.selection = "first",
                                    moimbo.maxit = 1000)
    designSize <- 10L
    if(nDim == 12){
      designSize <- 16L
    }else if(nDim == 16){
      designSize <- 22L
    }
    design <- generateDesign(designSize, getParamSet(obj.fun), fun = lhs::maximinLHS)
    ctrl <- setMBOControlTermination(ctrl, iters = (TOTAL_FUN_BUDGET-designSize)/6)
    
    lrn <- makeMBOLearner(ctrl, obj.fun)
    
    res <- mbo(obj.fun, design = design, learner = lrn,
               control = ctrl, show.info = TRUE)
    
    return(res)
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
                                                 algoID = paste("MlrMbo",paste(args,collapse="_"),sep="_")
                                                 , instanceID = instance, 
                                                 experimentPath = "exdata")
    solver(wrappedRobotFun,lower = c(rep(c(0.01,0.01),nDim/2)), upper = c(rep(c(10,2*3.14159),nDim/2)), list())
}else{
    runCOCO(solver,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
            functions = funID,solver_name = paste("MlrMbo",paste(args,collapse="_"),sep="_"))
}
