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

### Recieve Seed + Instance
instance <- as.numeric(args[1])
seed <- as.numeric(args[2])
set.seed(seed)

### BUDGET
TOTAL_FUN_BUDGET = 102

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

control <- list()
parControl <- list()

infillLCB <- function(predictionList, model){
    mean <- predictionList$y
    sd <- predictionList$s
    return(mean-sd)
}

## Kriging / BO runs
if(algoID %in% c(2:7)){
    control$model <- buildKriging
}
if(algoID %in% c(2:3)){
    control$infillCriterion <- NULL
}
if(algoID %in% c(4:5)){
    control$infillCriterion <- infillExpectedImprovement
}
if(algoID %in% c(6:7)){
    control$infillCriterion <- infillLCB
}

## BO Kriging SETUP:
if(algoID %in% c(2:7)){
    control$modelControl = list(target = c("y", "s"),
                                useLambda = T, 
                                lambdaUpper = -4,
                                algTheta = optimDE,
                                budgetAlgTheta = 250)
    ## Even Numbers do Optimize P in the kernel
    if(algoID %in% c(2,4,6)){
        control$modelControl$optimizeP <- T
    }else{
        control$modelControl$optimizeP <- F
    }
}

if(algoID == 8){
    for(i in 1:6){
        parControl[[i]] <- list(optimizer = optimDE,
                                optimizerControl = list(funEvals = nDim * 300, 
                                                        populationSize = 5 * nDim),
                                model = buildKriging,
                                modelControl = list(target = c("y", "s"),
                                                    useLambda = T, 
                                                    lambdaUpper = -4,
                                                    algTheta = optimDE,
                                                    budgetAlgTheta = 250))
        if(i %in% c(1,2)){
            parControl[[i]]$infillCriterion <- NULL
        }
        if(i %in% c(3,4)){
            parControl[[i]]$infillCriterion <- infillExpectedImprovement
        }
        if(i %in% c(5,6)){
            parControl[[i]]$infillCriterion <- infillLCB
        }
        if(algoID %in% c(1,3,5)){
            parControl[[i]]$modelControl$optimizeP <- T
        }else{
            parControl[[i]]$modelControl$optimizeP <- F
        }
    }
}

################# SPOT
solver <- function(fun,lower,upper,solverParameterList){
  ########target function wrapper for SPOT
  tfun <- function(x){
      print(x)
      cat(paste("time=",Sys.time(),"samples=",nrow(x),"\n"), file = paste("timeRes/SPOT",paste(args,collapse="_"),sep="_"), append = T)
    apply(x,1,fun)
  }
  
  control$seedSPOT <- seed
  control$funEvals <- TOTAL_FUN_BUDGET
  
  control$optimizer <- optimDE
  control$optimizerControl <- list(funEvals = length(lower) * 300, 
                                   populationSize = 5 * length(lower))
  
  if(algoID <= 7){
      res <- spot(NULL, tfun, lower, upper, control = control)
  }else{
      res <- parSpot(NULL, tfun, lower, upper, sequentialControlList = control, parallelControlLists = parControl)
  }
  saveRDS(res,paste("spotRes/spotResultOut",paste(args,collapse="_"),sep="_"))
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
    wrappedRobotFun <- bbobr::wrapToBBOBFunction(fun = robotFun, functionID = funID, nDim = nDim, algoID = paste("SPOT",paste(args,collapse="_"),sep="_")
                                                 , instanceID = instance, 
                                                 experimentPath = "robotTest")
    solver(wrappedRobotFun,lower = c(rep(c(0.01,0.01),nDim/2)), upper = c(rep(c(10,2*3.14159),nDim/2)), list())
}else{
    runCOCO(solver,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
            functions = funID,solver_name = paste("SPOT",paste(args,collapse="_"),sep="_"))  
}