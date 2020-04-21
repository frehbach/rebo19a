library(SPOT)
source("runBBOB.R")
source("readBBOB.R")
source("wrapExternalFunction.R")
library(dplyr)

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
        print(x)
        cat(paste("time=",Sys.time(),"samples=",nrow(x),"\n"), file = paste("timeRes/SPOT",paste(args,collapse="_"),sep="_"), append = T)
        matrix(apply(x,1,fun), ncol=1)
    }
    
    X <- designLHD(,lower,upper)
    y <- tfun(X)
    
    modelControl = list(target = c("y", "s"),
                        useLambda = T, 
                        lambdaUpper = -4,
                        algTheta = optimDE,
                        budgetAlgTheta = 250)
    optimizerControl = list(funEvals = nDim * 300, 
                            populationSize = 5 * nDim)
    
    while(nrow(X) < TOTAL_FUN_BUDGET){
        model <- buildKriging(X,y,modelControl)
        model$target <- c("y","s")
        
        evalIPI = function(x,tVal){
            yVal = predict(model, x)$y
            yMin = min(y)
            s = predict(model,x)$s
            return(-(0.5*pnorm((yMin-yVal)/(1.05-tVal))+pnorm((-(s-tVal)^2)/(0.05))))
        }
        
        tVals <- NULL
        for(i in 1:N_CPUS)
            tVals = c(tVals,i*(1/(N_CPUS+1)))
        
        newX = NULL
        for(tVal in tVals)
        {
            newX = rbind(newX, optimDE(,fun = function(x){evalIPI(x,tVal = tVal)}, lower,upper,optimizerControl)$xbest)
        }
        newY <- tfun(newX)
        
        X <- rbind(X, newX)
        y <- rbind(y,newY)
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
    wrappedRobotFun <- bbobr::wrapToBBOBFunction(fun = robotFun, functionID = funID, nDim = nDim, algoID = paste("IPI",paste(args,collapse="_"),sep="_")
                                                 , instanceID = instance, 
                                                 experimentPath = "robotTest")
    solver(wrappedRobotFun,lower = c(rep(c(0.01,0.01),nDim/2)), upper = c(rep(c(10,2*3.14159),nDim/2)), list())
}else{
    runCOCO(solver,current_batch = 1,number_of_batches = 1,dimensions=nDim, instances = instance,
            functions = funID,solver_name = paste("IPI",paste(args,collapse="_"),sep="_"))
}