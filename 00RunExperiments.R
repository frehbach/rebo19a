###
### Main File for starting the Experiments.
### There is a small test set of experiments, with reduced budget, repeats, etc. 
### Use this test set (FULL_EXPERIMENTS <- F) to see that all the codes etc. are running correctl on your system. 
### Once that is working you can try to run the full experiment set (FULL_EXPERIMENTS <- T)
###

## Should the full experiments be run? 
## Otherwise a quick test set is run
FULL_EXPERIMENTS <- F

##
## Function for starting a specific set of experiments
runExperiments <- function(idList, startFile = "mainSPOT.R"){
    configs <- expand.grid(idList)
    
    print(paste("Starting Experiments on:", startFile))
    dir.create("slurmOut", showWarnings = FALSE)
    
    startSingle <- function(r){
        print(r)
        system(paste0("cp runSingleCore.slurm delete.slurm"))
        write(paste("/opt/software/R/R-current/bin/Rscript", startFile ,paste(r, collapse=" ")),file="delete.slurm",append=TRUE)
        system("sbatch delete.slurm")
        system("rm delete.slurm") 
    }
    
    apply(configs, 1, startSingle)
    print("done!")
}

if(FULL_EXPERIMENTS){
    ## 
    ## Robot Experiments
    ## 
    instanceID <- 1
    seed <- 1:30
    
    all.funs <- c(25,26,37)
    all.d <- c(12,16,8)
    for(i in 1:length(funs)){
        funs <- all.funs[i]
        d <- all.d[i]
        
        algoID <- 1
        startParams <- list(instanceID,seed,funs,d, algoID)
        runExperiments(startParams,"mainCMAES.R")
        algoID <- 7
        startParams <- list(instanceID,seed,funs,d, algoID)
        runExperiments(startParams,"mainSPOT.R")
        algoID <- 9
        startParams <- list(instanceID,seed,funs,d, algoID)
        runExperiments(startParams,"mainIPI.R")
        algoID <- 10
        startParams <- list(instanceID,seed,funs,d, algoID)
        runExperiments(startParams,"mainQEI.R")
        algoID <- 14
        startParams <- list(instanceID,seed,funs,d, algoID)
        runExperiments(startParams,"mainMlrMbo.R")
        algoID <- 16
        startParams <- list(instanceID,seed,funs,d, algoID)
        runExperiments(startParams,"mainRandomSearch.R")
    }
    
    ##
    ## BBOB Experiments
    ##
    
    instanceID <- 1:15
    seed <- 1:2
    funs <- 1:24
    d <- c(5,10,20)
    
    algoID <- 1
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainCMAES.R")
    algoID <- 7
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainSPOT.R")
    algoID <- 9
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainIPI.R")
    algoID <- 10
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainQEI.R")
    algoID <- 14
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainMlrMbo.R")
    algoID <- 16
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainRandomSearch.R")
}else{
    ##
    ## BBOB Experiments
    ##
    
    instanceID <- 1:3
    seed <- 1
    funs <- 1
    d <- 5
    
    algoID <- 1
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainCMAES.R")
    algoID <- 7
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainSPOT.R")
    algoID <- 9
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainIPI.R")
    algoID <- 10
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainQEI.R")
    algoID <- 14
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainMlrMbo.R")
    algoID <- 16
    startParams <- list(instanceID,seed,funs,d, algoID)
    runExperiments(startParams,"mainRandomSearch.R")
}
