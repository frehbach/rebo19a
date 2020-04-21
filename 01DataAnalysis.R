##
## Main File for analysing the experiment data and creating plots.
## Running this file should produce .pdf Files with the plots similar to the papers supplementary material.
## Running this Code on the data generated with (FULL_EXPERIMENTS <- F) results in some warnings, these can
## be ignored.
##

library(ggplot2)
source("bbobResultAnalysis.R")

createAllPlots <- function(plotData, strName, type = 0){
    bbobTitles <- c("Sphere",
                    "Ellipsoidal",
                    "Rastrigin",
                    "Büche-Rastrigin",
                    "Linear Slope",
                    "Attractive Sector",
                    "Step Ellipsoidal",
                    "Rosenbrock",
                    "Rosenbrock, rotated",
                    "Ellipsoidal",
                    "Discus",
                    "Bent Cigar",
                    "Sharp Ridge",
                    "Different Powers",
                    "Rastrigin",
                    "Weierstrass",
                    "Schaffers F7",
                    "Schaffers F7",
                    "Griwank-Rosenbrock",
                    "Schwefel",
                    "Gallagher Gaussian 101-me Peaks",
                    "Gallagher Gaussian 21-hi Peaks",
                    "Katsuura",
                    "Lunacek bi-Rastrigin",
                    "Robot-Gait Learning", "Robot-Directed Locomotion")
    h <- 4
    if(type == 2){
        h <- 5
    }
    pdf(strName, width = 9,height = h)
    for(i in 1:26){
        title <- bbobTitles[i]
        factor <- 1
        if(i == 25){
            i <- c(25,26,27)
            factor <- -1
        }else if(i == 26){
            i <- c(28,29,30)
            factor <- -1
        }
        at <- filter(plotData,functionID %in% i)
        if(nrow(at) == 0){
            next
        }
        at$y <- at$y * factor
        if(nrow(at) == 0){
            next
        }
        if(type == 0){
            print(errorBarPlot(at, title = title))
        }else if(type == 1){
            print(convergencePlotBBOB(at, title = title))   
        }else{
            for(it in c(25,50,70,100)){
                print(bbobBoxPlot(at, title = title, iter = it))
            }
        }
    }
    dev.off()
}

## Read All BBOB and Robot Results, write them into rds files for later loading
readFoldersAndWriteRDS(dirs = c("exdata"), 
                       variablesInAlgoName = c("instanceID","seed","functionID","nDim","algoID"),
                       algorithmIDsToReplace = c(1:10,13,14,15,16),
                       algorithmNamesToInsert = c("CMAES","BO-PV-FitP", "BO-PV-P2"
                                                  ,"BO-EI-FitP", "BO-EI-P2"
                                                  ,"BO-LCB-FitP", "BO-LCB-P2",
                                                  "MK",
                                                  "IPI", "Q-EI","DE","MOI",
                                                  "Pareto-SPOT","RS"))

## Preprocess and clean data
df <- readRDS("exdata.rds")

df <- df %>% filter(iteration <= 100)
df <- distinct(df)

createAllPlots(filter(df,algoID %in% as.character(c(1,8:10,14,16))), "convergencePlots.pdf", type = 1)
createAllPlots(filter(df,algoID %in% as.character(c(1,8:10,14,16))), "rankedBoxPlots.pdf", type = 2)

createAllPlots(filter(df,algoID %in% as.character(c(1:7,16))), "singleCoreConvergencePlots.pdf", type = 1)
createAllPlots(filter(df,algoID %in% as.character(c(1:7,16))), "singleCoreRankedBoxPlots.pdf", type = 2)
