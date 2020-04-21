###
### This file collects functions that can be used to load and analyze the results of BBOB benchmark results
###

require(bbobr)
source("statisticalRanking.R")
source("multiComparisonTests.R")
library(gridExtra)
#library(plyr)
library(dplyr)

negate_match_df <- function (y, x, on = NULL) 
{
    if (is.null(on)) {
        on <- intersect(names(x), names(y))
        #message("Matching on: ", paste(on, collapse = ", "))
    }
    keys <- join.keys(x, y, on)
    x[!(keys$x %in% keys$y), , drop = FALSE]
}

## Give 1 or multiple lists of settings. Checks if all these settings are present in a given data frame.
## If not it returns the configs which should be rerun
checkForMissingExperiments <- function(listsOfSettings, data){
    for(i in 1:ncol(data)){
        if(typeof(data[,i])=="factor"){
            data[,i] <- as.character(data[,i])
        }
    }
    allMatches <- NULL
    for(l in listsOfSettings){
        requiredSettings <- expand.grid(l)
        for(i in 1:ncol(requiredSettings)){
            requiredSettings[,i] <- as.character(requiredSettings[,i])
        }
        allMatches <- bind_rows(allMatches,negate_match_df(data, requiredSettings))
    }
    allMatches
}


##
## Read one or multiple experiment data folders and save the results into an rds file with the same name
readFoldersAndWriteRDS <- function(dirs, variablesInAlgoName, 
                                   algorithmIDsToReplace,
                                   algorithmNamesToInsert){
    for(dir in dirs){
        df <- readBBOB(dir)
        df <- applyFrNamingScheme(df)
        df <- df %>% select(-"seed")
        df <- appendVariablesByAlgoName(df, variablesInAlgoName)
        df$algName <- replaceConfigVector(df$algoID, algorithmIDsToReplace, algorithmNamesToInsert)
        df$nDim <- as.numeric(as.character(df$nDim))
        saveRDS(df,paste0(dir,".rds"))
    }
}

##
## In order to figure out the parameters with which an algorithm was run on bbob, the algorithm name is parsed.
## This assumes that the algorithm name was appended with all run parameters before the algorithm was run.
## Each parameter is seperated with a '_'. Keeping the order of the parameters is required!
cutParametersFromString <- function(strName, 
                                    paramNameList = c("seed","budget","functionID","infillID","nDim")){
  require(stringr)
  paramValues <- str_split(strName,"_")[[1]][-1]
  names(paramValues) <- paramNameList
  return(as.data.frame(t(paramValues), stringsAsFactors=F))
}

## 
## Append a data.frame with additional columns regarding the run parameters of an algorithm.
## The lines are filled based on the the parameters that can be parsed from the algorithm name via 'cutParametersFromString'
appendVariablesByAlgoName <- function(df, paramNameList = c("seed","budget","functionID","infillID","nDim")){
  require(dplyr)
  print("Starting to append variables")
  cutParams <- function(str){cutParametersFromString(str, paramNameList)}
  additionalVars <- bind_rows(pbapply::pblapply(df$algName,cutParams))
  additionalVars[,which(names(additionalVars) %in% names(df))] <- NULL
  return(cbind(df,additionalVars))
}

## 
## Exchange numeric parameters with character strings
## for example replace infillID=1 with infillID=BP
replaceConfigVector <- function(vec, numericLevels, strLevels){
  for(i in 1:length(numericLevels)){
    nLev <- numericLevels[i]
    strLev <- strLevels[i]
    if(length(which(vec == nLev)) > 0){
      vec[which(vec == nLev)] <- strLev
    }
  }
  return(vec)
}

cleanAlgoName <- function(at){
  splits <- stringr::str_split(at$algName,stringr::fixed("_"))
  splits <- sapply(splits,function(x) x[1])
  at$algName <- splits
  return(at)
}

addVariablesToAlgoName <- function(at, varNames){
  for(v in varNames){
    at$algName <- paste(at$algName,at[[v]],sep="_")
  }
  return(at)
}

## Create a convergence plot
convergencePlotBBOB <- function(at, title = ""){
  require(dplyr)
    
  #at <- at %>%  mutate(isSPOT = str_starts(algName,"SPOT"))
    at <- at %>% group_by(iteration, nDim, algName) %>%  mutate(upper = summary(y)[[5]]) %>%
        mutate(lower = summary(y)[[2]]) %>% mutate(med = summary(y)[[3]]) %>% mutate(mmin = mean(y))
  
  if(any(as.numeric(as.character(at$functionID)) > 24)){
      at$nDim[at$nDim=="8"] <- "Snake"
      at$nDim[at$nDim=="12"] <- "Gecko"
      at$nDim[at$nDim=="16"] <- "Spider"
  }
  
    ggplot(at, aes(x=iteration,y=med))  + 
        geom_line(aes(color = algName)) + 
        geom_ribbon(aes(fill = algName, ymin=lower, ymax=upper), alpha=0.3) + 
        #facet_grid(rows = vars(),cols = vars(nDim), scales = "free_y") + 
        facet_wrap(facets = vars(nDim), nrow = 1, scales = "free") + 
        theme(text=element_text(size=16)) + 
        labs(x="iteration", y = paste0(title,"\ny")) + scale_y_continuous(trans = 'log10')# +
        #ggtitle(title)
}

bbobBoxPlot <- function(at, title = "", iter = 100){
    at <- at %>% filter(iteration == iter)
    
    ranks <- NULL
    for(dim in unique(at$nDim)){
        d <- filter(at, nDim == dim)
        
        ### only for robot functions!!!! Remove this later!!!
        factor <- 1
        if(unique(as.numeric(as.character(at$functionID))) > 24){
            factor <- -1
        }
        lRank <- rankDataByPostHocKruskalConover(d$y*factor, as.factor(d$algName))
        
        ranks <- rbind(ranks, data.frame("algName" = names(lRank), "rank" = lRank, "nDim" = dim))
    }
    rownames(ranks) <- NULL
    for(i in 1:nrow(ranks)){
        ranks$up[i] <- summary(at$y[at$algName == ranks[i,]$algName & at$nDim == ranks[i,]$nDim])[[5]]
    }
    
    if(any(as.numeric(as.character(at$functionID)) > 24)){
        at$nDim[at$nDim=="8"] <- "Snake"
        at$nDim[at$nDim=="12"] <- "Gecko"
        at$nDim[at$nDim=="16"] <- "Spider"
        ranks$nDim[ranks$nDim=="8"] <- "Snake"
        ranks$nDim[ranks$nDim=="12"] <- "Gecko"
        ranks$nDim[ranks$nDim=="16"] <- "Spider"
    }
    
    ggplot(at, aes(x=algName,y=y))  + geom_boxplot() + facet_wrap(facets = vars(nDim), nrow = 1, scales = "free") + 
        theme(text=element_text(size=16)) + scale_y_continuous(trans = 'log10') +
        labs(x="Algorithm", y = paste0(title, "\nEvaluation: ", iter, "\ny")) + theme(axis.text.x = element_text(angle = 90)) +
        geom_text(
            data    = ranks,
            mapping = aes(y = up, label = rank),
            hjust   = -0.1,
            vjust   = -1,
            color = "red"
        )# + ggtitle()
}

errorBarPlot <- function(at, title = ""){
    require(dplyr)
    
    #at <- at %>%  mutate(isSPOT = str_starts(algName,"SPOT"))
    at <- at %>% group_by(iteration, nDim, algName) %>%  mutate(upper = summary(y)[[5]]) %>%
        mutate(lower = summary(y)[[2]]) %>% mutate(med = summary(y)[[3]]) %>% mutate(mmin = mean(y))
    
    ## calculate positions for error bars
    desPos <- seq(21,101,30)
    ind <- unique(at$iteration)
    ind <- ind[-which(ind==20)]
    ind <- ind[-which(ind==40)]
    chosenP <- NULL
    for(p in desPos){
        chosenP <- c(chosenP,ind[which.min(abs(ind-p))])
    }

    errorBarData <- at %>% filter(iteration %in% chosenP)
    
    ggplot(at, aes(x=iteration,y=med))  + 
        geom_line(aes(color = algName)) + 
        #geom_ribbon(aes(fill = algName, ymin=lower, ymax=upper), alpha=0.3)+#+, linetype=2) + 
        geom_errorbar(data = errorBarData, aes(ymin=lower, ymax=upper, color = algName), width=15, position = "dodge") +
        #facet_grid(rows = vars(),cols = vars(nDim), scales = "free_y") + 
        facet_wrap(facets = vars(nDim), nrow = 1, scales = "free") + 
        theme(text=element_text(size=16)) + 
        labs(x="iteration", y = "y") + scale_y_continuous(trans = 'log10') +
        ggtitle(title)
}

rankWilcox <- function(at,...){
  y1 <- filter(at, infillID == "PM")$y
  y2 <- filter(at, infillID == "EI")$y
  
  res <- suppressWarnings(wilcox.test(x = y1, y = y2, alternative = "two.sided", paired = F, conf.int = T))
  if(is.na(res$p.value)){
    return(data.frame("dominant" = NA, "p" = res$p.value, "leading" = "PM"))
  }
  if(res$p.value > 0.05){
    return(data.frame("dominant" = NA, "p" = res$p.value, "leading" = ifelse(res$estimate < 0,"PM","EI")))
  }
  if(res$estimate < 0){
    return(data.frame("dominant" = "PM", "p" = res$p.value, "leading" = "PM"))
  }
  return(data.frame("dominant" = "EI", "p" = res$p.value, "leading" = "EI"))
}

rankTTest <- function(at,...){
  y1 <- filter(at, infillID == "PM")$y
  y2 <- filter(at, infillID == "EI")$y
  
  res <- suppressWarnings(t.test(x = y1, y = y2, alternative = "two.sided", paired = F, conf.int = T))
  if(is.na(res$p.value)){
    return(data.frame("dominant" = NA, "p" = res$p.value, "leading" = "PM"))
  }
  if(res$p.value > 0.05){
    return(data.frame("dominant" = NA, "p" = res$p.value, "leading" = ifelse(res$estimate[1] < res$estimate[2],"PM","EI")))
  }
  if(res$estimate[1] < res$estimate[2]){
    return(data.frame("dominant" = "PM", "p" = res$p.value, "leading" = "PM"))
  }
  return(data.frame("dominant" = "EI", "p" = res$p.value, "leading" = "EI"))
}

rankEIvsBP <- function(at, iter, dim, returnSum = T, useWilcox = T){
  ## Filter the data for the given iteration and dimension
  at <- filter(at, iteration == iter)
  at <- filter(at, nDim == dim)
  
  ## Group by functions
  at <- at %>% group_by(functionID)
  
  if(nrow(at) == 0){
    warning("Missing Data in Rank Test")
    return(NULL)
  }
  
  ## Calculate ranks (1-x) and subtract 1 to know amount of dominations instead of rank
  if(useWilcox){
    rank <- group_modify(at, rankWilcox) 
  }else{
    rank <- group_modify(at, rankTTest) 
  }
  
  
  if(!returnSum){
    rank$iter <- iter
    rank$nDim <- dim
    return(rank)
  }
  
  ## Aggregate sum over all functions
  sumRank <- rank %>% group_by() %>% 
    mutate(sumEI = length(which(rank$dominant == "EI"))) %>% 
    mutate(sumBP = length(which(rank$dominant == "PM"))) 
  
  ## Remove unnecessary rows and columns from aggregated df
  sumRank <- sumRank %>% filter(functionID == min(as.numeric(as.character(sumRank$functionID))))
  sumRank <- sumRank[,c(3:4)]
  
  ## Add columns with run-information
  sumRank$iter <- iter
  sumRank$nDim <- dim
  sumRank
}

## Rank EI vs BP at a given iteration and dimension
## 
rankEIvsBPMultiComparison <- function(at, iter, dim){
  appRankData <- function(at,...){
    df <- suppressWarnings(rankDataByPostHocKruskalConover(
      at$y,as.factor(at$infillID)))
    df <- t(data.frame(df))
    return(as.data.frame(df))
  }
  
  ## Filter the data for the given iteration and dimension
  at <- filter(at, iteration == iter)
  at <- filter(at, nDim == dim)
  
  ## Group by functions
  at <- at %>% group_by(functionID)
  
  if(nrow(at) == 0){
    warning("Missing Data in Rank Test")
    return(NULL)
  }
  
  ## Calculate ranks (1-x) and subtract 1 to know amount of dominations instead of rank
  rank <- group_modify(at, appRankData) 
  rank[,2:3] <- rank[,2:3]-1
  
  ## Aggregate sum over all functions
  sumRank <- rank %>% group_by() %>% 
    mutate(sumEI = sum(EI)) %>% 
    mutate(sumBP = sum(PM)) 
  
  ## Remove unnecessary rows and columns from aggregated df
  sumRank <- sumRank %>% filter(functionID == min(as.numeric(as.character(sumRank$functionID))))
  sumRank <- sumRank[,c(4:5)]
  
  ## Add columns with run-information
  sumRank$iter <- iter
  sumRank$nDim <- dim
  sumRank
}

##
## Count amount of dominations and plot over given iterations and dimensions
rankDominatedAmountsEIvsBP <- function(at, iters = unique(at$iteration), nDims = unique(at$nDim), doPlot = T){
  ## Iterate over all dimensions and iters, apply ranking function and collect result in single data.frame
  df <- NULL
  for(d in nDims){
    getSingleDF <- function(i){
      df <- rankEIvsBP(at,i,d)
      df
    }
    df <- dplyr::bind_rows(df,lapply(iters,getSingleDF))
  }
  
  names(df) <- c("EI","PM","iteration","nDim")
  
  df <- df[,c(2,1,3,4)]
  
  if(doPlot){
    ## melt the data for plotting
    melted <- reshape2::melt(df, id.vars = c("iteration","nDim"))
    names(melted) <- c("iteration","nDim","InfillCriterion","CountDominated")

    ## generate plot
    p <- ggplot(melted, aes(x=iteration, y = CountDominated)) + geom_line(aes(color = InfillCriterion)) + 
      facet_grid(rows = vars(),cols = vars(nDim), scales = "free_y") + 
      theme(text=element_text(size=16)) + ylab("Amount of Dominations") + xlab("Iteration")
    return(p)
  }
  df
}

##
## modes: onlySign, contPVal, discPVal
##
tilePlotDominations <- function(at, useWilcox = T, plotMode = "onlySign", removeLegend = F){
  iters <- unique(at$iteration)
  iters <- iters[iters >= 10]
  iters <- iters[!iters %in% c(20,30,40,60,150,200,250)]
  nDims <- unique(at$nDim)
  
  df <- NULL
  for(d in nDims){
    getSingleDF <- function(i){
      df <- rankEIvsBP(at,i,d,F, useWilcox)
      df
    }
    df <- dplyr::bind_rows(df,lapply(iters,getSingleDF))
  }
  
  df$functionID <- factor(df$functionID, levels = c(24:1))
  df$iter <- as.factor(df$iter)
  df$dominant <- factor(df$dominant, levels = c("PM","EI"))
  df$groupID <- c(rep(1,5),rep(2,4),rep(3,5),rep(4,5),rep(5,5))
  df$groupID <- paste("Group:", df$groupID)
  df$p <- 1 - df$p
  #df$nDim <- paste("nDim:", df$nDim)
  df$p[df$leading == "EI"] <- -df$p[df$leading == "EI"]
  df$p[abs(df$p) >= 0.95] <- 2*df$p[abs(df$p) >= 0.95]
  
  if(plotMode == "onlySign"){
    p <- ggplot(df, aes(iter, functionID)) +
        geom_tile(aes(fill = df$dominant), colour = "grey10", width = 0.9, height = 0.9, size = 0.1) +
        facet_grid(rows = vars(nDim),cols = vars()) +
        scale_y_discrete(breaks=c(1,5,10,15,20,24)) + 
        scale_x_discrete(breaks=iters[seq(1, length(iters), 3)]) + 
        scale_fill_manual(values = c("red","blue","grey90")) + 
        theme(text=element_text(size=16)) + 
        ylab("BBOB Function ID") + xlab("Iteration") + labs("fill" = "Dominant")
  }
  if(plotMode == "contPVal"){
    p <- ggplot(df, aes(iter, functionID)) +
        geom_tile(aes(fill = df$p), colour = "grey10", width = 0.9, height = 0.9, size = 0.1) +
        facet_grid(rows = vars(nDim),cols = vars()) +
        scale_y_discrete(breaks=c(1,5,10,15,20,24)) + 
        scale_x_discrete(breaks=iters[seq(1, length(iters), 3)]) + 
        scale_fill_gradient2(low = "blue", mid = "white",
                             high = "red", midpoint = 0, space = "Lab",
                             na.value = "grey50", guide = "colourbar", aesthetics = "fill") +
        theme(text=element_text(size=16)) + 
        ylab("BBOB Function ID") + xlab("Iteration") + labs("fill" = "PM>0")
  }
  if(plotMode == "discPVal"){
    df <- df %>% group_by() %>% mutate(pLevel = cut(df$p, breaks=c(-Inf,-0.95,-0.9,-0.8,-0.5,0.5,0.8,0.9,0.95,Inf), 
                                     labels=c("-0.05","-0.10","-0.20","-0.50","0","0.50","0.20","0.10","0.05")))
    df$pLevel <- as.numeric(as.character(df$pLevel))
    #df$pLevel <- factor(df$pLevel, levels = c("EI-0.05","EI-0.10","EI-0.20","EI-0.50","X","PM-0.50","PM-0.20","PM-0.10","PM-0.05"))
    p <- ggplot(df, aes(iter, functionID)) +
        geom_tile(aes(fill = df$pLevel), colour = "grey10", width = 0.9, height = 0.9, size = 0.1) +
        facet_grid(rows = vars(nDim),cols = vars()) +
        scale_y_discrete(breaks=c(1,5,10,15,20,24)) + 
        scale_x_discrete(breaks=iters[seq(1, length(iters), 3)]) + 
        scale_fill_manual(values = c("#0000FF","#0505FF","#1010FF","#1010FF","grey90","#FF1010","#FF1010","#FF0505","#FF0000")) + 
        theme(text=element_text(size=16)) + 
        ylab("BBOB Function ID") + xlab("Iteration") + labs("fill" = "Dominant")
      # ggplot(df, aes(iter, functionID)) +
      #   geom_tile(aes(fill = df$pLevel), colour = "grey10", width = 0.9, height = 0.9, size = 0.1) +
      #   facet_grid(rows = vars(nDim),cols = vars()) +
      #   scale_y_discrete(breaks=c(1,5,10,15,20,24)) + 
      #   scale_x_discrete(breaks=iters[seq(1, length(iters), 3)]) + 
      #   scale_fill_gradient2(low = "blue", mid = "white",
      #                        high = "red",midpoint = 0, space = "Lab",
      #                        na.value = "grey50", guide = "colourbar", aesthetics = "fill")+
      #   theme(text=element_text(size=16)) + 
      #   ylab("BBOB Function ID") + xlab("Iteration") + labs("fill" = "Dominant")
  }
  if(removeLegend){
    p <- p + theme(legend.position="none")
  }
  return(p)
}


