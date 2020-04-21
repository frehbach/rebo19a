library(stringr)

fillYamlWithParams <- function(baseFile, targetFile, x){
    lines <- readLines(baseFile)
    index <- 1
    for(i in 1:length(lines)){
        if(str_detect(lines[i], fixed("##"))){
            lines[i] <- str_replace(lines[i], fixed("##"), x[index])
            index <- index + 1
        }
    }
    
    cat(lines, file = targetFile, sep = "\n")
}

runDockerContainer <- function(robotName){
    try({
        dockercmd<-"docker run --rm -v /mnt/data/frRebo19a:/revolve/experiments/bodies robot/controller:version2 bin/bash -c "
        revolvecmd <-paste0("cd revolve; ./revolve.py --manager experiments/examples/getFitness.py --robot-name ", 
                            robotName)
        
        call<-paste0(dockercmd, shQuote(revolvecmd, type = "sh"))
        print(call)
        dockeroutput<-system(call,intern = TRUE,ignore.stderr = TRUE, timeout = 90)
        
        for(line in dockeroutput){
            if(str_starts(line, fixed("**The Data is:"))){
                datastr <- str_split(line, fixed("**The Data is:"))
            }
        }
        datastr<-str_trim(datastr[[1]][2])
        clean_datastr<-strsplit(datastr,",")[[1]]
        clean_datastr<-str_remove_all(clean_datastr, fixed("["))
        clean_datastr<-str_remove_all(clean_datastr, fixed("]"))
        values_docker <- as.numeric(clean_datastr)
                                                                    
        #######
        ####### Which Fitness !!
        #######
        return(values_docker[2])
    })
    return(-1)
}

createRobotFitness<-function(robotName, baseRobot)
{
    singularDockerID <- 0
    return(function(x){
        singularDockerID <- singularDockerID + 1
        targetFile <- paste0("/mnt/data/frRebo19a/", robotName, "_", singularDockerID,".yaml")
        evalRobotName <- paste0(robotName, "_", singularDockerID)
        baseFile <- paste0("/mnt/data/frRebo19a/", baseRobot)
        fillYamlWithParams(baseFile, targetFile, x)
        res <- mean(replicate(5,runDockerContainer(evalRobotName)))
        unlink(targetFile)
        return(-res)
    })
}
