# rebo19a
Supplementary Codes for "Parallelized Bayesian Optimization for Expensive Robot Controller Evolution"

The experiments are based on the linux task manager SLURM.
The files should be run in the order:
* 00RunExperiments.R - In order to run the experiments. After submitting them to slurm wait for task completion. You might need to adapt 'runSingleCore.slurm' to your local cluster configuration.
* 01DataAnalysis.R - In order to generate the plots as given in the papers' supplementary material.
* 02cleanAll.R - In order to remove all output files, for a fresh restart of the experiments.

To run the robot experiments first pull the docker image using the command `docker pull cigroup/revolve:master` and modify it with the given Dockerfile
