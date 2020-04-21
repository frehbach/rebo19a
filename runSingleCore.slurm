#!/bin/bash
#SBATCH --job-name=FR
#SBATCH --time=1-23:00:00
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=1024
#SBATCH --partition=all
#SBATCH --output=slurmOut/%A-%a.out
#SBATCH --exclude=
#SBATCH --mail-user=frederik.re@gmail.com
#SBATCH --mail-type=NONE

source ~/.bashrc
echo $PATH
which R
