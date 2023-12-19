#!/bin/bash
#SBATCH -J MyArrayJob       # name of my job
#SBATCH -A eecs          # name of slurm account to use
#SBATCH -p share         # name of partition/queue to use
#SBATCH --array=1-260
#SBATCH -o rray_%A_%a.out # name of output file for batch script
#SBATCH -e array_%A_%a.err  # name of error file for batch script 
#SBATCH -n 1             # number of tasks
#SBATCH -c 1             # number of cores per task
#SBATCH --mail-user=$mark.novak@oregonstate.edu
#SBATCH --mail-type=ALL

# gather basic information, can be useful for troubleshooting
hostname
date
echo $SLURM_JOBID
showjob $SLURM_JOBID

# load modules needed for job
module unload gcc/5.1.0
module load gcc/9.2.0
module load R/4.2.1

# run my job

Rscript fit_datasets.R $SLURM_ARRAY_TASK_ID

