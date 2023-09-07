#!/bin/sh

# Give the job a name
#$ -N nPrey_simple

#$ -S /bin/sh

# set working directory on all host to
# directory where the job was started
#$ -cwd

# send output to job.log (STDOUT + STDERR)
#$ -o job.log
#$ -j y


# email information
#$ -m e
# Just change the email address.  You will be emailed when the job has finished.
#$ -M mark.novak@oregonstate.edu

#Change which version of R you want to load on the Compute Nodes
module unload gcc/5.1.0
module load gcc/9.2.0
module load R/4.2.1

# command to run.  ONLY CHANGE THE NAME OF YOUR APPLICATION  
Rscript fit_datasets.R
