#!/bin/bash

#SBATCH --job-name=un_cc_pc
#SBATCH --account=account_no
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=40:00:00

# Change to the working directory
cd /user/work/username

# Point Stata to your local libncurses copy
export LD_LIBRARY_PATH=/user/work/username/libncurses:$LD_LIBRARY_PATH

# Load the Stata module
module load apps/stata/17

# Run Stata
stata do gformula_unadj_cc_postc.do

# Unload the Stata module
module unload apps/stata/17
