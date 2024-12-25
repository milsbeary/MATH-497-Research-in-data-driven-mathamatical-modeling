#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --account=def-wanghao
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=40
#SBATCH --mem=0

module restore my_modules
Rscript ~/scratch/500pt_450ROC_forclust.R
