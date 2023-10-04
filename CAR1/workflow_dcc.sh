#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=15:00:00
#SBATCH --mem-per-cpu=8192
#SBATCH --job-name=PASS_ProjTest
#SBATCH --array=1-120
#SBATCH --error=./Cluster_out/PASS_ProjTest.%A_%a.stderr
#SBATCH --output=./Cluster_out/PASS_ProjTest.%A_%a.stdout
#SBATCH --mail-type=END
#SBATCH --mail-type=FAIL
#SBATCH --mail-user=sk770



module load R

Rscript ./Codes/runSim.R PASS_Proj_Test_ufDA $SLURM_ARRAY_TASK_ID 100 FALSE
Rscript ./Codes/runSim.R Extract_Eigencomp_fDA $SLURM_ARRAY_TASK_ID 1000 FALSE
