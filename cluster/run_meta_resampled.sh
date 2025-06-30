#!/bin/bash
#SBATCH --job-name=meta_buf_speed          # name of the job
#SBATCH --partition=defq,intel        # partition to be used (defq, gpu or intel)
#SBATCH --time=05:00:00            # walltime (up to 96 hours)
#SBATCH --nodes=1                     # number of nodes
#SBATCH --ntasks=1                    # number of parallel processes
#SBATCH --cpus-per-task=1             # number of cores per node
#SBATCH --mem-per-cpu=40GB            # increase memory available to each process
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=i.simoes-silva@hzdr.de
#SBATCH --output=logs/slurm-%A.out

module load miniconda
conda init bash
source activate main-env

cd /home/simoes48/studydesign_ms/cluster

date

# Arguments:
max_samples=250 # max number of resamples
iter_step=1 # number of individuals per set
n_files=1 # number of files below

filenames_array=(
  "buffalo_speed_dti1day_dur4years_60inds"
)

filenames="${filenames_array[*]}"

echo

echo "*********************************************************"
echo "*** Initiating script ******************************* ***"
echo "*** - script: meta_resampled.R ********************** ***"
echo
Rscript meta_resampled.R \
${max_samples} \
${iter_step} \
${n_files} \
${filenames}

echo
echo "*** script completed ******************************** ***"
echo "***************************************************** ***"
date

conda deactivate
