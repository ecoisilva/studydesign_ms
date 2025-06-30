#!/bin/bash
#SBATCH --job-name=speed_gaz_option1            # name of the job
#SBATCH --partition=defq,intel        # partition to be used (defq, gpu or intel)
#SBATCH --time=03:00:00              # walltime (up to 96 hours)
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

if [ -f logs.csv ]; then
 echo "logs.csv already exists! Continuing..."
else
 echo "Creating output file: logs.csv"
 echo "job_name,species,analysis,filename,dti_value,dti_unit,dur_value,dur_unit,inds,runtime_hrs" > logs.csv
fi

date

# Arguments:
species="gazelle"
individuals_no=8
dur_value=1
dur_unit="year"
dti_value=15
dti_unit="minutes"

add_individual_variation="TRUE"

echo

echo "*********************************************************"
echo "*** Initiating script ******************************* ***"
echo "*** - script: mean_speed.R ************************** ***"
echo
Rscript mean_speed.R \
${SLURM_CPUS_PER_TASK} \
${species} \
${individuals_no} \
${dur_value} ${dur_unit} ${dti_value} ${dti_unit} \
${add_individual_variation} \
${SLURM_JOB_NAME}

echo
echo "*** script completed ******************************** ***"
echo "***************************************************** ***"
date

conda deactivate
