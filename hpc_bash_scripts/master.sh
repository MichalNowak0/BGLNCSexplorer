#!/bin/sh

#SBATCH -N 1
#SBATCH --tasks-per-node=2

#SBATCH -t 00:10:00

#SBATCH -J jobFarm

#SBATCH -p hep
#SBATCH -A hep2016-1-5
#SBATCH --mail-user=### write your email address here ###
#SBATCH --mail-type=ALL

#SBATCH -o res_jobFarm_%j.out
#SBATCH -e res_jobFarm_%j.err

cat $0

# set the number of jobs
export NB_of_jobs=10

# loop over the job number
for ((i=0; i<$NB_of_jobs; i++))
do
    # run the worker script:
    srun --exclusive --overlap -n 1 -N 1 workScript.sh $i &> worker_${SLURM_JOB_ID}_${i} &
    sleep 1

done

wait
