#!/bin/sh

#SBATCH -N 1
#SBATCH --tasks-per-node=1

#SBATCH -t 00:30:00

#SBATCH -J dataanalysis

#SBATCH -p hep
#SBATCH -A hep2016-1-5
#SBATCH --mail-user=### write your email address here ###
#SBATCH --mail-type=ALL

#SBATCH -o process_%j.out
#SBATCH -e process_%j.err

start=$SECONDS

cat $0

# copy my own files to the node execution directory 
cp -pr $SNIC_BACKUP/Downloads/BGLNCSexplorer/ $SNIC_TMP

# change to the node execution directory
cd $SNIC_TMP

# purge potential modules
module purge 

# load the gcc compilers
module load GCC/9.3.0

# load the other compilers
module load OpenMPI/4.0.3
module load Python/3.8.2
module load SciPy-bundle/2020.03-Python-3.8.2

# install sympy
pip install flavio
pip install matplotlib
pip install sympy

# make a local results directory
mkdir ./Result_data/

# copy the data to the local dir
cp -r $SNIC_NOBACKUP/Result_data/2021-07-07-* ./Result_data/

# analyze the data
cd ./BGLNCSexplorer/
python3 analysis_and_plotting.py

# save the results
cd $SNIC_TMP
cp -pr analysis_* $SNIC_NOBACKUP/Result_data/
rm -r ./Result_data/

duration=$(( SECONDS - start))
echo "Total duration was $duration"


