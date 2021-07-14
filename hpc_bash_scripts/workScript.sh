#!/bin/sh

cat $0

# receive my worker number
export WRK_NB=$1

# create a worker-private subdirectory in $SNIC_TMP
export WRK_DIR=$SNIC_TMP/WRK_${WRK_NB}
mkdir $WRK_DIR

# create a directory where data will be permanently saved:
export JOB_DIR=$SNIC_NOBACKUP/Result_data/$(date +"%Y-%m-%d-%H-%M-%S")_job_${1}/
mkdir $JOB_DIR

# copy the tarballs to the node execution directory
cp -p $SNIC_BACKUP/TarBalls/HiggsBounds-5.3.2beta.tar.gz $SNIC_BACKUP/TarBalls/HiggsSignals-2.2.3beta.tar.gz $WRK_DIR

# copy my own files to the node execution directory 
cp -pr $SNIC_BACKUP/programs/SPheno-4.0.4/ $WRK_DIR
cp -pr $SNIC_BACKUP/Downloads/BGLNCSexplorer/ $WRK_DIR

# change to the node execution directory
cd $WRK_DIR

# purge potantial modules
module purge 

# load the gcc compilers
module load GCC/9.3.0

# construct higgsbounds
cd $WRK_DIR
tar -xf HiggsBounds-5.3.2beta.tar.gz
cd HiggsBounds-5.3.2beta
./configure
make

# construct higgssignals
cd $WRK_DIR
tar -xf HiggsSignals-2.2.3beta.tar.gz
cd HiggsSignals-2.2.3beta
sed -i 's@CONF_HBPATH=".*@CONF_HBPATH="../HiggsBounds-5.3.2beta"@' configure
./configure
make
cd $WRK_DIR

# load the other compilers
module load OpenMPI/4.0.3
module load Python/3.8.2
module load SciPy-bundle/2020.03-Python-3.8.2

# install sympy
pip install sympy

# run the wrapper i.e. generate data
cd  $WRK_DIR/BGLNCSexplorer/
python3 ./wrapper_master.py

# save the results
cp -pr $WRK_DIR/Result_data/* ${JOB_DIR}

# clean up the local disc and remove the worker-private directory
cd $SNIC_TMP
rm -rf WRK_${WRK_NB}

