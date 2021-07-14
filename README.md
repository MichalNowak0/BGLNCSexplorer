# BGLNCSexplorer

Welcome to the BGLNCSexplorer repo.
The name stands for the class of models that are considered in this project: **BGL**-like (https://doi.org/10.1016/0370-2693(96)00494-7) with type-I seesaw mechanism for **N**eutrinos and a **C**omplex **S**inglet. 

The repo contains three main segments. The python files constitute the actual BGLNCSexplorer program. The SARAH folder contains the Mathematica files required to generate the SPheno code which is provided in the SPheno folder. Lastly, the bash script folder contains bash scripts which can be used to run the program in an HPC enviroment (they are configured for the LUNARC AURORA cluster, but can be readily adapted for an other enviroment).

What do you need to do to run the program? 
1. Make sure the paths specified in `wrapper_master-py` and `analysis_and_plotting.py` match those of your installation.
2. Make sure the variable `cluster_run` in `wrapper_master.py` is set to False if you are running the codes locally.

If you want to use the bash scripts to run the program on a cluster, the following needs to be configured:
1. Make sure the paths specified in `master.sh` and `analysis_submit.sh` match your installation.
2. Make sure the variable `cluster_run` in `wrapper_master.py` is set to True.

The function of the various python files and their interdependence is summarized in the table below:

| File | Description | Dependence |
| --- | --- | --- |
| `wrapper_master` | **The main excution file**. | Calls `wrapper_loop` and `data_handling` |
| `wrapper_loop` | The main loop. 1 Iteration = 1 Point. | Calls `inversion_procedure`, `electroweak_precision_observables` and `file_writing`|
| `analysis_and_plotting` | Harvests from files and analyzes the data. **Executed independently from master**. | Calls `visualization`|
| `inversion_procedure` | Implements the inversion procedure for the Yukawa and scalar sectors. | None |
| `electroweak_precision_observables` | Calculates the electroweak precision observables. | None |
| `file_writing` | Saves the preSPheno data, writes the SLHA input file for SPheno. | None |
| `data_handling` | Bunches the data into larger files, tosses out unneeded data. | None |
| `visualization` | Creates the plots. | None |

The function of the bash scripts:

| File | Description | Dependence |
| --- | --- | --- |
| `master.sh` | The execution file of the `wrapper_master.py` chain of codes. Parallerizes the simulation process over processor cores. | Calls `workScript.sh` for each parallel process |
| `workScript.sh` | Runs the `wrapper_master.py` chain of codes. Called for each processor core by `master.sh` | None |
| `analysis_submit.sh` | Analyzes the previously collected and stored data. Uses the `analysis_and_plotting.py` chain of codes. | None |
