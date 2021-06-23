# BGLNCSexplorer

Welcome to the BGLNCSexplorer repo.
The name stands for the class of models that are considered in this project: **BGL**-like (https://doi.org/10.1016/0370-2693(96)00494-7) with type-I seesaw mechanism for **N**eutrinos and a **C**omplex **S**inglet. 

The function of the various files and their interdependence is summarized in the table below:

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
