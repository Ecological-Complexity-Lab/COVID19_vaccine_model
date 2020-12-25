# Pipeline for work

These are general instructions of how to run simulations on HPC.

## Step 1: run a simulation

Submit a job using `qsub run_sim.sh`. This file defines the parameters for the simulation in the line:
```
Rscript main_hpc.R Israel 12 0.4 7 6.4 1.5
```
The parameters (example values) are:
* country (Israel)
* number of weeks to run (12)
* the asymptomatic probability m (0.4), which is uniform for all ages
* days to recovery (7), to calculate gamma
* days of incubation (6.4), to calculate alpha
* days to quarentine (1.5), to calculate eta

Every job sent to the HPC has a unique JOB_ID. We take advantage of that to link a specific run to its parameters and results. When the simulation is executed, it will produce a file called `JOB_ID_run_summary.csv` (e.g., `4192476_run_summary.csv`), which contains all the relevant parameters of the specific simulation:

|parameter|value|
|-------|------|
JOB_ID|4192476|
|country|Israel|
|N|8712900|
|sim_weeks|5|
|m|0.4|
|beta|0.24|
|q|0.112|
|gamma|0.142|
|alpha|0.156|
|eta|0.6666|

The second file produced is: `4192476_results_Israel.csv` (`JOBID_results_country.csv`), which contains all the data of the simulation.

These files are stored in the main folder and will be used when parsing the results.

## Step 2: Parse results of a simulation

Submit a job using `qsub parse_sim.sh`. This file defines the parameters for the simulation in the line:
```
Rscript parse_sim_results.R 4192476 12
```
 The first number is the JOB_ID for which we want to parse the results. The second number is the number of weeks we want to take from the simulation. For example, even of a simulation was run for 30 weeks we may want to take the results only for the first 12.
 
 The code producess all the plots into PDFs and each file has the JOB_ID at the beginning. E.g.,: `4192476_ea_ae_SD0_by_age.pdf`. All the files related to a particular JOB_ID are then placed in a folder named JOB_ID (e.g., `4192476`), besides the `4192476_run_summary.csv` file.
 
# Fututre changes

This is the basic pipeline. It is also possible to modify it later (with some extensive programming) to do things like:
* Run separate jobs per value of `k`
* Run separate jobs per strategies (e.g., a job for each targeted vaccination strategy`)
