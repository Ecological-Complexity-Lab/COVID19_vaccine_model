# Pipeline for work

These are general instructions of how to run simulations on HPC.

## Step 1: run a simulation

Submit a job using `qsub run_sim.sh EXPERIMENT_ID`. `EXPERIMENT_ID` is the ID number of the experiment form the file experiments.csv. This file contains all the model and implemntation parameters. Every job sent to the HPC has a unique JOB_ID. We take advantage of that to link a specific run to its parameters and results. When the simulation is executed, it will produce a file called `JOB_ID_run_summary.csv` (e.g., `4192476_run_summary.csv`), which contains all the relevant parameters of the specific simulation. An example:


|parameter|value|Description|
|-------|----|------------|
|JOB_ID|8299549|Job ID on the HPC|
|exp_id|2|experiment id in experiments.csv|
|current_country|Israel|Country to initialize population structure and other parameters|
|sim_weeks|24|Number of weeks to run the simulation|
|m|0.4|Prob. of asymptomatic infection|
|gamma|0.142|1/time to recovery|
|alpha|0.156|1/time of incubation|
|phi|0.47|1/time of presymptomatic|
|eta|1|1/time to quarantine|
|vacc_eff|0.95|vaccine efficiency, e|
|beta|0.38|Rate of infection|
|prop_vacc|0.8|Proportion accepting vaccines|
|b_p|0.5|Scaling parameter for presymptomatic vs symptomatic|
|active_infected|100|Number of infected individuals at time 0|
|k_min|0.1|minimum daily vaccination deployment (% of total population)|
|k_max|0.5|maximum daily vaccination deployment (% of total population)|
|comment|Sensitivity analysis gamma| |

The second file produced is: `4192476_results_Israel.csv` (`JOBID_results_country.csv`), which contains all the data of the simulation.

These files are stored in the main folder and will be used when parsing the results.

## Step 2: Parse results of a simulation

Submit a job using `qsub parse_sim.sh`. This file defines the parameters for the simulation in the line:
```
Rscript parse_sim_results.R 4192476 12
```
 The first number is the JOB_ID for which we want to parse the results. The second number is the number of weeks we want to take from the simulation. For example, even of a simulation was run for 30 weeks we may want to take the results only for the first 12. The code producess all the plots into PDFs and each file has the JOB_ID at the beginning.