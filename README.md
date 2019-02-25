## Replication files for "Income Contingent University Loans: Policy Design and an Application to Spain", by A. Cabrales, M. Güell, R. Madera, and A. Viola

This file documents computer programs to replicate the results in this paper. The main results of the paper should be easily replicable using the simulated data in the simulations folder. Given social security data restrictions, availability of empirical results used to calculate the simulation is limited. Don't hesitate to contact us for comments or questions at romadera@smu.edu.

## Obtaining the MCVL data 

This paper uses anonymized administrative data from the Muestra Continua de Vidas Laborales (MCVL) with the permission of Spain's Dirección General de Ordenación de la Seguridad Social. 
We are not permitted to give this data to others. 
Interested researchers can request access to the data to the Spain's Dirección General de Ordenación de la Seguridad Social.
For the required steps to obtain the data and to build the raw panel data, we refer the reader to two excellent sources:

- De la Roca and Puga's replication website that can be found here https://diegopuga.org/data/mcvl/, with instructions on how to apply for the data. 
- Detailed files on building a panel and cleaning the data by Cristina Lafuente https://github.com/crisla/MCVL.

## Replicating the results in the paper

### Figure 2: Quantiles of Log Earnings Over Life-Cycle
To compare empirical and simulated earnings ditribution, run simulations/figure2.m.

### Figures 3-8 and Table 4: Main Results
To replicate main results in the paper, run all codes in folder stata. In particular 3_output_results creates the graphs after the loan laboratory has been calculated in loan_lab. Case_s3_e4.dta is provided in stata/output and it is the output of 2_loan_lab. 2_loan_lab is pretty slow. 

### Table 3: Quality
Turn global parameter "qualityadj = 1" in 1_read_fortran_simul and run 2_loan_lab after to create Case_s3_e4_q.dta. The output is Case_s3_e4_q.dta, which is also provided in stata/output. Then flagging makequality=1 in 3_output_results generates Table3.

