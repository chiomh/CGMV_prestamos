## Replication files for "Income Contingent University Loans: Policy Design and an Application to Spain", by A. Cabrales, M. Güell, R. Madera, and A. Viola

This file documents computer programs to replicate the results in this paper. 

## Obtaining the MCVL data 

This paper uses anonymized administrative data from the Muestra Continua de Vidas Laborales (MCVL) with the permission of Spain's Dirección General de Ordenación de la Seguridad Social. 
We are not permitted to give this data to others. 
Interested researchers can request access to the data to the Spain's Dirección General de Ordenación de la Seguridad Social.
For the required steps to obtain the data and to build the raw panel data, we refer the reader to two excellent sources:

- De la Roca and Puga's replication website that can be found here https://diegopuga.org/data/mcvl/, with instructions on how to apply for the data. 
- Detailed files on building a panel and cleaning the data by Cristina Lafuente https://github.com/crisla/MCVL.

## Replicating the results in the paper

### Figure 2: Quantiles of Log Earnings Over Life-Cycle
To compare empirical and simulated earnings ditribution, run simulations/figure2.m with the appropriate paths.

### Figures 3-8 and Table 4: Main Results
To replicate main results in the paper, run all codes in folder stata. In particular 3_make_graphs creates the graphs after the loan laboratory has been calculated in loan_lab. 2_loan_lab is pretty slow.

### Table 4: Quality
Turn global parameter "qualityadj = 1" and rerun codes in stata.

