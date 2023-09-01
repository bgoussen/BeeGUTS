/* Code adapted from Virgile Baudrot
https://github.com/virgile-baudrot/gutsRstan */

// Number of datasets
int <lower=1> nDatasets;

// Number of groups
int<lower=1> nGroup; // Number of groups (one group is combination of one dataset and one treatment)
array[nGroup] int groupDataset; // Corresponding dataset for each group

// Concentration
int<lower=1> nData_conc; // length of data for concentration
array[nData_conc] real conc; // concentration
array[nData_conc] real tconc; // time of concentration

array[nGroup] int<lower=1> idC_lw; // e.g. 1 6 12 18
array[nGroup] int<lower=1> idC_up; // e.g. 6 12 18 24

// Survivors
int<lower=1> nData_Nsurv; // number of group: 4
array[nData_Nsurv] int Nsurv;
array[nData_Nsurv] int Nprec;
array[nData_Nsurv] real tNsurv; // time of Nbr survival

array[nGroup] int<lower=1> idS_lw; // e.g. 1 6 12 18
array[nGroup] int<lower=1> idS_up; // e.g. 6 12 18 24

// PRIORS
real hbMean_log10;
real hbSD_log10;
real kdMean_log10;
real kdSD_log10;

// Parameters for integration of differentiol equations
real relTol;
real absTol;
int maxSteps;
