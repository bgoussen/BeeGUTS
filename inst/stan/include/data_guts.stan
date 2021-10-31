/* Code adapted from Virgile Baudrot
https://github.com/virgile-baudrot/gutsRstan */

// Number of datasets
int <lower=1> nDatasets;

// Number of groups
int<lower=1> nGroup; // Number of groups (one group is combination of one dataset and one treatment)
int groupDataset[nGroup]; // Corresponding dataset for each group

// Concentration
int<lower=1> nData_conc; // length of data for concentration
real conc[nData_conc]; // concentration
real tconc[nData_conc]; // time of concentration

int<lower=1> idC_lw[nGroup]; // e.g. 1 6 12 18
int<lower=1> idC_up[nGroup]; // e.g. 6 12 18 24

// Survivors
int<lower=1> nData_Nsurv; // number of group: 4
int Nsurv[nData_Nsurv];
int Nprec[nData_Nsurv];
real tNsurv[nData_Nsurv]; // time of Nbr survival

int<lower=1> idS_lw[nGroup]; // e.g. 1 6 12 18
int<lower=1> idS_up[nGroup]; // e.g. 6 12 18 24

// PRIORS
real hbMean_log10;
real hbSD_log10;
real kdMean_log10;
real kdSD_log10;

// Parameters for integration of differentiol equations
real relTol;
real absTol;
int maxSteps;
