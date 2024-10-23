/* Code adapted from Virgile Baudrot
https://github.com/virgile-baudrot/gutsRstan */

#include /include/license.stan

functions {

#include /include/common_stan_functions.stan
}
data {

int <lower=1> nDatasets;

// Number of groups
int<lower=1> nGroup; // Number of groups (one group is combination of one dataset and one treatment)

// Survivors
int<lower=1> nData_Nsurv; // number of group: 4
array[nData_Nsurv] int Nsurv;
array[nData_Nsurv] int Nprec;
array[nData_Nsurv] real tNsurv; // time of Nbr survival

int<lower=1> idS_lw; // e.g. 1 6 12 18
int<lower=1> idS_up; // e.g. 6 12 18 24

// PRIORS
real hbMean_log10;
real hbSD_log10;
}
transformed data{


}
parameters {

  real sigma;

}
transformed parameters{

  real hb_log10;

  real<lower=0> param; //

  vector<lower=0, upper=1>[nData_Nsurv] Psurv_hat;
  vector<lower=0, upper=1>[nData_Nsurv] Conditional_Psurv_hat;

  hb_log10  = hbMean_log10 + hbSD_log10 * sigma;

  for(i in 1:nData_Nsurv){
    param = 10^hb_log10; // hb

    Psurv_hat[i] = exp( - param * tNsurv[i]);
    Conditional_Psurv_hat[i] =  i == 1 ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;
  }

}
model {

  target += normal_lpdf(sigma | 0, 1);

  target += binomial_lpmf(Nsurv[idS_lw:idS_up] | Nprec[idS_lw:idS_up], Conditional_Psurv_hat[idS_lw:idS_up]);
}
generated quantities {

  array[nData_Nsurv] int Nsurv_ppc;
  array[nData_Nsurv] int Nsurv_sim;
  array[nData_Nsurv] int Nsurv_sim_prec;

  vector[nData_Nsurv] log_lik;

  /* binomial_rng function cannot be vectorized, so we need to use a loop*/
   for(i in idS_lw:idS_up){
     Nsurv_ppc[i] = binomial_rng(Nprec[i], Conditional_Psurv_hat[i]);

     Nsurv_sim_prec[i] = i == idS_lw ? Nprec[i] : Nsurv_sim[i-1] ;

     Nsurv_sim[i] = binomial_rng(Nsurv_sim_prec[i], Conditional_Psurv_hat[i]);

     log_lik[i] = binomial_lpmf(Nsurv[idS_lw:idS_up] | Nprec[idS_lw:idS_up], Conditional_Psurv_hat[idS_lw:idS_up]);
   }
}

