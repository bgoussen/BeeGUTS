/* Code adapted from Virgile Baudrot
https://github.com/virgile-baudrot/gutsRstan */

#include /include/license.stan

functions {

#include /include/common_stan_functions.stan

  vector TKTD_varIT( real t,
                     vector y,
                     array[] real theta,
                     array[] real x_r,
                     array[] int  x_i) {

    // - parameters
    real kd = theta[1];

    // - new variables
    vector[1] dy_dt; //

      // - latent variables
    int Nconc = x_i[1]; // Number of point measuring concentration

    vector[Nconc] tconc = to_vector(x_r[1:Nconc]);
    vector[Nconc] conc = to_vector(x_r[Nconc+1:2*Nconc]); // to take dose time which is in ts

    int pulse_index = find_interval_elem(t, tconc, 1); // pulse_index index

    // if_else statement
    real conc_linInterp = pulse_index != 0 ? linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] ) : conc[1];

    // - model
    dy_dt[1] =  kd * ( conc_linInterp - y[1]);

    return(dy_dt);
  }

  matrix solve_TKTD_varIT(array[] real y0, real t0, array[] real ts, array[] real theta, data array[] real tconc, data array[] real conc, data real relTol, data real absTol, int maxSteps){

    array[1] int x_i;
    x_i[1] = size(tconc);
    array[size(ts)] vector[1] ode_res
      = ode_rk45_tol(TKTD_varIT, to_vector(y0), t0, ts,
                         // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
                         relTol, absTol, maxSteps, theta,
                         to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
                         x_i);
    matrix[size(ts), 1] rtn;
    for(i in 1:size(ts)) {
      rtn[i] = transpose(ode_res[i]);
    }
    return rtn;
  }
}

data {

#include /include/data_guts.stan

   int distribution;

   /* PRIORS */
   real mwMean_log10;
   real mwSD_log10;
   real betaMin_log10;
   real betaMax_log10;
}
transformed data{

  array[1] real<lower=0> y0;

  array[nData_Nsurv] real tNsurv_ode; // time of Nbr survival to include in the ode !
  array[nData_conc] real tconc_ode; // time of Nbr survival to include in the ode !

  y0[1] = 1e-20; // cannot start at 0 for log(0)

  for(gr in 1:nGroup){
    tNsurv_ode[idS_lw[gr]:idS_up[gr]] = tNsurv[idS_lw[gr]:idS_up[gr]];
    tNsurv_ode[idS_lw[gr]] = tNsurv[idS_lw[gr]] + 1e-9 ; // to start ode integrator at 0
    tconc_ode[idC_lw[gr]:idC_up[gr]] = tconc[idC_lw[gr]:idC_up[gr]];
    tconc_ode[idC_lw[gr]] = tconc[idC_lw[gr]] + 1e-9 ; // to start ode integrator at 0
  }

}
parameters {

  real beta_log10  ;

  array[2 + nDatasets] real sigma ;


}
transformed parameters{

  real kd_log10 = kdMean_log10 + kdSD_log10 * sigma[1] ;
  real mw_log10 = mwMean_log10 + mwSD_log10 * sigma[2] ;
  array[nDatasets] real hb_log10;

  array[1] real<lower=0> param; //

  matrix[nData_Nsurv, 1] y_hat;
  vector<lower=0, upper=1>[nData_Nsurv] Psurv_hat;
  vector<lower=0, upper=1>[nData_Nsurv] Conditional_Psurv_hat;

  real mw = 10^mw_log10; // mw
  real beta = 10^beta_log10; // beta

  for(i in  1:nDatasets){
    hb_log10[i]  = hbMean_log10 + hbSD_log10 * sigma[2+i];
  }

  param[1] = 10^kd_log10; // kd

  for(gr in 1:nGroup){
     real hb  = 10^hb_log10[groupDataset[gr]]; // hb
    /* initial time must be less than t0 = 0, so we use a very small close small number -1e-9 */
      y_hat[idS_lw[gr]:idS_up[gr], 1] = solve_TKTD_varIT(y0, 0, tNsurv_ode[idS_lw[gr]:idS_up[gr]], param, tconc_ode[idC_lw[gr]:idC_up[gr]], conc[idC_lw[gr]:idC_up[gr]], relTol, absTol, maxSteps)[,1];

    for(i in idS_lw[gr]:idS_up[gr]){

     if(distribution == 1){
       Psurv_hat[i] = exp(- hb * tNsurv_ode[i]) * (1-exp(loglogistic_2_lcdf(max(y_hat[idS_lw[gr]:i, 1]) | mw, beta)));
     }
     if(distribution == 2){
       Psurv_hat[i] = exp(- hb * tNsurv_ode[i]) * (1-exp(lognormal_lcdf(max(y_hat[idS_lw[gr]:i, 1]) | mw, beta)));
     }

     Conditional_Psurv_hat[i] =  i == idS_lw[gr] ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;

    }

  }
}
model {

  target += uniform_lpdf(beta_log10 | betaMin_log10 , betaMax_log10 );

  target += normal_lpdf(sigma | 0, 1);

  for(gr in 1:nGroup){

    target += binomial_lpmf(Nsurv[idS_lw[gr]:idS_up[gr]] | Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);

  }
}
generated quantities {

#include /include/gen_quantities_guts.stan

}
