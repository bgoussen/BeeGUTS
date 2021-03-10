/* Code adapted from Virgile Baudrot
https://github.com/virgile-baudrot/gutsRstan */

#include /include/license.stan

functions {

#include /include/common_stan_functions.stan

  real[] TKTD_varSD( real t,
                     real[] y,
                     real[] theta,
                     real[] x_r,
                     int[]  x_i) {

    // - parameters
    real hb = theta[1];
    real kd = theta[2];
    real zw = theta[3];
    real bw = theta[4];

    // - new variables
    real max_zw[2]; //
    real dy_dt[2]; //

    // - latent variables
    int Nconc = x_i[1]; // Number of point measuring concentration

    vector[Nconc] tconc = to_vector(x_r[1:Nconc]);
    vector[Nconc] conc = to_vector(x_r[Nconc+1:2*Nconc]); // to take dose time which is in ts

    int pulse_index = find_interval_elem(t, tconc, 1); // pulse_index index

    // if_else statement
    real conc_linInterp = pulse_index != 0 ? linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] ) : conc[1];
    //real conc_linInterp = linearInterp(t , tconc[pulse_index], tconc[pulse_index+1], conc[pulse_index], conc[pulse_index+1] );

    // - model
    dy_dt[1] =  kd * ( conc_linInterp - y[1]);

    max_zw[1] = 0;
    max_zw[2] = y[1] - zw;

    dy_dt[2] = bw * max(max_zw) + hb;

    return(dy_dt);
  }

  matrix solve_TKTD_varSD(real[] y0, real t0, real[] ts, real[] theta, real[] tconc, real[] conc, real[] odeParam){

    int x_i[1];
    x_i[1] = size(tconc);

    return(to_matrix(
      integrate_ode_rk45(TKTD_varSD, y0, t0, ts, theta,
                         to_array_1d(append_row(to_vector(tconc), to_vector(conc))),
                         x_i,
                         // additional control parameters for the solver: real rel_tol, real abs_tol, int max_num_steps
                          odeParam[1], odeParam[2], odeParam[3])));
  }
}

data {

#include /include/data_guts.stan

  /* PRIORS */
  real bwMean_log10;
  real bwSD_log10;
  real zwMean_log10;
  real zwSD_log10;

}
transformed data{

  real<lower=0> y0[2];
  real odeParam[3];

  real tNsurv_ode[nData_Nsurv]; // time of Nbr survival to include in the ode !
  real tconc_ode[nData_conc]; // time of Nbr survival to include in the ode !

  y0[1] = 0;
  y0[2] = 0;

  // Add odeSolveParameters
  odeParam[1] = relTol;
  odeParam[2] = absTol;
  odeParam[3] = maxSteps;

  for(gr in 1:nGroup){
    tNsurv_ode[idS_lw[gr]:idS_up[gr]] = tNsurv[idS_lw[gr]:idS_up[gr]];
    tNsurv_ode[idS_lw[gr]] = tNsurv[idS_lw[gr]] + 1e-9 ; // to start ode integrator at 0
    tconc_ode[idC_lw[gr]:idC_up[gr]] = tconc[idC_lw[gr]:idC_up[gr]];
    tconc_ode[idC_lw[gr]] = tconc[idC_lw[gr]] + 1e-9 ; // to start ode integrator at 0
  }


}
parameters {

  real sigma[4];

}
transformed parameters{

  real hb_log10 = hbMean_log10 + hbSD_log10 * sigma[1];
  real kd_log10 = kdMean_log10 + kdSD_log10 * sigma[2];
  real zw_log10 = zwMean_log10 + zwSD_log10 * sigma[3];
  real bw_log10 = bwMean_log10 + bwSD_log10 * sigma[4];

  real<lower=0> param[4]; //

  matrix[nData_Nsurv,2] y_hat;
  vector<lower=0, upper=1>[nData_Nsurv] Psurv_hat;
  vector<lower=0, upper=1>[nData_Nsurv] Conditional_Psurv_hat;

  param[1] = 10^hb_log10; // hb
  param[2] = 10^kd_log10; // kd
  param[3] = 10^zw_log10; // zw
  param[4] = 10^bw_log10; // bw

  for(gr in 1:nGroup){
  /* initial time must be less than t0 = 0, so we use a very small close small number 1e-9 to at at time tNsurv and tconc */
    y_hat[idS_lw[gr]:idS_up[gr],1:2] = solve_TKTD_varSD(y0, 0, tNsurv_ode[idS_lw[gr]:idS_up[gr]], param, tconc_ode[idC_lw[gr]:idC_up[gr]], conc[idC_lw[gr]:idC_up[gr]], odeParam);

    Psurv_hat[idS_lw[gr]:idS_up[gr]] = exp( - y_hat[idS_lw[gr]:idS_up[gr], 2]);

    for(i in idS_lw[gr]:idS_up[gr]){

      Conditional_Psurv_hat[i] =  i == idS_lw[gr] ? Psurv_hat[i] : Psurv_hat[i] / Psurv_hat[i-1] ;

    }
  }

}
model {

  target += normal_lpdf(sigma | 0, 1);

  for(gr in 1:nGroup){

    target += binomial_lpmf(Nsurv[idS_lw[gr]:idS_up[gr]] | Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);

    // Nsurv[idS_lw[gr]:idS_up[gr]] ~ binomial( Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);

  }
}
generated quantities {

#include /include/gen_quantities_guts.stan

}

