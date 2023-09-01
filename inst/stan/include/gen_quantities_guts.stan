/* Code adapted from Virgile Baudrot
https://github.com/virgile-baudrot/gutsRstan */

array[nData_Nsurv] int Nsurv_ppc;
array[nData_Nsurv] int Nsurv_sim;
array[nData_Nsurv] int Nsurv_sim_prec;

vector[nData_Nsurv] log_lik;

for(gr in 1:nGroup){
  /* binomial_rng function cannot be vectorized, so we need to use a loop*/
   for(i in idS_lw[gr]:idS_up[gr]){
     Nsurv_ppc[i] = binomial_rng(Nprec[i], Conditional_Psurv_hat[i]);

     Nsurv_sim_prec[i] = i == idS_lw[gr] ? Nprec[i] : Nsurv_sim[i-1] ;

     Nsurv_sim[i] = binomial_rng(Nsurv_sim_prec[i], Conditional_Psurv_hat[i]);

     log_lik[i] = binomial_lpmf(Nsurv[idS_lw[gr]:idS_up[gr]] | Nprec[idS_lw[gr]:idS_up[gr]], Conditional_Psurv_hat[idS_lw[gr]:idS_up[gr]]);
   }
}
