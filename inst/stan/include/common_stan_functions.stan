/* for multiple .stan files */

/* bisectioning search of the index i such that sorted[i] < x < sorted[i+1]
  this come from Sebastian Weber
  */
int find_interval_elem(real x, vector sorted, int start_ind){
      int res;
      int N;
      int max_iter;
      real left;
      real right;
      int left_ind;
      int right_ind;
      int iter;

      N = num_elements(sorted);

      if(N == 0) return(0);

      left_ind  = start_ind;
      right_ind = N;

      max_iter = 100 * N;
      left  = sorted[left_ind ] - x;
      right = sorted[right_ind] - x;

      if(0 <= left)  return(left_ind-1);
      if(0 == right) return(N-1);
      if(0 >  right) return(N);

      iter = 1;
      while((right_ind - left_ind) > 1  && iter != max_iter) {
        int mid_ind;
        real mid;
        // is there a controlled way without being yelled at with a warning?
        mid_ind = (left_ind + right_ind) / 2;
        mid = sorted[mid_ind] - x;
        if (mid == 0) return(mid_ind-1);
        if (left  * mid < 0) { right = mid; right_ind = mid_ind; }
        if (right * mid < 0) { left  = mid; left_ind  = mid_ind; }
        iter = iter + 1;
      }
      if(iter == max_iter) print("Maximum number of iterations reached.");
      return(left_ind);
 }
 /*    Function for linear interpolation*/
real linearInterp( real t_x, real t_before, real t_after, real y_before, real y_after){
    real linInterp_hat;

    linInterp_hat = y_before + (t_x - t_before) * (y_after - y_before)/(t_after-t_before);

    return(linInterp_hat);
}

 /*    Function for loglogistic law */
real loglogistic_2_lpdf(real y_hat, real mw, real beta) {
      return log(beta) - log(mw) + (beta - 1) * (log(y_hat) - log(mw)) -
             2 * log1p_exp(beta * (log(y_hat) - log(mw)));
}

real  loglogistic_2_lcdf(real y_hat, real mw, real beta){
  return -log1p_exp(-beta * (log(y_hat) - log(mw))) ;
}

real  loglogistic_2_lccdf(real y_hat, real mw, real beta){
  return -log1p_exp(beta * (log(y_hat) - log(mw))) ;
}
