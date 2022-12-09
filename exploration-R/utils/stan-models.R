
stan_normal_reliability <- function() {
  
  stan_normal_reliability <- write_stan_file("
data {
  int n_data;
  int n_subj;
  vector[n_data] response;
  array[n_data] int subj;
  array[n_data] int x; // timepoint
}

transformed data {
  real scale_cont = sqrt(2) / 4;
  real scale_cat = 1.0/2;
}

parameters {
  cholesky_factor_corr[2] L; //cholesky factor of covariance
  vector<lower=0>[2] L_std;
  real<lower=0> sigma_error;
  real mu_ic;
  real mu_time;
  matrix[n_subj, 2] tau_rs;
}

transformed parameters {
  vector[n_data] mu_rs;
  vector[2] mu_zeros;
  
  for (n in 1:n_data) {
    mu_rs[n] = mu_time * (x[n] - 1.5) + tau_rs[subj[n], x[n]];
  }
  mu_zeros[1] = 0;
  mu_zeros[2] = 0;
}

model {
  for (n in 1:n_data) {
    response[n] ~ normal(mu_rs[n], sigma_error);
    
  }

  L ~ lkj_corr_cholesky(1);
  L_std ~ normal(0, 2.5);
  matrix[2, 2] L_Sigma = diag_pre_multiply(L_std, L);
  

  for (s in 1:n_subj) {
    tau_rs[s] ~ multi_normal_cholesky(mu_zeros, L_Sigma);
  }
  
  sigma_error ~ gamma(1, 1);
  mu_time ~ normal(0, 1);
}
 
  
generated quantities {
 corr_matrix[2] Sigma;
 array[n_data] real log_lik_pred;

 Sigma = multiply_lower_tri_self_transpose(L);

 for (n in 1:n_data) {
   log_lik_pred[n] = normal_lpdf(response[n] | mu_rs[n], sigma_error);
 }
}

")
  return(stan_normal_reliability)
}



