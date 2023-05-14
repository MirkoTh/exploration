
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



stan_choice <- function() {
  
  stan_logistic <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int choice;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, v, ru, vtu
  int n_data_predict;
  array[n_data_predict] int subj_predict;
  matrix[n_data_predict, 4] x_predict; // ic, v, ru, vtu
}

transformed data {
  real scale_cont = sqrt(2) / 4;
}

parameters {
  matrix[n_subj, 4] b;
  vector[4] mu;
  vector <lower=0>[4] sigma_subject;
}

transformed parameters {
  array[4] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = mu[2];
  mu_tf[3] = mu[3];
  mu_tf[4] = mu[4];
  array[n_data] real <lower=0,upper=1> theta;
  
  for (n in 1:n_data) {
    theta[n] = Phi(
    b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + 
    b[subj[n], 3] * x[n, 3] + b[subj[n], 4] * x[n, 4]);
  }
}

model {
  for (n in 1:n_data) {
    choice[n] ~ bernoulli(theta[n]);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
    b[s, 3] ~ normal(mu_tf[3], sigma_subject[3]);
    b[s, 4] ~ normal(mu_tf[4], sigma_subject[4]);
  }

  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  sigma_subject[3] ~ uniform(0.001, 10);
  sigma_subject[4] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ normal(0, 1);
  mu[3] ~ normal(0, 1);
  mu[4] ~ normal(0, 1);
}

generated quantities {
  vector[n_data_predict] posterior_prediction;
  vector[n_data_predict] theta_predict;

  for (n in 1:n_data_predict) {
    theta_predict[n] = Phi(
      b[subj_predict[n], 1] * x_predict[n, 1] + b[subj_predict[n], 2] * x_predict[n, 2] + 
      b[subj_predict[n], 3] * x_predict[n, 3] + b[subj_predict[n], 4] * x_predict[n, 4]
      );
    posterior_prediction[n] = bernoulli_rng(theta_predict[n]);
  }
}

")
  return(stan_logistic)
}


stan_choice_reduced <- function() {
  
  stan_logistic <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int choice;
  array[n_data] int subj;
  matrix[n_data, 3] x; // ic, v/vtu, ru
  int n_data_predict;
  array[n_data_predict] int subj_predict;
  matrix[n_data_predict, 3] x_predict; // ic, v, ru, vtu
}

transformed data {
  real scale_cont = sqrt(2) / 4;
}

parameters {
  matrix[n_subj, 3] b;
  vector[3] mu;
  vector <lower=0>[3] sigma_subject;
}

transformed parameters {
  array[3] real mu_tf;
  mu_tf[1] = mu[1];
  mu_tf[2] = mu[2];
  mu_tf[3] = mu[3];
  array[n_data] real <lower=0,upper=1> theta;
  
  for (n in 1:n_data) {
    theta[n] = Phi(b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + b[subj[n], 3] * x[n, 3]);
  }
}

model {
  for (n in 1:n_data) {
    choice[n] ~ bernoulli(theta[n]);
  }

  for (s in 1:n_subj) {
    b[s, 1] ~ normal(mu_tf[1], sigma_subject[1]);
    b[s, 2] ~ normal(mu_tf[2], sigma_subject[2]);
    b[s, 3] ~ normal(mu_tf[3], sigma_subject[3]);
  }

  sigma_subject[1] ~ uniform(0.001, 10);
  sigma_subject[2] ~ uniform(0.001, 10);
  sigma_subject[3] ~ uniform(0.001, 10);
  mu[1] ~ normal(0, 1);
  mu[2] ~ normal(0, 1);
  mu[3] ~ normal(0, 1);
}


generated quantities {
  vector[n_data_predict] posterior_prediction;
  vector[n_data_predict] theta_predict;

  for (n in 1:n_data_predict) {
    theta_predict[n] = Phi(
      b[subj_predict[n], 1] * x_predict[n, 1] + b[subj_predict[n], 2] * x_predict[n, 2] + 
      b[subj_predict[n], 3] * x_predict[n, 3]
      );
    posterior_prediction[n] = Phi(theta_predict[n]);
  }
}

")
  return(stan_logistic)
}


stan_choice_lkj <- function() {
  
  stan_logistic <- write_stan_file("
data {
  int n_data;
  int n_subj;
  array[n_data] int choice;
  array[n_data] int subj;
  matrix[n_data, 4] x; // ic, v, ru, vtu
  int n_data_predict;
  array[n_data_predict] int subj_predict;
  matrix[n_data_predict, 4] x_predict; // ic, v, ru, vtu
}

transformed data {
  real scale_cont = sqrt(2) / 4;
}

parameters {
  matrix[n_subj, 4] b;
  vector[4] mu;
  vector <lower=0>[4] sigma_subject;
  cholesky_factor_corr[4] L;

}

transformed parameters {

  array[n_data] real <lower=0,upper=1> theta;
  
  for (n in 1:n_data) {
    theta[n] = Phi(
    b[subj[n], 1] * x[n, 1] + b[subj[n], 2] * x[n, 2] + 
    b[subj[n], 3] * x[n, 3] + b[subj[n], 4] * x[n, 4]);
  }
}


model {
  for (n in 1:n_data) {
    choice[n] ~ bernoulli(theta[n]);
  }
  L ~ lkj_corr_cholesky(1);

  for (s in 1:n_subj) {
    b[s] ~ multi_normal_cholesky(mu, diag_pre_multiply(sigma_subject, L));
  }

  sigma_subject ~ uniform(0.001, 10);
  mu ~ normal(0, 1);
}

generated quantities {
  vector[n_data_predict] posterior_prediction;
  vector[n_data_predict] theta_predict;
  corr_matrix[4] Sigma;
  
  Sigma = multiply_lower_tri_self_transpose(L);

  for (n in 1:n_data_predict) {
    theta_predict[n] = Phi(
      b[subj_predict[n], 1] * x_predict[n, 1] + b[subj_predict[n], 2] * x_predict[n, 2] + 
      b[subj_predict[n], 3] * x_predict[n, 3] + b[subj_predict[n], 4] * x_predict[n, 4]
      );
    posterior_prediction[n] = bernoulli_rng(theta_predict[n]);
  }
}

")
  return(stan_logistic)
}





