functions {
  /* Compute p_rational  */
  real choiceprob(real rho, real kappa, real sigma, int include_scalar, real v_l, real rew_multi, real p_l, real v_s) {
    real theta_l; /* probability choosing lottery */
    real stddev_l; /* standard deviation of lottery option*/
    real u_l;	/* Lottery utility */
    real u_s;	/* Surebet utility */
    real noise_term; /* Independent noise or scalar noise */
    
    stddev_l = sqrt(p_l * (v_l^2) * (1.0 - p_l));
    u_l = (p_l * ((v_l*rew_multi) ^ rho)) - (kappa * stddev_l);
    u_s = (1.0 * ((v_s*rew_multi) ^ rho)) - (kappa * 0.0); 
    if (include_scalar) {
      noise_term = sqrt( ((v_l*rew_multi)^2 * sigma^2 + (v_s*rew_multi)^2 * sigma^2) );
    } else{
      noise_term = sqrt(2) * sigma;
    }
    theta_l = normal_cdf(0, u_s - u_l, noise_term);
    return theta_l;
  }
  
  real mixprob(real theta_rational, vector omega, vector omega_win, vector omega_lose, real prev_outcome, real prev_choice) {
    int J = rows(omega); /* number of agents */
    int include_perseverance = (J == 4);
    int include_mix = (J >= 3);
    real theta_mixed;
    vector[J] myomega;
    real perseverancetheta;
    myomega = omega;
    if (prev_outcome == 1){
      myomega = omega_win;
    } else if(prev_outcome == -1){
      myomega = omega_lose;
    }
    if(include_mix){
      theta_mixed = (myomega[1] * theta_rational) + (myomega[2] * 1) + (myomega[3] * 0);
    } else{
      theta_mixed = myomega[1] * theta_rational;
    }
    if (include_perseverance) {
      perseverancetheta = 0.5; /* By default */
      if (prev_choice == 0){
        perseverancetheta = 0;
      } else if (prev_choice == 1){
        perseverancetheta = 1;
      }
      theta_mixed += myomega[4] * perseverancetheta;
    }
    return theta_mixed;
  }
}

data {
  int<lower=0> T; // Number of trial types (unique lottery, surebet combinations) we have
  int<lower=0> K; // number of subjects
  int individual[T]; // vector of subjid indexes
  vector[T] lottery_mag; // Lottery value for that choice
  vector[T] lottery_prob; // Lottery probabilities for that choice
  vector[T] sb_mag; // Surebet values for that choice
  vector[T] total_rew_multi; // total reward multiplier = base_reward * rew_multi
  vector[T] prev_outcome; // -1 = lose, 1 = win, 0 = surebet
  vector[T] prev_choice; // Previous choice. 0 = surebet, 1 = lottery, -1 = "other" (violation/first trial)
  int n_chose_lott[T]; // number chose lottery for this trial type
  int n_trials[T]; // total number of trials for this trial type
  /* Hyperparameters */
  real<lower=0> rho_mu;
  real<lower=0> rho_raw_sd;
  real<lower=0> sigma_mu;
  real<lower=0> sigma_sigma;
  real<lower=0> omega_alpha_rational;
  real<lower=0> omega_alpha_lottery;
  real<lower=0> omega_alpha_surebet;
  real<lower=0> omega_alpha_perseverance;
  /* Toggle */
  int<lower=0,upper=1> include_kappa; // Boolean.
  int<lower=0,upper=1> include_rho; // Boolean.
  int<lower=0,upper=1> include_scalar; // Boolean.
  /* Whether omega should be a 3 or 4 simplex (otherwise it's a "1-simplex" [which is just 1.]) */
  int<lower=0,upper=1> include_mix;
  int<lower=0,upper=1> include_perseverance;
  int<lower=0,upper=1> include_history; // Whether or not to use omega_win/omega_lose. If not they'll be fixed.
}
transformed data {
  real rho_raw_mu;
  real sigma_alpha;
  int n_agents;
  
  n_agents = 1;
  if(include_mix)
  n_agents = include_perseverance ? 4 : 3;
  rho_raw_mu = log(rho_mu);
  sigma_alpha = log(sigma_mu);
}
parameters {
  // species level
  vector[include_rho] rho_raw;
  vector<offset=0, multiplier=0.01>[include_kappa] kappa_raw;
  real<lower=0> sigma;
  simplex[n_agents] omega_alpha;
  simplex[include_history ? n_agents : 1] omega_win_alpha;
  simplex[include_history ? n_agents : 1] omega_lose_alpha;
  
  // individual level
  vector[K] rho_i_raw;
  vector[K] kappa_i_raw;
  vector<lower=0>[K] sigma_i;
  simplex[n_agents] omega_i[K];
  simplex[include_history ? n_agents : 1] omega_win_i[K];
  simplex[include_history ? n_agents : 1] omega_lose_i[K];
}
transformed parameters {
  real<lower=0> rho;
  real kappa;
  vector<lower=0>[K] rho_i;
  vector[K] kappa_i;
  
  rho = include_rho ? exp(rho_raw[1]) : 1;
  kappa = include_kappa ? kappa_raw[1] : 0;
  rho_i = include_rho ? exp(rho_i_raw) : rep_vector(1, K); 
  kappa_i = include_kappa ? kappa_i_raw : rep_vector(0, K);
}
model {
  if (include_mix) {
    omega_alpha[1] ~ normal(omega_alpha_rational, 1); /* draw omega_alpha from the Dirichlet hyperpriors */
    omega_alpha[2] ~ normal(omega_alpha_lottery, 1);
    omega_alpha[3] ~ normal(omega_alpha_surebet, 1);
    for (k in 1:K){
      omega_i[k] ~ dirichlet(omega_alpha);
    }
  }
  if (include_history){
    omega_win_alpha[1] ~ normal(omega_alpha_rational, 1);
    omega_win_alpha[2] ~ normal(omega_alpha_lottery, 1);
    omega_win_alpha[3] ~ normal(omega_alpha_surebet, 1);
    omega_lose_alpha[1] ~ normal(omega_alpha_rational, 1);
    omega_lose_alpha[2] ~ normal(omega_alpha_lottery, 1);
    omega_lose_alpha[3] ~ normal(omega_alpha_surebet, 1);
    for (k in 1:K){
      omega_win_i[k] ~ dirichlet(omega_win_alpha);
      omega_lose_i[k] ~ dirichlet(omega_lose_alpha);
    }
  }
  if (include_perseverance){
    omega_alpha[4] ~ normal(omega_alpha_perseverance, 1);
  }
  if (include_rho){
    rho_raw[1] ~ normal(rho_raw_mu, rho_raw_sd);
    for (k in 1:K){
      rho_i_raw[k] ~ normal(rho_raw, 0.5); // these are to be estimated
    }
  }
  if (include_kappa){
    kappa_raw[1] ~ normal(0, 1);
    for (k in 1:K){
      kappa_i[k] ~ normal(kappa, 1);
    }
  }
  sigma ~ lognormal(sigma_alpha, sigma_sigma);
  for (k in 1:K){
    sigma_i[k] ~ normal(sigma, 0.5);
  }
  
  for (t in 1:T){
    n_chose_lott ~ binomial(n_trials, mixprob(choiceprob(rho_i[individual[t]], kappa_i[individual[t]], sigma_i[individual[t]], include_scalar, 
                                                         lottery_mag[t], total_rew_multi[t], lottery_prob[t], sb_mag[t]),
                                              omega_i[individual[t]],
                                              (include_history ? omega_win_i[individual[t]] : omega_i[individual[t]]),
                                              (include_history ? omega_lose_i[individual[t]]: omega_i[individual[t]]),
                                              prev_outcome[t], prev_choice[t]));
  }
}
