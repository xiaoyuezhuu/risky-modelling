functions {
  vector choiceprob(real rho, real kappa, real sigma, int include_scalar, vector v_l, vector rew_multi, vector p_l, vector v_s) {
    vector[rows(v_l)] theta_l;
    real u_l;	/* Lottery utility */
    real u_s;	/* Surebet utility */
    real noise_term; /* Independent noise or scalar noise */
    real stddev_l; /* standard deviation of lottery offer */
    
    for (i in 1:rows(v_l)){
      stddev_l = sqrt( p_l[i] * (v_l[i]*rew_multi[i])^(2*rho) * (1.0 - p_l[i]));
      u_l = (p_l[i] * ((v_l[i]*rew_multi[i]) ^ rho)) - (kappa * stddev_l);
      u_s = (1.0 * ((v_s[i]*rew_multi[i]) ^ rho)) - (kappa * 0.0);
      /*
      Normal distributions add variance.
      First three lines should be equivalent.
      */
      if (include_scalar) {
        noise_term = sqrt((u_l * sigma)^2 + (u_s * sigma)^2);
      } else{ 
        noise_term = sqrt(2) * sigma;
      }
      theta_l[i] = normal_cdf(0, u_s - u_l, noise_term);
    }
    return theta_l;
  }
	vector mixprob(vector theta_rational,
		       vector omega, vector omega_win, vector omega_lose,
		       vector prev_outcome, vector prev_choice) {
		int M = rows(theta_rational);
		int J = rows(omega);
		int include_perseverance = (J == 4);
		int include_mix = (J >= 3);
		vector[M] theta_mixed;
		vector[J] myomega;
		real perseverancetheta;
		for (m in 1:M) {
			myomega = omega;
			if (prev_outcome[m] == 1)
				myomega = omega_win;
			else if(prev_outcome[m] == -1)
				myomega = omega_lose;
			if(include_mix){
			  theta_mixed[m] = (myomega[1] * theta_rational[m]) + (myomega[2] * 1) + (myomega[3] * 0);
			} else{
			  theta_mixed[m] = myomega[1] * theta_rational[m];
			}
			if(include_perseverance) {
				perseverancetheta = 0.5; /* By default */
				if (prev_choice[m] == 0)
					perseverancetheta = 0;
				else if (prev_choice[m] == 1)
					perseverancetheta = 1;
				theta_mixed[m] += myomega[4] * perseverancetheta;
			}
		}
		return theta_mixed;
	}
}
data {
	int<lower=0> T; // Number of trial types (unique lottery, surebet combinations) we have
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
	int n_agents;
	
	n_agents = 1;
	if(include_mix)
		n_agents = include_perseverance ? 4 : 3;
	rho_raw_mu = log(rho_mu);
}
parameters {
	vector[include_rho] rho_raw;
	vector<offset=0, multiplier=0.01>[include_kappa] kappa_raw;
	real<offset=8> sigma; /* noise multiplicative scaler */
	simplex[n_agents] omega;
	simplex[include_history ? n_agents : 1] omega_win;
	simplex[include_history ? n_agents : 1] omega_lose;
}
transformed parameters {
	real<lower=0> rho;
	real kappa;
	rho = include_rho ? exp(rho_raw[1]) : 1;
	kappa = include_kappa ? kappa_raw[1] : 0;
}
model {
	vector[n_agents] omega_alpha;
	omega_alpha = rep_vector(1, n_agents);
	if(include_mix) {
		omega_alpha[1] = omega_alpha_rational;
		omega_alpha[2] = omega_alpha_lottery;
		omega_alpha[3] = omega_alpha_surebet;
	}
	if(include_perseverance)
		omega_alpha[4] = omega_alpha_perseverance;
	if(include_kappa)
		kappa_raw[1] ~ normal(0, 1);
	if(include_rho)
		rho_raw[1] ~ normal(rho_raw_mu, rho_raw_sd);
	
	sigma ~ gamma(6, 3);
	omega ~ dirichlet(omega_alpha);
	/* TODO: Measure distance between omega and omega_win. Prefer being closer together. */
	if(include_history) {
		omega_win ~ dirichlet(omega_alpha);
		omega_lose ~ dirichlet(omega_alpha);
	}
	if (n_agents == 1){
	  n_chose_lott ~ binomial(n_trials, choiceprob(rho, kappa, sigma, include_scalar, lottery_mag, total_rew_multi, lottery_prob, sb_mag)); 
	} else{
	  n_chose_lott ~ binomial(n_trials, mixprob(choiceprob(rho, kappa, sigma, include_scalar,
							                                          lottery_mag, total_rew_multi, lottery_prob, sb_mag),
					       omega,
					       (include_history ? omega_win : omega),
					       (include_history ? omega_lose : omega),
					       prev_outcome, prev_choice));
	}
}
