# fitting hyperparameters
SEED = 12345

# plotting colors
BASE_SIZE = 16
ANNOTATION_SIZE = 5
ANIMAL_COLORS = c("azure4", 'chocolate4')
SPECIES_COLORS = list('Human' = 'lightpink3', 
                      'Mouse' = "azure4", 
                      'Rat' = 'chocolate4', 
                      'human' = 'lightpink3', 
                      'nonverbal' = 'lightpink3', 
                      'non-verbal' = 'lightpink3', 
                      'mouse' = "azure4", 
                      'rat' = 'chocolate4')
PREV_OUTCOME_COLORS = c('#E43D40', '#41729F', '#76B947') # lose, surebet, win


# parameter names
latex_params = list(rho = "$\\rho$", kappa = '$\\kappa$', sigma = "$\\sigma$",
                    omega = '$\\omega$', omega_win = '$\\omega_{win}$', omega_lose = '$\\omega_{lose}$',
                    omega.1 = '$\\omega_{rational}$', omega.2 = '$\\omega_{lottery}$', omega.3 = '$\\omega_{surebet}$',
                    omega_win.1 = '$\\omega^{win}_{rational}$', omega_win.2 = '$\\omega^{win}_{lottery}$', omega_win.3 = '$\\omega^{win}_{surebet}$', 
                    omega_lose.1 = '$\\omega^{lose}_{rational}$', omega_lose.2 = '$\\omega^{lose}_{lottery}$', omega_lose.3 = '$\\omega^{lose}_{surebet}$')

# parameter breaks
lower_limits = list('rho' = 0, 'kappa' = -1, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0, 
                    'omega.1' = 0, 'omega.2' = 0, 'omega.3' = 0, 
                    'omega_win.1' = 0, 'omega_win.2' = 0, 'omega_win.3' = 0, 
                    'omega_lose.1' = 0, 'omega_lose.2' = 0, 'omega_lose.3' = 0)
upper_limits = list('rho' = 2, 'kappa' = 1, 'sigma' = 10, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1,
                    'omega.1' = 1, 'omega.2' = 1, 'omega.3' = 1, 
                    'omega_win.1' = 1, 'omega_win.2' = 1, 'omega_win.3' = 1, 
                    'omega_lose.1' = 1, 'omega_lose.2' = 1, 'omega_lose.3' = 1)
lower_breaks = list('rho' = 0, 'kappa' = -1, 'sigma' = 0, 'omega_rational' = 0, 'omega_lottery' = 0, 'omega_surebet' = 0,
                    'omega.1' = 0, 'omega.2' = 0, 'omega.3' = 0, 
                    'omega_win.1' = 0, 'omega_win.2' = 0, 'omega_win.3' = 0, 
                    'omega_lose.1' = 0, 'omega_lose.2' = 0, 'omega_lose.3' = 0)
middle_breaks = list('rho' = 1, 'kappa' = 0, 'sigma' = 5, 'omega_rational' = 0.5, 'omega_lottery' = 0.5, 'omega_surebet' = 0.5,
                     'omega.1' = 0.5, 'omega.2' = 0.5, 'omega.3' = 0.5, 
                     'omega_win.1' = 0.5, 'omega_win.2' = 0.5, 'omega_win.3' = 0.5, 
                     'omega_lose.1' = 0.5, 'omega_lose.2' = 0.5, 'omega_lose.3' = 0.5)
upper_breaks = list('rho' = 2, 'kappa' = 1, 'sigma' = 10, 'omega_rational' = 1, 'omega_lottery' = 1, 'omega_surebet' = 1,
                    'omega.1' = 1, 'omega.2' = 1, 'omega.3' = 1, 
                    'omega_win.1' = 1, 'omega_win.2' = 1, 'omega_win.3' = 1, 
                    'omega_lose.1' = 1, 'omega_lose.2' = 1, 'omega_lose.3' = 1)

# use hue to represent levels, lumiosity represent base_model
# base_models = yellow
# scalar = blue
# mix = red
# mix + scalar = purple
# history = green
# constant = 
model_colors = list('rho-sigma' = '#D6AD60','kappa-sigma' = '#FAD02C', 'rho-kappa-sigma' = '#FF9636',  # yellow
                    'rho-scalar' = '#B7CFDC', 'kappa-scalar' = '#6AABD2', 'rho-kappa-scalar' = '#385E72', # blue
                    'rho-scalar-constant' = '#ffb319', 'kappa-scalar-constant' = '#e1aa00', 'rho-kappa-scalar-constant' = '#dfb970', # yellow-ish
                    'mix-rho-sigma' = '#FFA3B8', 'mix-kappa-sigma' = '#D773A2', 'mix-rho-kappa-sigma'= '#821D30', # red
                    'mix-rho-scalar' = '#D3BBDD', 'mix-kappa-scalar' = '#A16AE8', 'mix-rho-kappa-scalar'= '#4120A9', # purple
                    'mix-rho-scalar-constant' = '#fac18e', 'mix-kappa-scalar-constant' = '#f58a28', 'mix-rho-kappa-scalar-constant'= '#663304', # orange
                    'history-mix-rho-sigma' = '#D1E2C4', 'history-mix-kappa-sigma' = '#81B622', 'history-mix-rho-kappa-sigma'= '#59981A', # green
                    'history-mix-rho-scalar' = '#31352E', 'history-mix-kappa-scalar' = '#4D4C60', 'history-mix-rho-kappa-scalar'= '#010100') # black

# toggle model hyperparameters
hyperparams = list(rho_mu = 0.9, rho_raw_sd = 0.4, 
                   sigma_mu = 6, sigma_sigma = 0.3,
                   omega_alpha_rational = 6, omega_alpha_lottery = 2, omega_alpha_surebet = 2, omega_alpha_perseverance = 2)

# load model
#toggle_model = stan_model(file = 'figures/all_models_toggle.stan') # just model fitting
#toggle_cv_model = stan_model(file = 'figures/all_models_toggle_cv.stan') # with cross-validation
#toggle_pred_model = stan_model(file = 'figures/all_models_toggle_pred.stan') # with out-of-sample prediction

get_params = function(model = 'rho-beta'){
  # get species-level parameters based on model
  params_list = vector()
  if (str_detect(model, 'rho')){
    params_list = append(params_list, 'rho')
  }
  if (str_detect(model, 'kappa')){
    params_list = append(params_list, 'kappa')
  }
  if (str_detect(model, 'beta')){
    params_list = append(params_list, 'beta')
  }
  if (str_detect(model, 'sigma') | str_detect(model, 'scalar')){
    params_list = append(params_list, 'sigma')
  }
  if (str_detect(model, 'mix')){
    params_list = append(params_list, c('omega.1', 'omega.2', 'omega.3'))
  }
  if (str_detect(model, 'history')){
    params_list = append(params_list, c('omega_win.1', 'omega_win.2', 'omega_win.3'))
    params_list = append(params_list, c('omega_lose.1', 'omega_lose.2', 'omega_lose.3'))
  }
  if (str_detect(model, 'wsls')){
    params_list = append(params_list, c('theta_win', 'theta_lose', 'theta_draw'))
  }
  if (str_detect(model, 'bias')){
    params_list = append(params_list, 'theta')
  }
  return(params_list)
}

bino = function(x){
  out = binom.test(sum(x), length(x))
  df = data.frame(y = mean(x), ymin = out$conf.int[1], ymax = out$conf.int[2])
  return(df)
}

se = function(x){
  return(sd(x) / sqrt(length(x)))
}

scale_save = function(p, name, width = 4, height = 4, scale = 0.7){
  # ggsave wrapper
  p = p + theme(text = element_text(size = BASE_SIZE), 
                axis.text = element_text(size = BASE_SIZE), 
                legend.text = element_text(size = BASE_SIZE))
  fname = sprintf('figures/plots/%s.pdf', name)
  ggsave(filename = fname, device = "pdf", width = width, height = height, scale = scale, units = 'cm', dpi = 72)
}

binomialize = function(df, individual = TRUE, history = TRUE){
  # format df into binomial version 
  if (individual & history){ # individual with history
    bino_df = df %>% group_by(prev_outcome, prev_choice, lottery_mag, lottery_prob, sb_mag, total_rew_multi)
  } else if (individual & !history){ # individual with no history
    bino_df = df %>% group_by(lottery_mag, lottery_prob, sb_mag, total_rew_multi)
  } else if (!individual & history){ # species with history
    bino_df = df %>% group_by(subjid, prev_outcome, prev_choice, lottery_mag, lottery_prob, sb_mag, total_rew_multi)
  } else if (!individual & !history){ # species with no history
    bino_df = df %>% group_by(subjid, lottery_mag, lottery_prob, sb_mag, total_rew_multi)
  }
  bino_df = bino_df %>% 
    add_tally() %>% summarise(n_trials = mean(n), n_chose_lott = sum(choice)) %>% 
    mutate(delta_ev = lottery_mag*lottery_prob*total_rew_multi - sb_mag*total_rew_multi) %>% 
    ungroup()
  #bino_df$individual = bino_df %>% group_indices(subjid)
  return(bino_df)
}

computeELPD = function(log_lik_heldout){
  # adapted from https://github.com/stan-dev/stancon_talks/blob/master/2017/Contributed-Talks/07_nicenboim/kfold.Rmd
  # The ELPD is the theoretical expected log pointwise predictive density for a new dataset
  # the loglik from the fit is a n_iter x T matrix, the pointwise is 1 x T vector, elpd is the sum of pointwise
  library(matrixStats)
  logColMeansExp = function(x) {
    S = nrow(x)
    colLogSumExps(x) - log(S)
  }
  pointwise = matrix(logColMeansExp(log_lik_heldout), ncol = 1)
  colnames(pointwise) = "elpd"
  elpd = sum(pointwise)
  se_elpd = sqrt(ncol(log_lik_heldout) * var(pointwise))
  out = list('pointwise' = pointwise,
             'elpd' = elpd,
             'se_elpd' = se_elpd)
  return(structure(out, class = 'S4'))
}

plot_risk = function(df, by_prev_outcome = FALSE){
  # This function plots binned risk choices with specified subjid
  m1 = glm('choice ~ delta_ev', df, family = 'binomial')
  df$pred = predict(m1, type = 'response')
  n_trials = dim(df)[1]
  p = ggplot(df) + theme_classic() +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) + 
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    xlab(expression(Delta~EV)) + ylab("P(Chose Lottery)")
  if (!by_prev_outcome){
    p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, geom = 'pointrange') +
      geom_line(mapping = aes(x = delta_ev, y = pred))
  } else {
    p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(prev_outcome)), fun.data = bino, geom = 'pointrange') +
      geom_line(mapping = aes(x = delta_ev, y = pred, color = as.factor(prev_outcome)))
  }
  return(p)
}

get_freq_task_param = function(df, param = 'sb_mag'){
  # find the most frequent task parameter given the real dataset df
  return(as.numeric(names(sort(table(df[param]), decreasing = TRUE)[1])))
}

gen_syn_smooth = function(df, history = TRUE){
  # generate smooth synthetic data points for every task parameter combinations 
  if (history){
    pred_df = df %>% group_by(subjid) %>% 
      data_grid(delta_ev = seq_range(delta_ev, by = 0.5), 
                prev_outcome = prev_outcome, prev_choice = prev_choice,
                lottery_prob = get_freq_task_param(df, 'lottery_prob'), 
                sb_mag = get_freq_task_param(df, 'sb_mag'), 
                total_rew_multi = get_freq_task_param(df, 'total_rew_multi'),
                n_trials = 200) %>% 
      mutate(lottery_mag = (delta_ev + sb_mag*total_rew_multi) / (lottery_prob*total_rew_multi))
  } else{
    pred_df = df %>% group_by(subjid) %>% 
      data_grid(delta_ev = seq_range(delta_ev, by = 0.5), 
                lottery_prob = get_freq_task_param(df, 'lottery_prob'), 
                sb_mag = get_freq_task_param(df, 'sb_mag'), 
                total_rew_multi = get_freq_task_param(df, 'total_rew_multi'),
                n_trials = 200) %>% 
      mutate(lottery_mag = (delta_ev + sb_mag*total_rew_multi) / (lottery_prob*total_rew_multi))
  }
  pred_df = pred_df %>% filter(lottery_mag >= 0) %>% ungroup()
  colnames(pred_df) = paste0('pred_', colnames(pred_df))
  return(pred_df)
}

get_kfold_models = function(){
  models_list = c('rho-sigma', 'kappa-sigma', 'rho-kappa-sigma', # base models
                  'rho-scalar', 'kappa-scalar', 'rho-kappa-scalar', # scalar models, kappa has problems!
                  'mix-rho-sigma', 'mix-kappa-sigma', 'mix-rho-kappa-sigma', # base models + mixture
                  'mix-rho-scalar', 'mix-kappa-scalar', 'mix-rho-kappa-scalar', # scalar models + mixture
                  'history-mix-rho-sigma', 'history-mix-kappa-sigma', 'history-mix-rho-kappa-sigma', # base models + mixture + history
                  'history-mix-rho-scalar', 'history-mix-kappa-scalar', 'history-mix-rho-kappa-scalar' # base models + mixture + history
                  
  )
  return(models_list)
}

concat_species_HBM = function(model = 'rho-sigma'){
  # concatenate HBM results of each species into one csv
}

debug_constant = function(){
  kappa = -0.02
  rho = 1.21
  sigma = 0.50
  c = 1.01
  lott_mag = c(0, 2, 4, 8, 16, 32) * 8
  sb_mag = 3 * 8
  df1 = data.frame()
  df2 = data.frame()
  for (i in 1:length(lott_mag)){
    stddev_l = sqrt( 0.5 * (lott_mag[i])^(2*rho) * (1 - 0.5) )
    u_l = 0.5 * lott_mag[i] ^ rho - (kappa * stddev_l)
    u_s = sb_mag ^ rho
    noise_term = sqrt( (u_l * sigma + c)^2 + (u_s * sigma + c)^2 )
    p_rational = pnorm(0, u_s - u_l, noise_term);
    this_df1 = data.frame(p_rational = p_rational, lott_mag = lott_mag[i])
    df1 = rbind(df1, this_df1)
    this_df2 = data.frame(eu_dist = rnorm(1000, u_l, u_l * sigma + c), lott_mag = lott_mag[i])
    df2 = rbind(df2, this_df2)
  }
  df1$lott_mag = as.factor(df1$lott_mag)
  df2$lott_mag = as.factor(df2$lott_mag)
  p1 = ggplot(df1, aes(x = lott_mag, y = p_rational, color = lott_mag)) + theme_classic() +
    geom_point(size = 2) + theme(legend.position = 'none') +
    ggtitle(sprintf('rho = %.1f, kappa = %.1f, sigma = %.1f, c = %.1f', rho, kappa, sigma, c)) +
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1))
  p2 = ggplot(df2, aes(x = eu_dist, fill = lott_mag)) + theme_classic() +
    geom_density(alpha = 0.4) + theme(legend.position = 'bottom')
  #scale_x_continuous(limits = c(0, 1000))
  return(p1 / p2)
}

fit_all = function(df){
  #df = read.csv('figures/csv/preprocessed_all.csv')
  models = get_kfold_models()[7:18]
  subj_list = unique(df$subjid)
  for (model in models){
    draws_df = data.frame()
    for (subj in subj_list){
      fit = fit_toggle(model, df %>% filter(subjid == subj))
      draws_df = rbind(draws_df, as.data.frame(rstan::extract(fit)) %>% mutate(subjid = subj, model = model) %>% sample_n(100))
    }
    write.csv(draws_df, sprintf('figures/csv/fits/%s_fits.csv', model))
  }
}

fit_toggle = function(model, subj_df){
  # fit data using the toggle model
  bino_df = binomialize(subj_df, individual = TRUE, history = TRUE)
  data = as.list(bino_df)
  data$T = dim(bino_df)[1]
  data = append(data, hyperparams)
  # toggle
  data$include_rho = str_detect(model, 'rho')
  data$include_kappa = str_detect(model, 'kappa')
  data$include_scalar = str_detect(model, 'scalar')
  data$include_constant = str_detect(model, 'constant')
  data$include_mix = str_detect(model, 'mix')
  data$include_perseverance = str_detect(model, 'perseverance')
  data$include_history = str_detect(model, 'history')
  
  cat(sprintf("Fitting %s using %s ... \n", unique(subj_df$subjid), model))
  fit = sampling(toggle_model, data = data, seed = SEED, refresh = 0, warmup = 500, iter = 1000, init = 'random')
  return(fit)
}

fit_all_pred = function(model = 'rho-sigma'){
  df = read.csv('figures/csv/preprocessed_all.csv')
  subj_list = unique(df$subjid)
  pred_df = data.frame()
  for (subj in subj_list){
    subj_df = df %>% filter(subjid == subj)
    fit = fit_toggle_pred(model, subj_df)
    draws = rstan::extract(fit)
    ncl_df = as.data.frame(t(draws$pred_n_chose_lott))
    this_pred_df = gen_syn_smooth(subj_df, history = TRUE) %>% mutate(y = rowMeans(ncl_df)/pred_n_trials, 
                                                                           ymin_80 = apply(ncl_df, 1, quantile, 0.1)/pred_n_trials,  
                                                                           ymax_80 = apply(ncl_df, 1, quantile, 0.9)/pred_n_trials,
                                                                           ymin_90 = apply(ncl_df, 1, quantile, 0.05)/pred_n_trials,  
                                                                           ymax_90 = apply(ncl_df, 1, quantile, 0.95)/pred_n_trials,
                                                                           ymin_95 = apply(ncl_df, 1, quantile, 0.025)/pred_n_trials,  
                                                                           ymax_95 = apply(ncl_df, 1, quantile, 0.975)/pred_n_trials,
                                                                           ymin_99 = apply(ncl_df, 1, quantile, 0.005)/pred_n_trials,  
                                                                           ymax_99 = apply(ncl_df, 1, quantile, 0.995)/pred_n_trials,
                                                                           subjid = subj, model = model)
    pred_df = rbind(pred_df, this_pred_df)
  }
  write.csv(pred_df, sprintf('figures/csv/pred/%s_pred.csv', model))
}

fit_toggle_pred = function(model, subj_df){
  # fit data using the toggle_pred model
  bino_df = binomialize(subj_df, individual = TRUE, history = TRUE)
  pred_df = gen_syn_smooth(subj_df, history = TRUE)
  data = append(as.list(bino_df), as.list(pred_df))
  data$T = dim(bino_df)[1]
  data$P = dim(pred_df)[1]
  data = append(data, hyperparams)
  # toggle
  data$include_kappa = str_detect(model, 'kappa')
  data$include_rho = str_detect(model, 'rho')
  data$include_scalar = str_detect(model, 'scalar')
  data$include_mix = str_detect(model, 'mix')
  data$include_perseverance = str_detect(model, 'perseverance')
  data$include_history = str_detect(model, 'history')
  
  cat(sprintf("Fitting %s using %s ... \n", unique(subj_df$subjid), model))
  fit = sampling(toggle_pred_model, data = data, seed = SEED, refresh = 1, 
                 warmup = 200, iter = 400, init = 'random')
  return(fit)
}

kfold_toggle = function(K_FOLD = 10, model, save = FALSE, save_path = 'fits/'){
  # the generic function for manual K-fold cross validation  
  df = read.csv('figures/csv/preprocessed_all.csv')
  subj_list = unique(df$subjid)
  fit_model = stan_model(file = 'figures/all_models_toggle.stan')
  # first find the portion size for each subject
  portion_df = df %>% group_by(subjid) %>% tally() %>% 
    mutate(portion = round(n/K_FOLD), test_begin = 1, test_end = test_begin + portion)
  # initialize elpd container for this model's kfold
  elpd_df = data.frame()
  for (k in 1:K_FOLD){
    df$holdout = 0 # 1 means to be heldout, 0 means to be used for fitting
    for (sx in 1:length(subj_list)){
      subj = subj_list[sx]
      test_begin = portion_df$test_begin[portion_df$subjid == subj]
      test_end = portion_df$test_end[portion_df$subjid == subj]
      df$holdout[df$subjid == subj][test_begin:test_end] = 1
      # update test_begin and test_end number for the next fold
      portion_df$test_begin[portion_df$subjid == subj] = test_end
      if (k == K_FOLD - 1){
        portion_df$test_end[portion_df$subjid == subj] = portion_df$n[portion_df$subjid == subj]
      } else {
        portion_df$test_end[portion_df$subjid == subj] = test_end + portion_df$portion[portion_df$subjid == subj]
      }
      # format it into binomial version
      all_df = df %>% filter(subjid == subj) %>% group_by(holdout, prev_outcome, prev_choice, lottery_mag, lottery_prob, sb_mag, total_rew_multi) %>% 
        add_tally() %>% summarise(n_trials = mean(n), n_chose_lott = sum(choice))
      train_df = all_df[all_df$holdout == 0, ]
      test_df = all_df[all_df$holdout == 1, ]
      # put data into a list
      train_data = as.list(train_df)
      colnames(test_df) = paste0('test_', colnames(test_df))
      test_data = as.list(test_df)
      data = append(train_data, test_data)
      data$T = dim(train_df)[1]
      data$D = dim(test_df)[1]
      data = append(data, hyperparams)
      # toggle
      data$include_rho = str_detect(model, 'rho')
      data$include_kappa = str_detect(model, 'kappa')
      data$include_scalar = str_detect(model, 'scalar')
      data$include_mix = str_detect(model, 'mix')
      data$include_perseverance = str_detect(model, 'perseverance')
      data$include_history = str_detect(model, 'history')
      # fit the model
      cat(sprintf('Fitting %d/%d fold on %d using %s...\n', k, K_FOLD, subj, model))
      fit = sampling(fit_model, data = data, seed = SEED, refresh = 0, init = 'random')
      if (save == TRUE){
        save(fit, file = sprintf('fits/cross-validation/%s/%d-%d_fits.RData', model, subj, k))
      }
      check_hmc_diagnostics(fit)
      # store elpd from this fold
      elpd_out = computeELPD(extract_log_lik(fit))
      temp_df = data.frame(subjid = subj, elpd = as.numeric(elpd_out$elpd), 
                           se_elpd = as.numeric(elpd_out$se_elpd), k_fold = k)
      elpd_df = rbind(elpd_df, temp_df)
    }
  }
  write.csv(elpd_df, sprintf("%s_kfold.csv", model))
  return(elpd_df)
}

risk_sim = function(params, 
                    model = 'rho-sigma', 
                    sb_mags = c(3,3), 
                    lottery_mags = c(0, 4, 8, 16, 24, 32),
                    lottery_prob = 0.5, 
                    total_rew_multi = 8, 
                    n_trials = 1000, n_sessions = 10, 
                    subj_df = NULL, prob = FALSE){
  # This function simulates synthetic data given the settings 
  # if subj_df is not NULL, it uses the real task parameters from subj_df
  if (!is.null(subj_df)){
    n_trials = dim(subj_df)[1]
    df = subj_df %>% select(c(lottery_mag, lottery_prob, lottery_outcome, sb_mag, total_rew_multi))
    df$prev_outcome = rep(9, n_trials)
  } else{
    df = data.frame(lottery_mag = sample(lottery_mags, n_trials, replace = TRUE),
                    sb_mag = sample(sb_mags, n_trials, replace = TRUE),
                    lottery_prob = lottery_prob,
                    lottery_outcome = rbinom(n_trials, 1, lottery_prob), 
                    total_rew_multi = total_rew_multi,
                    prev_outcome = 9, choice_prob = 9, choice = 9)
  }
  df$prev_outcome[1] = 0
  for (tx in 1:(n_trials-1)){
    df$choice_prob[tx] = toggle_agent(model, params, df$sb_mag[tx], df$lottery_mag[tx], df$lottery_prob[tx], df$total_rew_multi[tx], df$prev_outcome[tx])
    df$choice[tx] = rbinom(1, 1, df$choice_prob[tx])
    if (df$choice[tx] == 0){ # chose surebet
      df$prev_outcome[tx+1] = 0
    } else if (df$choice[tx] == 1 & df$lottery_outcome[tx] == 1){ # lottery-win
      df$prev_outcome[tx+1] = 1
    } else{ # lottery lose
      df$prev_outcome[tx+1] = -1
    }
  }
  df$choice_prob[n_trials] = toggle_agent(model, params, df$sb_mag[n_trials], df$lottery_mag[n_trials], df$lottery_prob[n_trials], df$total_rew_multi[n_trials], df$prev_outcome[n_trials])
  df = df %>% mutate(trialnum = seq(1, n_trials),
                     delta_ev = (total_rew_multi * lottery_mag * lottery_prob) - total_rew_multi * sb_mag) %>% 
    mutate(prev_outcome_s = case_when(prev_outcome == 1 ~ 'lottery_win',
                                      prev_outcome == -1 ~ 'lottery_lose',
                                      prev_outcome == 0 ~ 'surebet'))
  df$prev_outcome_s = factor(df$prev_outcome_s, levels = c('lottery_win', 'surebet', 'lottery_lose'))
  return(df)
}

toggle_agent = function(model, params, sb_mag, lottery_mag, lottery_prob, total_rew_multi, prev_outcome = NULL, probs = TRUE){
  # default parameter values
  rho = 1
  kappa = 0
  sigma = params$sigma
  noise_term = sqrt(2) * sigma
  # init output vectors
  n_trials = length(sb_mag)
  choice_prob = vector(length = n_trials)
  choice = vector(length = n_trials)
  # model toggle control
  if (str_detect(model, 'rho')){
    rho = params$rho
  }
  if (str_detect(model, 'kappa')){
    kappa = params$kappa
  }
  if (str_detect(model, 'mix')){
    omega = c(params$omega.1, params$omega.2, params$omega.3)
    omega_win = omega
    omega_lose = omega
  }
  if (str_detect(model, 'history')){
    omega_win = c(params$omega_win.1, params$omega_win.2, params$omega_win.3)
    omega_lose = c(params$omega_lose.1, params$omega_lose.2, params$omega_lose.3)
  }
  for (tx in 1:n_trials){
    lott_variance = lottery_prob[tx] * (1 - lottery_prob[tx]) * (total_rew_multi[tx] * lottery_mag[tx]) ^ (2*rho)
    if (str_detect(model, 'scalar')){
      noise_term = sqrt( ((lottery_mag[tx] * total_rew_multi[tx])^2 * sigma^2 + (sb_mag[tx]*total_rew_multi[tx])^2 * sigma^2) )
    }
    p_rational = 1 - pnorm(0, lottery_prob[tx] * (total_rew_multi[tx]*lottery_mag[tx])^rho - kappa * sqrt(lott_variance) - (total_rew_multi[tx]*sb_mag[tx])^rho, noise_term)
    P = c(p_rational, 1, 0)
    if (str_detect(model, 'mix')){
      if (prev_outcome[tx] == 0){ # surebet
        choice_prob[tx] = omega %*% P
      } else if (prev_outcome[tx] == 1){ # lottery-win
        choice_prob[tx] = omega_win %*% P
      } else if (prev_outcome[tx] == -1){ # lottery-lose
        choice_prob[tx] = omega_lose %*% P
      }
    } else{
      choice_prob[tx] = p_rational
    }
    choice[tx] = rbinom(1, 1, choice_prob[tx])
  }
  if (!probs){
    return(choice)
  } else{
    return(choice_prob)
  }
}

gen_syn_from_fitted_params = function(model){
  # generate synthetic data using fitted model parameters and actual task parameters
  # for Confusion matrix
  params_list = get_params(model)
  df = read.csv('figures/csv/preprocessed_all.csv')
  subj_list = unique(df$subjid)
  # generate synthetic data using fitted parameters from this model
  syn_df = data.frame()
  for (subj in subj_list){
    # fit model to this subject and get the fitted parameters
    subj_df = df %>% filter(subjid == subj)
    fit = fit_toggle(model, subj_df)
    draws_df = as.data.frame(rstan::extract(fit))
    fitted_params = list()
    for (param in params_list){
      fitted_params[param] = draws_df[which.max(draws_df$lp__), param]
    }
    # simulate synthetic data using fitted parameters
    this_syn_df = risk_sim(params = fitted_params, model = model, subj_df = subj_df)
    this_syn_df = this_syn_df %>% mutate(subjid = subj)
    syn_df = rbind(syn_df, this_syn_df)
  }
  # save syn file
  write.csv(syn_df, sprintf('figures/csv/%s_syn.csv', model))
}

change_file_names = function(x = 'scalar', y = 'scalar'){
  # change x into y
  dirs = list.dirs('figures/csv/')
  for (dir in dirs){
    files = list.files(dir)
    for (file in files){
      if (str_detect(file, x)){
        old_name = sprintf('%s/%s', dir, file)
        new_name = gsub(x, y, old_name)
        file.rename(old_name, new_name)
        cat(sprintf('Renamed %s to %s \n', old_name, new_name))
      }
    }
  }
}

permutation.test = function(d1, d2, n = 1000){
  # sample from distribution 1 and distribution 2, take the difference of the samples, what proportion of it is bigger / smaller than 0?
  diff_dist = sample(d1, n, replace = TRUE) - sample(d2, n, replace = TRUE)
  p1 = sum(diff_dist >= 0) / (n+1)
  p2 = sum(diff_dist <= 0) / (n+1)
  return(min(p1, p2))
}