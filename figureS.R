# Functions used to generate supplementary figures

sim_nonlinear = function(kappa = 0.3, sigma = 1){
  lott_mags = c(0, 2, 4, 8, 16, 32)
  df = data.frame()
  for (i in 1:length(lott_mags)){
    stddev_l = sqrt(0.5*lott_mags[i]^2*0.5)
    u_l = 0.5*lott_mags[i] - kappa*stddev_l
    u_s = 1*3
    temp_df = data.frame(y = rnorm(5000, u_l, sigma), lott_mag = lott_mags[i], u_l = u_l)
    df = rbind(df, temp_df)
  }
  p = ggplot(df, aes(x = y, color = as.factor(lott_mag))) + theme_classic(BASE_SIZE) +
    geom_density() + xlab('u_l distribution') + ylab('Density') +
    geom_vline(mapping = aes(xintercept = u_l), linetype = 'dashed') +
    scale_x_continuous(breaks = lott_mags, limits = c(0, 25)) +
    theme(legend.position = 'none', axis.text.y = element_blank(), axis.ticks.y = element_blank())
  return(p)
}

make_figureS = function(){
  models = get_kfold_models()
  for (model in models){
    if (model == 'kappa-sigma'){
      next
    } else{
      cat(sprintf('%s \n', model))
      figureS_sanity(model, N = 20)
    }
  }
  p = figureS_rho('rho')
  scale_save(p, 'figureS_rho', 80, 80)
}

figureS_training = function(){
  # computed the average expected value per trial of an agent that chose randomly, 
  # and a perfect expected value maximizer, or an agent that always chose the side with the greater expected value. 
  eff_df = read.csv('figures/csv/efficiency.csv')
  subj_list = unique(eff_df$subjid)
  for (subj in subj_list){
    p = ggplot(eff_df %>% filter(subjid == subj), aes(x = days, y = efficiency)) + theme_classic(BASE_SIZE) +
      geom_point(shape = 21, size = 0.8) + scale_y_continuous(limits = c(0.5, 1)) +
      scale_x_continuous(limits = c(0, 250)) +
      geom_hline(yintercept = median(eff_df$efficiency, na.rm = TRUE), linetype = 'dashed') +
      ggtitle(subj) + xlab('Training days') + ylab('Efficiency') 
    scale_save(p, sprintf('efficiency/%s', subj), 12, 8, 1)
  }
  return(p)
}

figureS_sanity = function(model = 'rho-sigma', N = 20){
  # Generate model parameters from the prior range, simulate dataset, and see if the model can recover them accurately
  # returns a scatter plot of predictions vs. true values
  params_df = data.frame(true = NULL, params = NULL, model_mean = NULL, model_sd = NULL)
  params = get_params(model)
  # define parameter-specific limits and breaks
  lower_limits = list('rho' = 0, 'kappa' = -2, 'sigma' = 0, 'omega.1' = 0, 'omega.2' = 0, 'omega.3' = 0, 'omega_win.1' = 0, 'omega_win.2' = 0, 'omega_win.3' = 0, 'omega_lose.1' = 0, 'omega_lose.2' = 0, 'omega_lose.3' = 0)
  upper_limits = list('rho' = 2, 'kappa' = 2, 'sigma' = 6, 'omega.1' = 1, 'omega.2' = 1, 'omega.3' = 1, 'omega_win.1' = 1, 'omega_win.2' = 1, 'omega_win.3' = 1, 'omega_lose.1' = 1, 'omega_lose.2' = 1, 'omega_lose.3' = 1)
  lower_breaks = list('rho' = 0, 'kappa' = -2, 'sigma' = 0, 'omega.1' = 0, 'omega.2' = 0, 'omega.3' = 0, 'omega_win.1' = 0, 'omega_win.2' = 0, 'omega_win.3' = 0, 'omega_lose.1' = 0, 'omega_lose.2' = 0, 'omega_lose.3' = 0)
  middle_breaks = list('rho' = 1, 'kappa' = 0, 'sigma' = 3, 'omega.1' = 0.5, 'omega.2' = 0.5, 'omega.3' = 0.5, 'omega_win.1' = 0.5, 'omega_win.2' = 0.5, 'omega_win.3' = 0.5, 'omega_lose.1' = 0.5, 'omega_lose.2' = 0.5, 'omega_lose.3' = 0.5)
  upper_breaks = list('rho' = 2, 'kappa' = 2, 'sigma' = 6, 'omega.1' = 1, 'omega.2' = 1, 'omega.3' = 1, 'omega_win.1' = 1, 'omega_win.2' = 1, 'omega_win.3' = 1, 'omega_lose.1' = 1, 'omega_lose.2' = 1, 'omega_lose.3' = 1)
  # use fit parameters on real subjects
  get_mlp = function(x, max_lp){
    return(mean(x[max_lp]))
  }
  fits_df = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model))
  fits_df = fits_df %>% filter(subjid > 2000 & subjid < 3000) %>% group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
    summarise_at(.vars = vars(params), ~ get_mlp(.x, max_lp)) %>% sample_n(N)
  for (n in 1:N){
    # generate parameters using the same as hyperpriors
    # true_params = list()
    # true_params$rho = exp(rnorm(1, log(1), 0.3))
    # true_params$kappa = rnorm(1, 0, 0.5)
    # true_params$sigma = rgamma(1, 6, 3)
    # omega = rdirichlet(1, alpha = c(6, 2, 2))
    # omega_win = rdirichlet(1, alpha = c(6, 2, 2))
    # omega_lose = rdirichlet(1, alpha = c(6, 2, 2))
    # true_params$omega = omega
    # true_params$omega_win = omega_win
    # true_params$omega_lose = omega_lose
    # true_params$omega.1 = omega[1]
    # true_params$omega.2 = omega[2]
    # true_params$omega.3 = omega[3]
    # true_params$omega_win.1 = omega_win[1]
    # true_params$omega_win.2 = omega_win[2]
    # true_params$omega_win.3 = omega_win[3]
    # true_params$omega_lose.1 = omega_lose[1]
    # true_params$omega_lose.2 = omega_lose[2]
    # true_params$omega_lose.3 = omega_lose[3]
    # create synthetic data
    true_params = as.list(fits_df[n, ])
    n_trials = 3000
    df = data.frame(lottery_mag = sample(c(0, 2, 4, 8, 16, 32), n_trials, replace = TRUE),
                    prev_outcome = sample(c(-1, 0, 1), n_trials, replace = TRUE),
                    prev_choice = sample(c(0, 1), n_trials, replace = TRUE), # this is wrong, but we are not using this so it's fine
                    sb_mag = 3, total_rew_multi = 8, lottery_prob = 0.5) %>%
      mutate(delta_ev = total_rew_multi*lottery_mag*lottery_prob - total_rew_multi*sb_mag)
    df$choice = toggle_agent(model, params = true_params, sb_mag = df$sb_mag, lottery_mag = df$lottery_mag,
                             lottery_prob = df$lottery_prob, total_rew_multi = df$total_rew_multi, prev_outcome = df$prev_outcome, probs = FALSE)
    # check if glm is good
    #m1 = glm('choice ~ delta_ev', df, family = binomial)
    #cf = coef(summary(m1))
    # not too flat, not too steep
    #if (cf['delta_ev', 'Estimate'] > 0.05 & mean(df$choice) > 0.3 & cf['delta_ev', 'Estimate'] < 1){
    # fit model
    fit = fit_toggle(model, df)
    draws = as.data.frame(rstan::extract(fit))
    max_lp = which.max(draws$lp__)
    temp_df = data.frame(true = unlist(true_params[params]), 
                         params = params, 
                         mlp = as.numeric(draws[max_lp,][params]),
                         n = n)
    params_df = rbind(params_df, temp_df)
    #} else{
    #  message('Not a good subject')
    #}
  }
  # make the scatter plot
  params_df$n = as.factor(params_df$n)
  for (i in 1:length(params)){
    param = params[i]
    this_df = params_df %>% filter(params == param)
    corr = cor.test(this_df$true, this_df$mlp)
    p = ggplot(this_df, aes(x = true, y = mlp, color = n)) + theme_classic(base_size = BASE_SIZE) +
      geom_point() + xlab(TeX(latex_params[param])) +
      theme(legend.position = 'none') +
      ylab('Model Estimate') + geom_abline(slope = 1, linetype =  'dashed', alpha = 0.4) +
      geom_smooth(method = lm, formula = y ~ x, color = 'gray', size = 0.2) +
      annotate("text", label = sprintf('R = %.3f', corr$estimate), x = middle_breaks[[param]], y = 0.1) + 
      scale_x_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) +
      scale_y_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) +
      theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12))
    if (i == 1){P = p} 
    else{p = p + theme(axis.title.y = element_blank())
    P = P + p}
  }
  P = P + plot_layout(ncol = length(params))
  scale_save(P, sprintf('figureS_sanity_%s', model), length(params)*6, 6, scale = 1)
  return(P)
  #scale_save(P, sprintf('figureS_sanity_%s', model), 6*6, 12, scale = 1) # for history plots
}

figureS_parameter_summary = function(){
  # summary of all model parameters like in Constantinople 2019 FIgure S1F
  get_mlp = function(x, max_lp){
    return(mean(x[max_lp]))
  }
  ss = c('Mouse', 'Rat', 'Human')
  params = get_params('history-mix-rho-kappa-sigma')
  params_df = read.csv('figures/csv/fits/history-mix-rho-kappa-sigma_fits.csv') %>% 
    mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  for (param in params){
    # find MLL estimates
    df1 = params_df %>% group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
      summarise_at(.vars = vars(param), ~ get_mlp(.x, max_lp)) 
    colnames(df1)[2] = 'med'
    df2 = params_df %>% group_by(subjid) %>% 
      summarise_at(.vars = vars(param), ~ quantile(.x, 0.025))
    colnames(df2)[2] = 'low'
    df3 = params_df %>% group_by(subjid) %>% 
      summarise_at(.vars = vars(param), ~ quantile(.x, 0.975))
    colnames(df3)[2] = 'high'
    mll_df = merge(df1, df2, by.x = 'subjid')
    mll_df = merge(mll_df, df3, by.x = 'subjid')
    mll_df = mll_df %>% mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
    for (i in 1:3){
      this_df = mll_df %>% filter(species_name == ss[i])
      p = ggplot(this_df, aes(x = med, xmin = low, xmax = high, y = reorder(subjid, med))) + theme_classic(BASE_SIZE) +
        geom_point(color = SPECIES_COLORS[[ss[i]]], size = 0.8) + geom_errorbarh(color = SPECIES_COLORS[[ss[i]]]) + 
        geom_vline(xintercept = median(params_df %>% filter(species_name == ss[i]) %>% pull(param)), alpha = 0.6) + 
        ggtitle(TeX(latex_params[[param]])) + xlab(' ') + ylab('') +  
        scale_x_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                           breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) +
        scale_y_discrete(breaks = seq(10, round(nrow(this_df)/10)*10, by = 10)) +
        theme(plot.title = element_text(hjust = 0.5))
      scale_save(p, sprintf("%s_%s", ss[i], param), 8, 10, 0.8)
    }
  }
}


# run this to keep the list consistent
# df = read.csv('figures/csv/preprocessed_all_freq.csv') # use the most freq task param
# n_per_species = 20
# ss = c('Mouse', 'Rat', 'Human')
# bad_list = c(1089, 1108, 1110, 1134, 1135, 1281, 1258, 1259, 1263, 1291, 2139, 2141, 2173, 2165, 2155,
#              5032, 5064, 5063, 5085, 5017, 5075, 5100)
# df = df %>% filter(!subjid %in% bad_list)
# for (i in 1:length(ss)){
#   this_df = df %>% filter(species_name == ss[i])
#   if (ss[i] == 'Mouse'){MOUSE_LIST = sample(unique(this_df$subjid), n_per_species)}
#   if (ss[i] == 'Rat'){RAT_LIST = sample(unique(this_df$subjid), n_per_species)}
#   if (ss[i] == 'Human'){HUMAN_LIST = sample(unique(this_df$subjid), n_per_species)}
# }
# GOOD_LIST = list(MOUSE_LIST, RAT_LIST, HUMAN_LIST)

figureS_model_pred = function(df, model = 'rho-sigma', n_per_species = 20){
  # for each model, for each subject, plot the model prediction ribbon overlaying binned data
  #df = read.csv('figures/csv/preprocessed_all_freq.csv') # use the most freq task param
  #df = read.csv('figures/csv/preprocessed_all.csv')
  bad_list = c(1089, 1108, 1110, 1134, 1135, 1281, 1259, 1263, 2139, 2141, 2173, 2165, 2155,
               5032, 5064, 5063, 5085, 5017, 5075, 5100)
  df = df %>% filter(!subjid %in% bad_list)
  
  ss = c('Mouse', 'Rat', 'Human')
  pred_df = read.csv(sprintf('figures/csv/pred/%s_pred_freq.csv', model)) %>% 
    mutate(species = ifelse(subjid > 5000, 'Human', ifelse(subjid > 2000, 'Rat', 'Mouse')))
  #pred_df = read.csv(sprintf('figures/csv/pred/%s_pred.csv', model)) %>% 
  #  mutate(species = ifelse(subjid > 5000, 'Human', ifelse(subjid > 2000, 'Rat', 'Mouse')))
  for (i in 1:length(ss)){
    s = ss[i]
    if (is.null(n_per_species)){
      this_df = df %>% filter(species_name == s)
      this_pred_df = pred_df %>% filter(species == s)
    } else{
      this_df = df %>% filter(species_name == s)
      this_df = this_df %>% filter(subjid %in% GOOD_LIST[[i]])
      this_pred_df = pred_df %>% filter(subjid %in% GOOD_LIST[[i]])
    }
    
    p = ggplot(this_df) + theme_classic(base_size = BASE_SIZE) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
      geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) + 
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") + ggtitle(s) + 
      facet_wrap(~subjid, scales = 'free') + theme(title = element_text(size = 20), axis.text.x = element_text(size = 10),
                                                   axis.text.y = element_text(size = 10))
    if (str_detect(model, 'history')){
      p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(prev_outcome)), fun.data = bino, bins = 7, geom = 'pointrange') +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_80, ymax = ymax_80, fill = as.factor(pred_prev_outcome)), alpha = 0.7) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_95, ymax = ymax_95, fill = as.factor(pred_prev_outcome)), alpha = 0.4) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_99, ymax = ymax_99, fill = as.factor(pred_prev_outcome)), alpha = 0.2) +
        scale_color_manual(values = PREV_OUTCOME_COLORS) + 
        scale_fill_manual(values = PREV_OUTCOME_COLORS) + theme(legend.position = 'none')
    } else{
      p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, bins = 7, geom = 'pointrange') +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_80, ymax = ymax_80), alpha = 0.7, fill = SPECIES_COLORS[s]) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_95, ymax = ymax_95), alpha = 0.4, fill = SPECIES_COLORS[s]) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_99, ymax = ymax_99), alpha = 0.2, fill = SPECIES_COLORS[s])
    }
    scale_save(p, sprintf('%s_%s_pred', model, s), width = 25, height = 25, scale = 1)
    #if (i == 1){P = p} else{P = P + p}
  }
  #P = P + plot_annotation(title = model) & theme(plot.title = element_text(size = 30))
  #scale_save(P, sprintf('%s_model_pred', model), width = 60, height = 30, scale = 1)
}

figureS_individiual_elpd = function( ){
  # by species or by individual cross-validation ELPD comparisons
  models_list = get_kfold_models()
  elpd_df = read.csv('figures/csv/elpd.csv')
  all_species = c('Mouse', 'Rat', 'Human')
  elpd_df$model = factor(elpd_df$model, levels = models_list)
  for (i in 1:3){
    this_df = elpd_df %>% filter(species == all_species[i])
    best_df = this_df %>% group_by(subjid) %>% mutate(best_elpd = max(elpd)) %>% 
      filter(elpd == best_elpd)
    p = ggplot(this_df) + theme_classic(BASE_SIZE) + 
      geom_point(aes(y = elpd, x = model, color = model), position = position_dodge(width = 1), size = 1) + 
      geom_errorbar(aes(y = elpd, ymin = elpd_min, ymax = elpd_max, x = model, color = model), position = position_dodge(width = 1), size = 0.1) +
      geom_point(best_df, mapping = aes(x = model, y = best_elpd), size = 4, color = 'indianred', shape = 21) +
      ylab('ELPD') + xlab('Model') + ggtitle(all_species[i]) + 
      scale_color_manual(values = model_colors) + 
      theme(legend.position = 'right', 
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
            axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            panel.grid = element_line(size = 0.3),
            panel.grid.major = element_line(), panel.grid.minor = element_line()) + 
      facet_wrap(~subjid, scales = 'free')
    scale_save(p, sprintf('individual_elpd_%s', all_species[i]), 40, 30, 1)
    if (i == 1){
      P = p
    } else if (i == 3){
      p = p + theme(legend.position = 'right')
      P = P + p
    }else{
      P = P + p
    }
  }
  #P = P + plot_layout(ncol = 3, nrow = 1)
  #scale_save(P, 'individual_elpd', 100, 30, 1)
}

figure_individiual_elpd = function(subj = 2053){
  # by species or by individual cross-validation ELPD comparisons
  models_list = get_kfold_models()
  elpd_df = read.csv('figures/csv/elpd.csv')
  all_species = c('Mouse', 'Rat', 'Human')
  elpd_df$model = factor(elpd_df$model, levels = models_list)
  this_df = elpd_df %>% filter(subjid == subj)
  best_df = this_df %>% group_by(subjid) %>% mutate(best_elpd = max(elpd)) %>% 
    filter(elpd == best_elpd)
  p = ggplot(this_df) + theme_classic(BASE_SIZE) + 
    geom_point(aes(y = elpd, x = model, color = model), position = position_dodge(width = 1), size = 1) + 
    geom_errorbar(aes(y = elpd, ymin = elpd_min, ymax = elpd_max, x = model, color = model), position = position_dodge(width = 1), size = 0.1) +
    geom_point(best_df, mapping = aes(x = model, y = best_elpd), size = 4, color = 'indianred', shape = 21) +
    ylab('ELPD') + xlab('Model') + ggtitle(subj) + 
    scale_color_manual(values = model_colors) + 
    theme(legend.position = 'none', 
          axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          panel.grid = element_line(size = 0.3),
          panel.grid.major = element_line(), panel.grid.minor = element_line())
  return(p)
}

figureS_top_3 = function(){
  # Plot fits from the top three best-fitting model for each subject
  elpd_df = read.csv('figures/csv/elpd.csv') 
  top_df = elpd_df %>% group_by(subjid) %>% top_n(3) %>% arrange(subjid, desc(elpd))
  
  df = read.csv('figures/csv/preprocessed_all.csv') %>% mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  subj_list = unique(df$subjid)
  
  elpd_df = read.csv('figures/csv/elpd.csv')
  for (sx in 1:length(subj_list)){
    subj = subj_list[sx]
    for (fx in 1:3){
      model = as.character(top_df$model[top_df$subjid == subj][fx])
      subj_df = df %>% filter(subjid == subj)
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
      data$include_constant = str_detect(model, 'constant')
      data$include_mix = str_detect(model, 'mix')
      data$include_perseverance = str_detect(model, 'perseverance')
      data$include_history = str_detect(model, 'history')
      cat(sprintf('Fitting %s on %d \n', model, subj))
      fit = sampling(toggle_pred_model, data = data, seed = SEED, refresh = 0, warmup = 200, iter = 400)
      # extract simulated n_chose_lott
      draws = rstan::extract(fit)
      ncl_df = as.data.frame(t(draws$pred_n_chose_lott))
      pred_df = pred_df %>% mutate(y = rowMeans(ncl_df)/pred_n_trials, 
                                   ymin = apply(ncl_df, 1, quantile, 0.1)/pred_n_trials,  
                                   ymax = apply(ncl_df, 1, quantile, 0.9)/pred_n_trials)
      # plot
      draws = as.data.frame(draws)
      params = get_params(model)
      p = ggplot(subj_df) + theme_classic(base_size = BASE_SIZE) +
        geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
        geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) + 
        scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
        scale_x_continuous(breaks = c(round(min(subj_df$delta_ev)/10)*10, 0, round(max(subj_df$delta_ev)/10)*10)) +
        xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") + ggtitle(model) +
        theme(legend.position = 'none', plot.title = element_text(size = 8))
      if (str_detect(model, 'history')){
        p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(prev_outcome)), bins = 7, fun.data = bino, geom = 'pointrange') +
          geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin, ymax = ymax, fill = as.factor(pred_prev_outcome)), alpha = 0.5) +
          scale_color_manual(values = PREV_OUTCOME_COLORS) + scale_fill_manual(values = PREV_OUTCOME_COLORS)
      } else{
        p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), bins = 7, fun.data = bino, geom = 'pointrange') +
          geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin, ymax = ymax), alpha = 0.5, fill = model_colors[[model]])
      }
      if (fx != 3){
        p = p + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
      }
      if (fx == 1){
        P = p
      } else{
        P = P + p
      }
    }
    # make elpd bar plot for this individual
    elpd_p = ggplot(elpd_df %>% filter(subjid == subj)) + theme_classic(BASE_SIZE) + 
      geom_point(aes(y = elpd, x = reorder(model, -elpd), color = model), position = position_dodge(width = 1), size = 1) + 
      geom_errorbar(aes(y = elpd, ymin = elpd_min, ymax = elpd_max, x = reorder(model, -elpd), color = model), position = position_dodge(width = 1), size = 0.1) +
      #geom_point(best_df, mapping = aes(x = model, y = best_elpd), size = 4, color = 'indianred', shape = 21) +
      ylab('ELPD') + xlab(' ') + 
      scale_color_manual(values = model_colors) + 
      theme(legend.position = 'none', 
            axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
            axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0, size = 8), 
            panel.grid = element_line(size = 0.3),
            panel.grid.major = element_line(), panel.grid.minor = element_line())
    
    P = P + elpd_p + plot_layout(ncol = 1, nrow = 4) + plot_annotation(title = subj)
    scale_save(P, sprintf('top_3/top_3_%d', subj), width = 10, height = 30, scale = 1) 
  }
}

figureS_confusion_matrix = function(){
  # plot confusion matrix
  # 1. Use toggle model to fit each subject, and use the fitted parameters to generate synthetic datasets
  # 2. Use each model to perform cross-validation on the synthetic datasets
  # 3. The result is a n_model by n_model matrix, with each grid representing how good the fitting model can fit the generating model
  # this has been done in HPC (takes too much time), the results are saved in csv/syn/ and csv/cm/
  gen_models_list = get_kfold_models()
  fitting_models_list = get_kfold_models()
  cm_df = data.frame()
  for (gen_model in gen_models_list){
    for (fitting_model in fitting_models_list){
      df = read.csv(sprintf('figures/csv/cm/gen_%s/%s_kfold.csv', gen_model, fitting_model)) %>% 
        mutate(gen_model = gen_model, fitting_model = fitting_model)
      cm_df = rbind(cm_df, df)
    }
  }
  cm_df$fitting_model = factor(cm_df$fitting_model, fitting_models_list)
  subj_list = unique(cm_df$subjid)
  cm_df = cm_df %>% group_by(subjid, gen_model, fitting_model) %>% 
    summarise(elpd_mean = mean(elpd, na.rm = TRUE), elpd_min = quantile(elpd, 0.05, na.rm = TRUE), elpd_max = quantile(elpd, 0.95, na.rm = TRUE)) %>% 
    group_by(subjid, gen_model) %>% mutate(best_model = fitting_models_list[which.max(elpd_mean)])
  # make one tile plot each animal
  for (subj in subj_list){
    this_cm_df = cm_df %>% filter(subjid == subj) %>% 
      group_by(gen_model) %>% mutate(best_model = fitting_models_list[which.max(elpd_mean)])
    p = ggplot(cm_df %>% filter(subjid > 5000), aes(x = fitting_model, y = gen_model)) + 
      geom_tile(aes(fill = elpd_mean)) + 
      geom_tile(aes(x = best_model, y = gen_model, fill = elpd_mean), color = '#FF8300') + 
      scale_fill_gradient(low = "white", high = "#4D4C60", name = 'ELPD') +
      geom_text(aes(label = sprintf("%.1f", elpd_mean)), size=3) + # label ELPD
      xlab('Fitting model') + ylab('Generative model') +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, face = 'bold', color = as.character(unlist(model_colors))),
            axis.text.y = element_text(face = 'bold', color = as.character(unlist(model_colors)))) +
      facet_wrap(~subjid)
    #scale_save(p, 'cm_human', 100, 100, 1)
    return(p)
  }
}

is_upper = function(i, total = 25){
  # calculates if i is in the upper diagnal of the square matrix 
  n = sqrt(total)
  row_num = ceiling(i / n)
  diagnal = seq(1, total, n+1)
  row_ends = seq(n, total+1, by = n)
  row_ends[length(row_ends)] = row_ends[length(row_ends)] - 1 
  is_upper = (i > diagnal[row_num] & i <= row_ends[row_num])
  return(is_upper)
}

figureS_rho = function(param = 'rho'){
  # aggregate pairs plot: each column and row is a model, rho is plotted as colored by species
  # param_models = get_kfold_models()[str_detect(get_kfold_models(), param)]
  # param_models = c('rho-sigma', 'rho-kappa-sigma', 'mix-rho-sigma', 'history-mix-rho-sigma', 'history-mix-rho-kappa-sigma')
  param_models = c('rho-sigma', 'rho-scalar', 'mix-rho-sigma', 'mix-rho-scalar', 'history-mix-rho-sigma', 'history-mix-rho-scalar', 
                   'rho-kappa-sigma', 'rho-kappa-scalar', 'mix-rho-kappa-sigma', 'mix-rho-kappa-scalar', 'history-mix-rho-kappa-sigma', 'history-mix-rho-kappa-scalar')
  n_params = length(param_models)
  param_df = data.frame()
  for (model in param_models){
    fits_df = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model))
    temp_df = fits_df %>% group_by(model, subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
      summarise(best_param = mean(rho[max_lp])) %>% ungroup() # change here
    param_df = rbind(param_df, temp_df)
  }
  param_df = param_df %>% mutate(species = case_when(subjid < 2000 ~ 'Mouse', 
                                                     (subjid > 2000 & subjid < 3000) ~ 'Rat', 
                                                     subjid > 5000 ~ 'Human'))
  example_subject = sample(param_df$subjid, 1)
  # pairs plot
  model_combos = as.data.frame(expand.grid(param_models, param_models))
  #model_combos$Var1 = factor(model_combos$Var1, c('rho-sigma', 'rho-scalar', 'mix-rho-sigma', 'mix-rho-scalar', 'history-mix-rho-sigma', 'history-mix-rho-scalar', 
  #                                                'rho-kappa-sigma', 'rho-kappa-scalar', 'mix-rho-kappa-sigma', 'mix-rho-kappa-scalar', 'history-mix-rho-kappa-sigma', 'history-mix-rho-kappa-scalar'))
  diagnal = seq(1, dim(model_combos)[1], length(param_models)+1)
  for (i in 1:nrow(model_combos)){
    x = as.character(model_combos[i,1])
    y = as.character(model_combos[i,2])
    if (is_upper(i, nrow(model_combos))){
      P = P + plot_spacer()
    } else{
      if (i %in% diagnal){ # posterior density plot colored by species
        this_df = param_df %>% filter(model == x)
        p = ggplot(this_df, aes(x = best_param, fill = species)) + theme_classic(base_size = BASE_SIZE) +
          geom_density(alpha = 0.4) +
          #geom_vline(mapping = aes(xintercept = apply(this_df, 2, median), color = species), size = 1) + 
          scale_fill_manual(values = SPECIES_COLORS) +
          scale_x_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) +
          theme(legend.position = 'none', axis.title = element_blank(), 
                axis.text.y = element_blank(), axis.ticks.y = element_blank())
      } else{ # posterior scatter plot colored by species
        this_df = param_df %>% filter(model == x | model == y) %>% 
          pivot_wider(names_from = model, values_from = best_param)
        new_x = gsub('-', '_', x, fixed = TRUE)
        new_y = gsub('-', '_', y, fixed = TRUE)
        colnames(this_df)[3] = new_x
        colnames(this_df)[4] = new_y
        p = ggplot(this_df, aes_string(x = new_x, y = new_y, color = 'species', fill = 'species')) + 
          theme_classic(base_size = BASE_SIZE) +
          scale_color_manual(values = SPECIES_COLORS) +
          scale_fill_manual(values = SPECIES_COLORS) +
          geom_point(size = 1) + 
          geom_smooth(method = lm, formula = y ~ x, size = 0.2, alpha = 0.2) + 
          scale_x_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) +
          scale_y_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) +
          geom_abline(intercept = 0, slope = 1, linetype = 'dashed', alpha = 0.6) +
          theme(legend.position = 'none', axis.title = element_blank())
      }
      # control x and y ticks
      if (i %% n_params == 1){ # if in the first column
        p = p + theme(axis.title.y = element_text(size = 15, angle = 0, hjust = 0, vjust = 0))
      }
      if (i > (nrow(model_combos) - n_params)){ # if in the last row
        p = p + theme(axis.title.x = element_text(size = 15, angle = -90, hjust = 0, vjust = 0))
      }
      if (i == 1){P = p} else{P = P + p}
    }
  }
  P = P + plot_layout(ncol = length(param_models), nrow = length(param_models))
  return(P)
}
