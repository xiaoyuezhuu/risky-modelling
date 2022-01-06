# Figure_behavior: example animal behavior with rho-sigma model fits 
make_figure_behavior = function(){
  plot_utility_curves(model = 'rho-sigma') # saves on its own
  plot_utility_curves(model = 'history-mix-rho-kappa-sigma') # saves on its own
  p = example_model_fits(model = 'rho-sigma')
  scale_save(p, 'figure_behavior_b', 25, 12, 1)
  p = example_model_fits(model = 'history-mix-rho-kappa-sigma')
  scale_save(p, 'figure_behavior2_b', 25, 12, 1)
  p = best_params_scatter(model = 'rho-sigma', params = c('rho', 'sigma'))
  scale_save(p, 'figure_behavior_c', 8, 8, 1)
  p = best_params_scatter(model = 'history-mix-rho-kappa-sigma', params = c('rho', 'kappa'))
  scale_save(p, 'figure_behavior2_c', 8, 8, 1)
  p = compare_concat_dist(model = 'rho-sigma', params = c('rho', 'sigma'))
  scale_save(p, 'figure_behavior_d', 9, 6, 1)
  p = compare_concat_dist(model = 'history-mix-rho-kappa-sigma', params = c('rho', 'kappa'))
  scale_save(p, 'figure_behavior2_d', 9, 6, 1)
  p = plot_indifference_point('rho-sigma')
  scale_save(p, 'figure_behavior_ip', 4, 6, 1)
  p = plot_indifference_point('history-mix-rho-kappa-sigma')
  scale_save(p, 'figure_behavior2_ip', 4, 6, 1)
}

plot_utility_curves = function(model = 'rho-sigma'){
  # Use rho estimates from the model to plot utility curves for each species
  params_df = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model)) %>% 
    group_by(subjid) %>% mutate(where_mlp = which.max(lp__)) %>% summarise(best_rho = mean(rho[where_mlp]))
  df = read.csv('figures/csv/preprocessed_all.csv')
  ss = c('Mouse', 'Rat', 'Human')
  xlabs = c('Water Volume(x)', 'Water Volume(x)', 'Money (x)')
  for (i in 1:3){
    this_df = df %>% filter(species_name == ss[i])
    subj_list = unique(this_df$subjid)
    util_df = data.frame()
    for (subj in subj_list){
      rho = params_df$best_rho[params_df$subjid == subj]
      lott_mags = unique(df %>% filter(subjid == subj) %>% pull(lottery_mag))
      lott_probs = unique(df %>% filter(subjid == subj) %>% pull(lottery_prob))
      temp_df = expand.grid(lottery_mag = lott_mags, lottery_prob = lott_probs, subjid = subj, rho = rho)
      util_df = rbind(util_df, temp_df)
    }
    util_df = util_df %>% mutate(utility = lottery_mag^rho)
    p = ggplot() + theme_classic(BASE_SIZE) + 
      xlab(xlabs[i]) + ylab('u(x)') +
      scale_x_continuous(breaks = c(0, 0.5, 1), limits = c(-0.05, 1.05)) + 
      scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(-0.05, 1.05)) +
      geom_abline(linetype = 'dashed')
    for (subj in subj_list){
      if (ss[i] == 'Human'){
        p = p + geom_line(util_df %>% filter(subjid == subj), 
                          mapping = aes(x = lottery_mag/max(lottery_mag), y = utility/max(utility), alpha = as.factor(lottery_prob)), color = SPECIES_COLORS[[ss[i]]], alpha = 0.6) 
      } else{
        p = p + geom_line(util_df %>% filter(subjid == subj), mapping = aes(x = lottery_mag/max(lottery_mag), y = utility/max(utility)), color = SPECIES_COLORS[[ss[i]]], alpha = 0.6)
      }
    }
    scale_save(p, sprintf('%s_utility', ss[i]), 8, 8, 1)
  }
}

example_model_fits = function(model = 'rho-sigma', subj_list = c(1368, 2158, 5026, 1278, 2181, 5018)){
  # Figure 2a: example data + model fit of a model, with three columns and two rows
  # each column represents one species, top row = risk-averse subjects, bottom row = risk-seeking subjects
  df = read.csv('figures/csv/preprocessed_all_freq.csv') %>% filter(subjid %in% subj_list) %>% 
    mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  pred_df = read.csv(sprintf('figures/csv/pred/%s_pred_freq.csv', model)) %>% 
    filter(subjid %in% subj_list) %>% 
    mutate(species = ifelse(subjid > 5000, 'Human', ifelse(subjid > 2000, 'Rat', 'Mouse')))
  colors = c('azure4', 'chocolate4', 'lightpink3', 'azure4', 'chocolate4', 'lightpink3')
  for (sx in 1:length(subj_list)){
    subj = subj_list[sx]
    subj_df = df %>% filter(subjid == subj)
    this_pred_df = pred_df %>% filter(subjid == subj)
    # plot
    #draws = as.data.frame(draws)
    #params = get_params(model)
    #label1 = sprintf("%s %d", unique(subj_df$species_name), subj)
    #label2 = sprintf("n = %d", dim(subj_df)[1])
    #prev_y = 0.2
    #unit = 0.1
    lower_break = round(min(subj_df$delta_ev)/5)*5
    upper_break = round(max(subj_df$delta_ev)/5)*5
    p = ggplot(subj_df) + theme_classic(base_size = BASE_SIZE) +
      geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
      geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) + 
      scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
      #scale_x_continuous(limits = c(lower_break - 2, upper_break + 2), breaks = c(lower_break, 0, upper_break)) +
      #annotate("text", label = label1, x = max(subj_df$delta_ev)*0.3, y = 0.3, hjust = 0 , size = 5) + 
      #annotate("text", label = label2, x = max(subj_df$delta_ev)*0.3, y = 0.2, hjust = 0, size = 5) + 
      xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)")
    if (str_detect(model, 'history')){
      p = p + geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_80, ymax = ymax_80, fill = as.factor(pred_prev_outcome)), alpha = 0.7) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_95, ymax = ymax_95, fill = as.factor(pred_prev_outcome)), alpha = 0.4) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_99, ymax = ymax_99, fill = as.factor(pred_prev_outcome)), alpha = 0.2) +
        stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(prev_outcome)), fun.data = bino, bins = 7, geom = 'pointrange') +
        scale_color_manual(values = PREV_OUTCOME_COLORS) + 
        scale_fill_manual(values = PREV_OUTCOME_COLORS) + theme(legend.position = 'bottom')
    } else{
      p = p + geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_80, ymax = ymax_80), alpha = 0.7, fill = colors[sx]) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_95, ymax = ymax_95), alpha = 0.4, fill = colors[sx]) +
        geom_ribbon(this_pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin_99, ymax = ymax_99), alpha = 0.2, fill = colors[sx]) +
        stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, bins = 7, geom = 'pointrange')
    }
    # for (param in params){
    #  prev_y = prev_y - unit
    #  this_label = sprintf("$\\%s = %.2f$", param, draws[which.max(draws$lp__), param])
    #  p = p + annotate("text", label = TeX(this_label), x = max(subj_df$delta_ev)*0.3, y = prev_y, hjust = 0, size = 5)
    # }
    if (sx %% 3 != 1){
      p = p + theme(axis.title.y = element_blank())
    }
    if (sx <= 3){
      p = p + theme(axis.title.x = element_blank())
    } 
    if (sx == 1){
      P = p
    } else{
      P = P + p
    }
  }
  P = P + plot_layout(ncol = 3, nrow = 2)
  return(P)
}

best_params_scatter = function(model = 'rho-sigma', params = c('rho', 'sigma')){
  # Figure 2b: parameter scatter plot with marginal histograms, colored by species
  # use fit parameters on real subjects
  get_mlp = function(x, max_lp){
    return(mean(x[max_lp]))
  }
  fits_df = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model)) %>% 
    group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
    summarise_at(.vars = vars(params), ~ get_mlp(.x, max_lp)) 
  fits_df = fits_df %>% mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  fits_df$species_name = factor(fits_df$species_name, c('Mouse', 'Rat', 'Human'))
  # make the scatter plot
  p = ggplot(fits_df, aes_string(x = params[1], y = params[2], color = 'species_name', fill = 'species_name')) + 
    theme_classic(base_size = BASE_SIZE) + 
    geom_point(shape = 21, color = 'black', size = 2) +
    #scale_x_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) + 
    #scale_y_continuous(limits = c(-1, 1), breaks = c(-1, 0, 1)) + 
    scale_x_continuous(limits = c(0, 2), breaks = c(0, 1, 2)) + 
    scale_y_continuous(limits = c(0, 10), breaks = c(0, 5, 10)) + 
    xlab(TeX(latex_params[params[1]])) + ylab(TeX(latex_params[params[2]])) +
    geom_vline(xintercept = 1, linetype = 'dashed', alpha = 0.4) + 
    scale_fill_manual(values = c('lightpink3', 'chocolate4', 'azure4')) + 
    scale_color_manual(values = c('lightpink3', 'chocolate4', 'azure4')) + 
    scale_fill_manual(values = SPECIES_COLORS) + 
    scale_color_manual(values = SPECIES_COLORS) + 
    theme(legend.position = 'none') 
  #p = ggMarginal(p, type = 'density', groupFill = TRUE, groupColour = FALSE, alpha = 0.4)
  return(p)
}

compare_concat_dist = function(model = 'rho-sigma', params = c('rho', 'sigma'), subsample = 100){
  # Figure 2c: compare species distribution by concatenating all individual samples together
  draws_pop = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model))
  draws_pop = draws_pop %>% mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat'))) %>% 
    group_by(species_name) %>% sample_n(2000, replace = TRUE)
  for (i in 1:length(params)){
    param = params[i]
    this_df = draws_pop %>% select(c(param, 'species_name'))
    plot_df = data.frame(mouse = this_df %>% filter(species_name == 'Mouse') %>% pull(param),
                         rat = this_df %>% filter(species_name == 'Rat') %>% pull(param),
                         human = this_df %>% filter(species_name == 'Human') %>% pull(param))
    color_scheme_set('gray')
    p = mcmc_areas(plot_df) + theme_bw(BASE_SIZE) +
      xlab(TeX(latex_params[param])) + 
      geom_vline(xintercept = median(plot_df$mouse), color = SPECIES_COLORS[2]) + 
      geom_vline(xintercept = median(plot_df$rat), color = SPECIES_COLORS[3]) + 
      geom_vline(xintercept = median(plot_df$human), color = SPECIES_COLORS[1]) +
      scale_y_discrete(labels = c('Mouse', 'Rat', 'Human')) +
      scale_x_continuous(limits = c(lower_limits[[param]], upper_limits[[param]]),
                         breaks = c(lower_breaks[[param]], middle_breaks[[param]], upper_breaks[[param]])) +
      theme(axis.text = element_text(color = 'black'))
    if (i == 1){
      P = p
    } else{
      p = p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                    axis.text.x = element_text(size = 13))
      P = P + p
    }
  }
  return(P)
}

indifference_point = function(model = 'rho-sigma'){
  # compute indifference points by MAP estimates from model
  params = get_params(model)
  get_mlp = function(x, max_lp){
    return(mean(x[max_lp]))
  }
  fits_df = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model)) %>% 
    group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
    summarise_at(.vars = vars(params), ~ get_mlp(.x, max_lp))
  fits_df = fits_df %>% mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  fits_df$species_name = factor(fits_df$species_name, c('Mouse', 'Rat', 'Human'))
  # use simulation to find the indifferent point
  indiff_df = data.frame(subjid = fits_df$subjid, species_name = fits_df$species_name, indiff_point = 0, model = model)
  fits_df = fits_df %>% select(!c('subjid', 'species_name'))
  for (i in 1:nrow(fits_df)){
    params_list = as.list(fits_df[i, ])
    sim_df = data.frame(sb_mag = 3, total_rew_multi = 8, lottery_mag = seq(1, 64, by = 0.01), lottery_prob = 0.5, 
                        prev_outcome = sample(c(-1, 0, 1), size = length(seq(1, 64, by = 0.01)), replace = TRUE))
    sim_df$choice_prob = toggle_agent(model = model, params = params_list, 
                                      sb_mag = sim_df$sb_mag, lottery_prob = sim_df$lottery_prob, 
                                      total_rew_multi = sim_df$total_rew_multi, lottery_mag = sim_df$lottery_mag,
                                      prev_outcome = sim_df$prev_outcome)
    x = sim_df[sim_df$choice_prob > 0.49 & sim_df$choice_prob < 0.51, ]
    indiff_df$indiff_point[i] = mean(x$lottery_mag * x$lottery_prob * x$total_rew_multi - x$sb_mag * x$total_rew_multi)
  }
  indiff_df = indiff_df[!is.na(indiff_df$indiff_point), ]
  p = mcmc_areas(indiff_df, pars = 'indiff_point')
  #write.csv(indiff_df, file = sprintf("figures/csv/%s_indiff.csv", model))
}

plot_indifference_point = function(model = 'rho-sigma'){
  df = read.csv(sprintf('figures/csv/%s_indiff.csv', model)) %>% select(c('indiff_point', 'species_name'))
  df = df[!is.na(df$indiff_point), ]
  plot_df = data.frame(mouse = df %>% filter(species_name == 'Mouse') %>% sample_n(100, replace = TRUE) %>% pull(indiff_point),
                       rat = df %>% filter(species_name == 'Rat') %>%  sample_n(100, replace = TRUE) %>% pull(indiff_point),
                       human = df %>% filter(species_name == 'Human') %>%  sample_n(100, replace = TRUE) %>% pull(indiff_point))
  p = mcmc_areas(plot_df) + theme_bw(BASE_SIZE) +
    xlab('Indiff point') + 
    geom_vline(xintercept = median(plot_df$mouse), color = SPECIES_COLORS[2]) + 
    geom_vline(xintercept = median(plot_df$rat), color = SPECIES_COLORS[3]) + 
    geom_vline(xintercept = median(plot_df$human), color = SPECIES_COLORS[1]) +
    scale_y_discrete(labels = c('Mouse', 'Rat', 'Human')) +
    scale_x_continuous(limits = c(-10, 50)) +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
                axis.text.x = element_text(size = 13))
  return(p)
}