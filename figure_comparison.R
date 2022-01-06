# figure_comparison: model comparisons
make_figure_comparisons = function(){
  p = plot_elpd(alluvial = TRUE)
  scale_save(p, 'figure_comparison_a', 20, 20)
  plot_elpd(alluvial = FALSE) # saves on its own
  p = top_3_fits_examples()
  scale_save(p, 'figure_comparison_c', width = 25, height = 15, scale = 1)
}

plot_elpd = function(type = 'alluvial'){
  # The best model according to the normalized ELPD for each individual and each species
  elpd_df = read.csv('figures/csv/elpd.csv')
  best_df = elpd_df %>% group_by(subjid) %>% mutate(best_elpd = max(elpd)) %>% filter(elpd == best_elpd)
  best_df = best_df %>% group_by(species) %>% add_tally() %>% rename(n_subjects = n)
  prop_df = best_df %>% group_by(species, model) %>% add_tally() %>% summarise(prop = (n / n_subjects)[1]) %>% 
    group_by(species) %>% mutate(is_best = prop == max(prop))
  count_df = best_df %>% group_by(species, model) %>% tally() %>% group_by(model) %>% mutate(total = sum(n)) %>% 
    filter(n > 2)
  if (type == 'alluvial'){ # make alluvial plot
    p = ggplot(count_df, aes(y = n, axis1 = reorder(model, -total), axis2 = species)) + theme_classic(BASE_SIZE) +
      geom_stratum(width = 1/10) +
      geom_alluvium(mapping = aes(fill = species), width = 1/10, alpha = 0.5) +
      geom_label(stat = "stratum", aes(label = after_stat(stratum)), hjust = 0.15, fill = NA, label.size = NA) +
      #scale_fill_manual(values = rev(as.character(model_colors[unique(count_df$model)]))) +
      scale_fill_manual(values = c('lightpink3', 'azure4', 'chocolate4')) +
      scale_x_discrete(limits = c("Best Model", "Species"), expand = c(.05, .05)) +
      ylab('Frequency') + theme(legend.position = 'none')
    return(p)
  } else if (type == 'bar'){
    # make a bar plot for proportions of the best models, subpanel by species 
    ss = c('Mouse', 'Rat', 'Human')
    for (i in 1:3){
      p = ggplot(prop_df %>% filter(species == ss[i]), aes(x = reorder(model, prop), y = prop, label = model)) + theme_classic(BASE_SIZE) +
        geom_bar(stat = 'identity', aes(fill = model)) + coord_flip() + 
        geom_label(aes(y = 0.12), fill = NA, label.size = NA) +
        ylab('') + xlab('') + ggtitle(ss[i]) + 
        scale_fill_manual(values = model_colors) +
        theme(legend.position = 'none', axis.text.y = element_blank(), axis.ticks.y = element_blank())
      scale_save(p, sprintf("%s_elpd_bar", ss[i]), 12, 10, 1)
    }
  } else if (type == 'pie'){
    # make a pie plot for proportions of the best models, subpanel by species 
    ss = c('Mouse', 'Rat', 'Human')
    for (i in 1:3){
      p = ggplot(prop_df %>% filter(species == ss[i]), aes(x = reorder(model, prop), y = prop*100, label = model)) + theme_classic(BASE_SIZE) +
        geom_bar(stat = 'identity', aes(fill = model), width = 1) +
        coord_polar("y", start = 0) +
        #geom_label(fill = NA, label.size = NA) +
        ylab('') + xlab('') + 
        scale_fill_manual(values = model_colors) +
        theme(axis.line = element_blank(), legend.position = 'none', axis.text.y = element_blank(), axis.ticks.y = element_blank())
      scale_save(p, sprintf("%s_elpd_pie", ss[i]), 8, 8, 1)
    }
  }
}

top_3_fits_examples = function(subj_list = c(1387, 2116, 5037)){
  # Example fits with the top three model, each column = example subject of one species, eac row = top 3 model fits
  elpd_df = read.csv('figures/csv/elpd.csv') 
  top_df = elpd_df %>% group_by(subjid) %>% top_n(3) %>% arrange(subjid, desc(elpd))
  
  df = read.csv('figures/csv/preprocessed_all_freq.csv') %>% filter(subjid %in% subj_list) %>% 
    mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  for (fx in 1:3){
    for (sx in 1:length(subj_list)){
      subj = subj_list[sx]
      subj_df = df %>% filter(subjid == subj)
      model = as.character(top_df$model[top_df$subjid == subj][fx])
      fit = fit_toggle_pred(model, subj_df)
      pred_df = gen_syn_smooth(subj_df, history = TRUE)
      # extract simulated n_chose_lott
      draws = rstan::extract(fit)
      ncl_df = as.data.frame(t(draws$pred_n_chose_lott))
      pred_df = pred_df %>% mutate(y = rowMeans(ncl_df)/pred_n_trials, 
                                   ymin = apply(ncl_df, 1, quantile, 0.1)/pred_n_trials,  
                                   ymax = apply(ncl_df, 1, quantile, 0.9)/pred_n_trials)
      # plot
      draws = as.data.frame(draws)
      params = get_params(model)
      labels = sprintf("%s %d \nn=%d \n%s", unique(subj_df$species_name), subj, dim(subj_df)[1], model)
      lower_break = round(min(subj_df$delta_ev)/5)*5
      upper_break = round(max(subj_df$delta_ev)/5)*5
      p = ggplot(subj_df) + theme_classic(base_size = BASE_SIZE) +
        geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) + 
        geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) + 
        scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
        scale_x_continuous(limits = c(lower_break - 2, upper_break + 2), breaks = c(lower_break, 0, upper_break)) +
        #annotate("text", label = TeX(labels), x = 0, y = 0, hjust = 0) + 
        xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)") +
        theme(legend.position = 'none', axis.title.x = element_blank(), axis.title.y = element_blank())
      if (str_detect(model, 'history')){
        if (subj == 5037){
          p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(prev_outcome)), bins = 7, fun.data = bino, geom = 'pointrange') + 
            geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin, ymax = ymax, fill = as.factor(pred_prev_outcome)), alpha = 0.5) +
            scale_color_manual(values = PREV_OUTCOME_COLORS) + scale_fill_manual(values = PREV_OUTCOME_COLORS)
        } else{
        p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice, color = as.factor(prev_outcome)), fun.data = bino, geom = 'pointrange') +
          geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin, ymax = ymax, fill = as.factor(pred_prev_outcome)), alpha = 0.5) +
          scale_color_manual(values = PREV_OUTCOME_COLORS) + scale_fill_manual(values = PREV_OUTCOME_COLORS)
        }
      } else{
        if (subj == 5037){
          p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), bins = 7, fun.data = bino, geom = 'pointrange') +
            geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin, ymax = ymax), alpha = 0.5, fill = model_colors[[model]])
        } else{
        p = p + stat_summary_bin(mapping = aes(x = delta_ev, y = choice), fun.data = bino, geom = 'pointrange') +
          geom_ribbon(pred_df, mapping = aes(x = pred_delta_ev, ymin = ymin, ymax = ymax), alpha = 0.5, fill = model_colors[[model]])
        }
      }
      # if (sx == 1){ # if first col
      #   p = p + theme(axis.title.y = element_text(size = 13, angle = 90))
      # }
      # if (fx == 3){ # if last row
      #   p = p + theme(axis.title.x = element_text(size = 13))
      # } 
      if (sx*fx == 1){
        P = p
      } else{
        P = P + p
      }
    }
  }
  P = P + plot_layout(ncol = 3, nrow = 3)
  return(P)
}

confusion_matrix_examples = function(){
  # plot confusion matrix
  # 1. Use toggle model to fit each subject, and use the fitted parameters to generate synthetic datasets
  # 2. Use each model to perform cross-validation on the synthetic datasets
  # 3. The result is a n_model by n_model matrix, with each grid representing how good the fitting model can fit the generating model
  # this has been done in HPC (takes too much time), the results are saved in csv/syn/ and csv/cm/
  #models_list = get_kfold_models()
  gen_models_list = c('rho-sigma', 'kappa-sigma', 'rho-kappa-sigma')
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
  #for (subj in subj_list){
  #this_cm_df = cm_df %>% filter(subjid == subj) %>% 
  #  group_by(gen_model) %>% mutate(best_model = fitting_models_list[which.max(elpd_mean)])
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
  #}
}


comparison_stats = function(model = 'rho-sigma', param = 'rho'){
  params_df = read.csv(sprintf('figures/csv/fits/%s_fits.csv', model)) %>% 
    group_by(subjid) %>% mutate(max_lp = which.max(lp__)) %>% 
    summarise_at(.vars = vars(param), ~ get_mlp(.x, max_lp)) %>% 
    mutate(species_name = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat')))
  m1 = lm(sprintf('%s ~ species_name', param), params_df)
  return(m1)
  # a = permutation.test(params_df %>% filter(species_name == 'Human') %>% pull(param),
  #                  params_df %>% filter(species_name == 'Mouse') %>% pull(param))
  # b = permutation.test(params_df %>% filter(species_name == 'Human') %>% pull(param),
  #                      params_df %>% filter(species_name == 'Rat') %>% pull(param))
  # c = permutation.test(params_df %>% filter(species_name == 'Rat') %>% pull(param),
  #                      params_df %>% filter(species_name == 'Mouse') %>% pull(param))
  # cat(sprintf("Human vs. Mouse: %.2f, Human vs. Rat: %.2f, Rat vs. Mouse: %.2f \n", a, b, c))
}