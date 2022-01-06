# figure_rho: compare rho estimates from different models
make_figure_rho = function(){
  p = corr_heatmap('rho')
  scale_save(p, 'figure_rho', 60, 28)
  p = corr_heatmap('kappa')
  scale_save(p, 'figure_kappa', 60, 28)
}

corr_heatmap = function(param = 'rho'){
  # a heatmap of the correlation coefficient of the parameter estimates for each species
  param_models = get_kfold_models()[str_detect(get_kfold_models(), param)]
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
  model_combos = as.data.frame(expand.grid(param_models, param_models))
  corr_df = data.frame()
  for (i in 1:nrow(model_combos)){
    x = as.character(model_combos[i,1])
    y = as.character(model_combos[i,2])
    if (x == y){
      this_df = param_df %>% filter(model == x) %>% pivot_wider(names_from = model, values_from = best_param)
      colnames(this_df)[3] = 'model1'
      this_df$model2 = this_df$model1
    } else{
      this_df = param_df %>% filter(model == x | model == y) %>% pivot_wider(names_from = model, values_from = best_param)
      colnames(this_df)[3] = 'model1'
      colnames(this_df)[4] = 'model2'
    }
    corx = this_df %>% group_by(species) %>% group_map(~ cor.test(.x$model1, .x$model2, metho = 'spearman'))
    temp_df = data.frame(corr = c(corx[[1]]$estimate, corx[[2]]$estimate, corx[[3]]$estimate), 
                         p = c(corx[[1]]$p.value, corx[[2]]$p.value, corx[[3]]$p.value), 
                         model1 = x, model2 = y, species = c('Human', 'Mouse', 'Rat'))
    corr_df = rbind(corr_df, temp_df)
  }
  # make the heat map
  factor_df = data.frame(model = param_models)
  factor_df = factor_df %>% mutate(has_kappa = str_detect(model, 'kappa'), 
                                   has_rho = str_detect(model, 'rho'),
                                   has_mix = str_detect(model, 'mix'), 
                                   has_scalar = str_detect(model, 'scalar')) %>% 
    arrange(has_kappa, has_mix, has_scalar) # for rho
    #arrange(has_rho, has_mix, has_scalar) # for kappa
  corr_df$model1 = factor(corr_df$model1, levels = factor_df$model)
  corr_df$model2 = factor(corr_df$model2, levels = factor_df$model)
  # for reporting results
  mouse_df = corr_df %>% filter(species == 'Mouse') %>% 
    mutate(has_kappa1 = str_detect(model1, 'kappa'), has_kappa2 = str_detect(model2, 'kappa')) %>% 
    filter(!has_kappa1) %>% filter(!has_kappa2)
  rat_df = corr_df %>% filter(species == 'Rat') %>% 
    mutate(has_kappa1 = str_detect(model1, 'kappa'), has_kappa2 = str_detect(model2, 'kappa')) %>% 
    filter(!has_kappa1) %>% filter(!has_kappa2)
  human_df = corr_df %>% filter(species == 'Human') %>% 
    mutate(has_kappa1 = str_detect(model1, 'kappa'), has_kappa2 = str_detect(model2, 'kappa')) %>% 
    filter(!has_kappa1) %>% filter(!has_kappa2)
  p = ggplot(corr_df, aes(x = model1, y = model2, fill = corr, color = corr)) + theme_classic(BASE_SIZE) +
    geom_tile() +
    #scale_fill_gradient(low = "white", high = "#887BB0", name = 'correlation') +
    scale_fill_gradient(low = "white", high = "#282120", name = 'Correlation') +
    scale_color_gradient(low = "white", high = "#282120", name = 'Correlation') +
    xlab("") + ylab("") + facet_wrap(~species) +
    theme(legend.position = 'right', axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0))
  return(p)
}