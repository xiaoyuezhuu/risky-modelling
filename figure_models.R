# figure_models: illustration of the different kinds of models we fit
make_figure_models = function(){
  p = rho_example()
  scale_save(p, 'figure_models_a', 6, 6, 1)
  p = kappa_example()
  scale_save(p, 'figure_models_b', 6, 6, 1)
  p = noise_example('nonscalar')
  scale_save(p, 'figure_models_c', 8, 6, 1)
  p = noise_example('scalar')
  scale_save(p, 'figure_models_d', 8, 6, 1)
}

rho_example = function(){
  # Use examples to illustrate how changing the risk preference parameter can change the expected utility function
  df = data.frame(x = c(1:1000)) %>% mutate(y = x^0.4)
  p = ggplot(df, aes(x = x, y = y)) + theme_classic(BASE_SIZE) +
    #scale_y_continuous(limits = c(0, 10)) +
    geom_line() + xlab('Reward') + ylab('Utility') +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(), 
          axis.ticks = element_blank())
  return(p)
}

kappa_example = function(){
  # Use examples to illustrate how changing the risk preference parameter can change the expected utility function
  #  variance is (1-P)PV*V, vs. lottery offer
  # df = data.frame(x = c(-100:100)) %>% mutate(y = -x^2)
  # p = ggplot(df, aes(x = x, y = y)) + theme_classic(BASE_SIZE) +
  #   scale_x_continuous(breaks = c(-100, 0, 100), labels = c(0, 0.5, 1)) +
  #   geom_line() + xlab('Lottery Probability') + ylab('Variance') +
  #   geom_vline(xintercept = 0.5, linetype = 'dashed') +
  #   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  df = data.frame(lottery_mag = seq(1, 32, 1), lottery_prob = 0.5) %>% 
    mutate(variance = (1 - lottery_prob)*lottery_prob*lottery_mag*lottery_mag)
  p = ggplot(df, aes(x = lottery_mag, y = variance)) + theme_classic(BASE_SIZE) +
    geom_line() + xlab('Lottery mag') + ylab('Variance') +
    theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  return(p)
}

noise_example = function(type = 'nonscalar'){
  # noise examples: scalar or nonscalar
  lott_mags = c(2, 4, 8)
  sigma = 0.4
  df = data.frame()
  for (lott_mag in lott_mags){
    if (type == 'nonscalar'){
      temp_df = data.frame(x = seq(0, 16, by = 0.1), y = dnorm(seq(0, 16, by = 0.1), lott_mag, sigma), lott_mag = lott_mag, sigma = sigma)
    } else{
      temp_df = data.frame(x = seq(0, 16, by = 0.1), y = dnorm(seq(0, 16, by = 0.1), lott_mag, sigma*lott_mag), lott_mag = lott_mag, sigma = sigma*lott_mag)
    }
    df = rbind(df, temp_df)
  }
  p = ggplot(df, aes(x = x, y = y, color = as.factor(lott_mag))) + theme_classic(BASE_SIZE) +
    geom_line(size = 1) + xlab('u(x)') + ylab('Density of u(x)') +
    geom_vline(mapping = aes(xintercept = lott_mag), linetype = 'dashed') +
    scale_x_continuous(breaks = lott_mags, limits = c(0, 10)) +
    #scale_color_manual(values = c("#4683C4", "#709493", "#a8ac52", "#d2bd21", "#eec900")) +
    #scale_color_manual(values = c("#4683C4", "#a8ac52", "#eec900")) +
    scale_color_manual(values = c('black', 'black', 'black')) +
    theme(legend.position = 'none', axis.text.y = element_blank(), axis.ticks.y = element_blank())
 return(p)   
}