 # a list of functions related to reporting stats, making tables etc.

first_order_viol = function(species = 'mouse'){
  # calculate first order violations
  df = read.csv(sprintf('figures/csv/preprocessed_%s.csv', species))
  x_df = df %>% group_by(subjid) %>% 
    mutate(first_viol = case_when(lottery_mag <= sb_mag & choice == 1 ~ 1,
                                  lottery_mag <= sb_mag & choice == 0 ~ 0,
                                  lottery_mag >= sb_mag  ~ 0)) %>% filter(lottery_mag <= sb_mag) %>% 
    add_tally() %>% 
    summarise(n_first_viol = sum(first_viol), n_trials = mean(n), pcg_first_viol = n_first_viol / n_trials)
  return(mean(x_df$pcg_first_viol))
}

make_settings_table = function(){
  # table for subject lottery side, increase or decrease, clicks or frequency 
  settings_df = read.csv('figures/csv/settings.csv')
  return(settings_df %>% group_by(freq_or_click, increase, lottery_side) %>% tally())
}

make_subjects_table = function(){
  # table for subject species, strain and gender 
  subjects_df = read.csv('figures/csv/subjects.csv')
  return(subjects_df %>% group_by(species, strain, gender) %>% tally())
}

fit_history_glm = function(){
  species = c('mouse', 'rat', 'nonverbal')
  for (ss in species){
    df = read.csv(sprintf('figures/csv/preprocessed_%s.csv', ss)) %>% group_by(subjid) %>% 
      sample_n(3000, replace = TRUE)
    df$prev_outcome_s = factor(df$prev_outcome_s, c('surebet', 'lottery_win', 'lottery_lose'))
    #m0 = glm('choice ~ delta_ev*prev_outcome_s', df, family = binomial)
    m1 = glmer('choice ~ delta_ev + prev_outcome_s + (delta_ev + prev_outcome_s | subjid)', df, family = binomial)
    save(m1, file = sprintf('figures/%s_history_GLMM.RData', ss))
  }
}