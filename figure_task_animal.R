# figure_task. The risky choice task and example behavior
make_figure_task = function(){
  p = sessions_v_trials()
  scale_save(p, 'figure_task_c', 8, 8, 1)
  p = viols_v_profit()
  scale_save(p, 'figure_task_d', 8, 8, 1)
  #p = profit_v_mass()
  #scale_save(p, 'figure_task_profits_v_mass', 12, 8, 1)
  viols_v_offer() # saves on its own
  p = plot_population_psychometric('mouse')
  scale_save(p, 'figure_behavior_a_mouse', 8, 8, 1)
  p = plot_population_psychometric('rat')
  scale_save(p, 'figure_behavior_a_rat', 8, 8, 1)
}

sessions_v_trials = function(){
  # figure1b: Scatter plot of # of sessions vs. average trials / session colored by species, shape by sex
  df = read.csv('figures/csv/preprocessed_animals.csv')
  subj_df1 = df %>% group_by(subjid) %>% tally() %>% rename(num_trials = n)
  subj_df2 = df %>% group_by(subjid, sessid) %>% summarise() %>% 
    group_by(subjid) %>% tally() %>% rename(num_sessions = n)
  subj_df = merge(subj_df1, subj_df2, by = 'subjid') %>% 
    mutate(avg_trials = num_trials / num_sessions,
           species = ifelse(subjid > 2000, 'Rat', 'Mouse'))
  p = ggplot(subj_df, aes(x = num_sessions, y = avg_trials, fill = species, color = species)) + 
    #geom_smooth(method = lm, formula = y ~ x, size = 0.2) +
    theme_classic(base_size = BASE_SIZE) + geom_point(shape = 21, size = 2, color = 'black') + 
    xlab("# Sessions") + ylab("Avg # Trials") +
    scale_fill_manual(values = ANIMAL_COLORS) +
    scale_color_manual(values = ANIMAL_COLORS) +
    theme(legend.position = 'none')
  return(p)
}

viols_v_profit = function(){
  # figure1c: Scatter plot of average % violation vs. average profit colored by species, shape by sex
  df = read.csv('figures/csv/good_sessions.csv')
  viol_df = df %>% group_by(subjid) %>% add_tally() %>% 
    summarise(num_trials = mean(n), n_viol = sum(viol)) %>% mutate(viol_Rate = n_viol / num_trials)
  profit_df = df %>% group_by(subjid, sessid) %>% summarise(total_profit = sum(reward_received)) %>% 
    group_by(subjid) %>% summarise(avg_profit = mean(total_profit))
  plot_df = merge(viol_df, profit_df, by = 'subjid') %>% 
    mutate(species = ifelse(subjid > 2000, 'Rat', 'Mouse'))
  p = ggplot(plot_df, aes(x = viol_Rate*100, y = avg_profit/1000, fill = species, color = species)) + 
    #geom_smooth(method = lm, formula = y ~ x, size = 0.2) +
    theme_classic(base_size = BASE_SIZE) + geom_point(shape = 21, size = 2, color = 'black') + 
    xlab("Avg % Violation") + ylab("Avg Profit (mL)") +
    scale_y_continuous(limits = c(0, 10)) + 
    scale_fill_manual(values = ANIMAL_COLORS) +
    scale_color_manual(values = ANIMAL_COLORS) +
    theme(legend.position = 'none')
  return(p)
}

profit_v_mass = function(){
  # figure1d: Scatter plot of average profit vs. average mass colored by species, shape by sex
  con = elConnect()
  sqlquery = "SELECT subjid, AVG(mass) as avg_mass, AVG(total_profit)/1000 AS avg_profit FROM beh.sessview WHERE sessid IN (SELECT sessid FROM risk.good_sessions) GROUP BY subjid"
  mass_df = suppressWarnings(dbGetQuery(con, sqlquery)) 
  dbDisconnect(con)
  mass_df = mass_df %>% mutate(species = ifelse(subjid > 2000, 'Rat', 'Mouse')) %>% filter(subjid != 2154)
  ss = c('Mouse', 'Rat')
  for (i in 1:2){
    s = ss[i]
    p = ggplot(mass_df %>% filter(species == s), aes(x = avg_mass, y = avg_profit)) + 
      geom_smooth(method = lm, formula = y ~ x, size = 0.2, color = ANIMAL_COLORS[i], fill = ANIMAL_COLORS[i]) +
      theme_classic(base_size = BASE_SIZE) + geom_point(shape = 21, size = 2, fill = ANIMAL_COLORS[i], color = 'black') + 
      ylab("Average profit (mL)") + xlab("Average mass (g)") +
      theme(legend.position = 'bottom')
    if (s == 'Mouse'){
      p = p + scale_x_continuous(breaks = c(20, 40), limits = c(19, 42)) +
        scale_y_continuous(breaks = c(1, 3, 5))
      P = p} 
    else {
      p = p + scale_x_continuous(breaks = c(200, 800), limits = c(180, 820)) +
        scale_y_continuous(breaks = c(1, 5, 10), limits = c(0, 11)) + 
        theme(axis.title.x = element_blank(), axis.title.y = element_blank()) 
      P = P + p}
  }
  return(P)
}

viols_v_offer = function(){
  # scatter plot of % violation vs. lottery magnitude 
  ss = c('Mouse', 'Rat')
  yy = c(4.5, 1.5)
  df = read.csv('figures/csv/good_sessions_viol.csv')
  df = df %>% mutate(choice_viol = case_when(RT_fixation < 300 & is.na(RT_choice) ~ 1,
                                             RT_fixation == 300 | is.na(RT_fixation) ~ 0, 
                                             !is.na(RT_choice) ~ 0))
  for (i in 1:2){
    this_df = df %>% filter(species_name == ss[i])
    viol_df = this_df %>% group_by(subjid, lottery_mag) %>% add_tally() %>% 
      summarise(viol_rate = bino(choice_viol)$y, viol_min = bino(choice_viol)$ymin, viol_max = bino(choice_viol)$ymax)
    
    m1 = glm('choice_viol ~ lottery_mag', data = this_df, family = 'binomial')
    pred_df = data.frame(lottery_mag = unique(viol_df$lottery_mag))
    preds = predict(m1, pred_df, se.fit = TRUE, type = 'response')
    pred_df$pred = preds$fit
    pred_df$se = preds$se.fit
    cf = coef(summary(m1))
    p = ggplot(viol_df) + theme_classic(base_size = BASE_SIZE) + 
      xlab('Lottery magnitude') + ylab("% Opt-out") +
      scale_x_continuous(breaks = c(0, 2, 4, 8, 16, 32)) + 
      annotate('text', label = sprintf("%.3f, p < 0.001", cf['lottery_mag', 'Estimate']), x = 16, y = yy[i]) + 
      theme(legend.position = 'none')
    subj_list = unique(df$subjid)
    for (subj in subj_list){
      p = p + geom_line(viol_df %>% filter(subjid == subj), mapping = aes(x = lottery_mag, y = viol_rate*100), color = ANIMAL_COLORS[[i]], alpha = 0.6)
    }
    p = p + geom_line(pred_df, mapping = aes(x = lottery_mag, y = pred*100), color = 'black', size = 1) +
      geom_errorbar(pred_df, mapping = aes(x = lottery_mag, y = pred*100, ymin = (pred - se)*100, ymax = (pred + se)*100), color = 'black', size = 1)
    if (ss[i] == 'Mouse'){
      p = p + scale_y_continuous(limits = c(0, 5), breaks = c(0, 2.5, 5))
    }
    scale_save(p, sprintf('%s_opt-out', ss[i]), 8, 8, 1)
  }
}

plot_population_psychometric = function(species = 'mouse'){
  # Population psychometric curves by GLMM, thin colored lines per subject, thick line for population
  df = read.csv(sprintf('figures/csv/preprocessed_%s.csv', species))
  # GLMM fit
  m0 = glm('choice ~ delta_ev', df, family = binomial)
  fname = sprintf("%s_glmm.RData", species)
  if (file.exists(sprintf('figures/%s', fname))){
    load(sprintf('figures/%s', fname))
  } else{
    m1 = glmer('choice ~ delta_ev + (delta_ev | subjid)', df, family = binomial)
    save(m1, file = sprintf('figures/%s', fname))
  }
  
  pop_pred_df = data_grid(df, delta_ev = seq_range(c(min(delta_ev)*1.3, max(delta_ev)*1.1), by = 1))
  ind_pred_df = data_grid(df, delta_ev = seq_range(c(min(delta_ev)*1.3, max(delta_ev)*1.1), by = 1), subjid = subjid)
  pop_pred_df$pred = predict(m0, pop_pred_df, type = 'response', allow.new.levels=TRUE)
  ind_pred_df$pred = predict(m1, ind_pred_df, type = 'response', allow.new.levels=TRUE)
  p = ggplot(df) + theme_classic(base_size = BASE_SIZE) +
    geom_hline(yintercept = 0.5, linetype = 'dashed', alpha = 0.4) +
    geom_vline(xintercept = 0, linetype = 'dashed',  alpha = 0.4) +
    scale_y_continuous(limits = c(-0.02, 1.02), breaks = c(0, 0.5, 1)) +
    scale_x_continuous(breaks = c(round(min(df$delta_ev)/10)*10, 0, round(max(df$delta_ev)/10)*10)) +
    annotate("text", label = sprintf('%d trials \n %d sessions \n %d subjects', dim(df)[1], length(unique(df$sessid)), length(unique(df$subjid))), x = max(df$delta_ev)*0.65, y = 0.15, size = ANNOTATION_SIZE) +
    xlab(expression(EV[lottery]-EV[surebet])) + ylab("P(Chose Lottery)")
  for (i in 1:length(unique(df$subjid))){
    subjidx = unique(df$subjid)[i]
    p = p + geom_line(ind_pred_df %>% filter(subjid == subjidx),
                      mapping = aes(x = delta_ev, y = pred), color = SPECIES_COLORS[[species]], alpha = 0.3)
  }
  p = p + geom_line(pop_pred_df, mapping = aes(x = delta_ev, y = pred), color = SPECIES_COLORS[[species]], alpha = 0.6, size = 2)
  return(p)
}