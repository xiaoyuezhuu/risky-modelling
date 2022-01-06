# All functions that retrieve and preprocess data go here

get_training_trials = function(){
  # get all risky sessions, including the training sessions
  df = read.csv('figures/csv/preprocessed_animals.csv')
  subj_list = unique(df$subjid)
  con = elConnect()
  all_df = data.frame()
  for (subjid in subj_list){
    sqlquery = sprintf("select * from proto.riskychoice_view where subjid = %d", subjid)
    all_df = rbind(all_df, suppressWarnings(dbGetQuery(con, sqlquery)))
  }
  all_df = preprocess_risk_(all_df)
  dbDisconnect(con)
  write.csv(all_df, file = 'figures/csv/all_training_trials.csv')
  return(all_df)
}

get_training_days = function(){
  # get the first date all animals in good_sessions entered into any risky choice stage
  con = elConnect()
  sqlquery = "SELECT subjid, sessid, min(sessiondate) AS first_date FROM beh.sessions 
              WHERE subjid > 1000 AND protocol = 'riskychoice' 
              GROUP BY subjid"
  first_df = suppressWarnings(dbGetQuery(con, sqlquery))
  dbDisconnect(con)
  # now merge it only with the first good session date
  gs_df = read.csv('figures/csv/preprocessed_animals.csv') %>% 
    group_by(subjid) %>% arrange(sessid) %>% summarise(first_good_session = sessiondate[1])
  df = merge(gs_df, first_df, by = 'subjid')
  df$n_training_days = abs(as.Date(df$first_date) - as.Date(df$first_good_session))
  write.csv(df, file = 'figures/csv/trainig_days.csv')
  return(df)
}

get_pre_training_days = function(){
  # get a list of n_days until the animals start to give good sessions and plot
  out = DBI::dbGetQuery(con, 'SELECT b.subjid, datearrived, min(sessiondate), max(sessiondate), datediff(min(sessiondate), datearrived) as n_days FROM risk.good_sessions AS a 
                            JOIN beh.sessview AS b
                            JOIN met.animals AS c
                            ON a.sessid = b.sessid AND b.subjid = c.subjid 
                            GROUP BY b.subjid')
  out = out %>% mutate(species = case_when(subjid > 2000 ~ 'rat',
                                           subjid < 2000 ~ 'mouse')) %>% 
    filter(n_days < 400) %>% 
    mutate(n_days = n_days - 10)
  mouse_df = out %>% filter(species == 'mouse')
  rat_df = out %>% filter(species == 'rat')
  p1 = ggplot(mouse_df, aes(x = as.factor(subjid), y = n_days)) + 
    theme_classic() + geom_col() + ylab('Days of training') + 
    geom_hline(yintercept = mean(mouse_df$n_days)) +
    theme(axis.text.x = element_text(angle = -30, vjust = 0),
          axis.title.x = element_blank())
  p2 = ggplot(rat_df, aes(x = as.factor(subjid), y = n_days)) + 
    theme_classic() + geom_col() +  ylab('Days of training') + 
    geom_hline(yintercept = mean(rat_df$n_days)) +
    theme(axis.text.x = element_text(angle = -30, vjust = 0), 
          axis.title.x = element_blank())
  p = p1 / p2
  return(p)
}

get_settings = function(){
  # get subject-specific task settings
  con = elConnect()
  sqlquery = "SELECT * FROM met.subject_settings WHERE settings_date IN 
            (SELECT max(settings_date) AS settings_date FROM met.subject_settings WHERE settings_date < '2021-01-01'
            GROUP BY subjid)"
  settings_df = suppressWarnings(dbGetQuery(con, sqlquery))
  dbDisconnect(con)
  gs_df = read.csv('figures/csv/preprocessed_animals.csv')
  settings_df = settings_df %>% filter(subjid %in% unique(gs_df$subjid)) %>% group_by(subjid) %>% 
    arrange(desc(settings_date)) %>% filter(row_number()==1)
  # extract information for each subject
  new_settings_df = settings_df
  new_settings_df$lottery_side = 9
  new_settings_df$increase = 9
  new_settings_df$freq_or_click = 9
  for (i in 1:nrow(settings_df)){
    x = rjson::fromJSON(settings_df$settings_data[i])
    if ('subject_lottery_side' %in% names(x$vals)){
      new_settings_df$lottery_side[i] = x$vals$subject_lottery_side
    }
    if ('increase_or_decrese' %in% names(x$vals)){
      new_settings_df$increase[i] = str_detect(x$vals$increase_or_decrease, 'increase')
    }
    if ('subject_lottery_generator' %in% names(x$vals)){
      new_settings_df$increase[i] = str_detect(x$vals$subject_lottery_generator, 'increase')
      new_settings_df$freq_or_click[i] = str_sub(x$vals$subject_lottery_generator, 9)
    }
  }
  write.csv(new_settings_df, file = 'figures/csv/settings.csv')
  return(new_settings_df)
}

get_subjects = function(){
  # get subject-specific task settings
  con = elConnect()
  sqlquery = "SELECT * FROM met.animals WHERE subjid > 1000"
  subjects_df = suppressWarnings(dbGetQuery(con, sqlquery))
  dbDisconnect(con)
  gs_df = read.csv('figures/csv/preprocessed_animals.csv')
  subjects_df = subjects_df %>% filter(subjid %in% unique(gs_df$subjid))
  write.csv(subjects_df, file = 'figures/csv/subjects.csv')
  return(subjects_df)
}

get_good_sessions = function(){
  # get trials from all the good sessions from risk.good_sessions
  con = elConnect()
  sqlquery = sprintf('select sessid from risk.good_sessions')
  sessids = suppressWarnings(dbGetQuery(con, sqlquery))$sessid
  # get subjid and date info
  sessid_str = reduce(sessids,function(s1,s2){paste(s1,",",s2)})
  sqlquery = sprintf('select * from beh.sessions where sessid in (%s)', sessid_str)
  sess_df = suppressWarnings(dbGetQuery(con, sqlquery))
  # get all the trials within these sessions
  sqlquery = sprintf("select * from proto.riskychoice_view where sessid in ( %s ) order by subjid, sessid, trialnum", sessid_str)
  trials_df = suppressWarnings(dbGetQuery(con, sqlquery))
  dbDisconnect(con)
  # preprocess a little bit
  df = merge(trials_df, sess_df, by = c('sessid', 'subjid'), all = TRUE)
  df = df %>% group_by(subjid) %>% add_tally() %>% ungroup() %>% 
    filter(n >= 1000) %>% # only use subjects with more than 1000 trials
    mutate(forced = is.na (lottery_poke) | is.na(surebet_poke),
           is_side_choice = is.na (lottery_poke) & is.na(surebet_poke),
           version = 1, # keep consistency with human data
           species_name = case_when(subjid > 1000 & subjid < 2000 ~ 'mouse', 
                                    subjid > 2000 ~ 'rat'),
           lottery_outcome = case_when(lottery_outcome == 'win' ~ 1,
                                       lottery_outcome == 'lose' ~ 0)) %>% 
    filter(is_side_choice == 0) %>% 
    select('sessiondate', 'species_name', 'subjid', 'sessid', 'trialnum', 'forced', 'viol', 'lottery_outcome',
           'lottery_mag', 'lottery_prob', 'sb_mag',
           'subj_choice', 'reward_received', 'RT', 'version')
  write.csv(df, 'figures/csv/good_sessions.csv')
  return(df)
}

get_human_trials = function(version = NULL){
  # get human trials from Jenya's Pav database
  con = elConnect()
  sqlquery = "select * from pav.sessions"
  sess_df = suppressWarnings(dbGetQuery(con, sqlquery))
  sqlquery = sprintf("select * from pav.riskychoice_view_pav_nv")
  trials_df = suppressWarnings(dbGetQuery(con, sqlquery))
  df = merge(sess_df, trials_df, by = c('sessid', 'subjid', 'version'))
  dbDisconnect(con)
  # preprocess a bit to be compatible with animal data
  df = df %>% arrange(subjid, sessid, trialid) %>% 
    mutate(forced = 0, viol = 0, # keep consistencey with animal's data
           species_name = case_when(subjid > 4000 & subjid < 5000 ~ 'verbal', 
                                    subjid > 5000 ~ 'nonverbal')) %>%
    rename(reward_received = reward) %>% # keep consistency
    mutate(choice = subj_choice,
           subj_choice = case_when(subj_choice == 1 ~ 'lottery',
                                   subj_choice == 0 ~ 'surebet')) # keep consistency
  # filter out really bad subjects
  nonverbal_df = df %>% group_by(subjid) %>% add_tally %>% 
    mutate(n_lotteries = length(unique(lottery_mag)), # create filter columns
           fraction_chose_lottery = mean(choice)) %>% ungroup() %>% 
    filter(n_lotteries > 4 & fraction_chose_lottery > 0.1 & fraction_chose_lottery < 0.9) %>% 
    filter(n >= 50) %>% ungroup() %>% 
    select('sessiondate', 'species_name', 'subjid', 'sessid', 'trialnum', 'forced', 'viol', 'lottery_outcome',
           'lottery_mag', 'lottery_prob', 'sb_mag',
           'subj_choice', 'reward_received', 'RT', 'version')
  write.csv(nonverbal_df, 'figures/csv/nonverbal.csv')
}

preprocess_risk_ = function(raw_df, remove_forced = TRUE, remove_slow = TRUE){
  # preprocess risk trials into separate fitting-friendly species dataframe
  df = raw_df %>% 
    mutate(forced = is.na (lottery_poke) | is.na(surebet_poke),
           is_side_choice = is.na (lottery_poke) & is.na(surebet_poke),
           version = 1, # keep consistency with human data
           species_name = case_when(subjid > 1000 & subjid < 2000 ~ 'mouse', 
                                    subjid > 2000 ~ 'rat'),
           lottery_outcome = case_when(lottery_outcome == 'win' ~ 1,
                                       lottery_outcome == 'lose' ~ 0)) %>% 
    group_by(sessid) %>% # find session-specific total_rew_multiplier 
    mutate(total_rew_multi = case_when(subj_choice == 'violation' ~ 0,
                                       subj_choice == 'lottery' ~ 0,
                                       subj_choice == 'surebet' ~ reward_received / sb_mag)) %>% 
    mutate(total_rew_multi = sort(unique(total_rew_multi))[2]) %>% ungroup() %>% 
    mutate(prev_reward = lag(reward_received)) %>%  # add previous reward
    mutate(delta_ev = (total_rew_multi * lottery_mag * lottery_prob) - (total_rew_multi * sb_mag)) %>%
    mutate(prev_viol = lag(viol)) %>% 
    #filter(prev_viol != 1) %>% # remove trials whose previous trial is violation
    mutate(choice = case_when(subj_choice == 'lottery' ~ 1,
                              subj_choice == 'surebet' ~ 0,
                              subj_choice == 'violation' ~ 9)) %>% # encode subject choice
    filter(subj_choice != 'violation') %>% # remove violation trials
    mutate(prev_choice = lag(choice)) %>% # add previous choice
    mutate(prev_outcome_s = case_when(prev_reward > 0 & prev_choice == 1 ~ 'lottery_win',
                                      prev_reward == 0 & prev_choice == 1 ~ 'lottery_lose',
                                      prev_choice == 0 ~ 'surebet',
                                      prev_choice == 9 ~ 'surebet')) %>% # previous violation trials count as surebet
    mutate(prev_outcome = case_when(prev_outcome_s == 'lottery_win' ~ 1,
                                    prev_outcome_s == 'lottery_lose' ~ -1,
                                    prev_outcome_s == 'surebet' ~ 0)) %>%  # add previous outcome code
    filter(trialnum != 1) %>% # remove the first trials
    filter(!is.na(prev_choice)) %>% # remove any other trials contain NA
    filter(!is.na(prev_outcome))
  if (remove_forced){
    df = df %>% filter(forced == 0) # remove forced trials
  }
  if (remove_slow){
    df = df %>% filter(RT < 3) # remove slow trials, assuming no attention is paid to this trial
  }
  #df$prev_outcome_s = factor(df$prev_outcome_s, levels = c('lottery_win', 'surebet', 'lottery_lose'))
  return(df)
}

preprocess_risk = function(remove_forced = TRUE, remove_slow = TRUE){
  # preprocess risk trials into separate fitting-friendly species dataframe
  gs_df = read.csv('figures/csv/good_sessions.csv')
  nonverbal_df = read.csv('figures/csv/nonverbal.csv')
  df = rbind.data.frame(gs_df, nonverbal_df)
  mouse_list = sort(unique(df$subjid[df$species_name == 'mouse']))
  rat_list = sort(unique(df$subjid[df$species_name == 'rat']))
  nonverbal_list = sort(unique(df$subjid[df$species_name == 'nonverbal']))
  which_array = function(subjid){
    # helper function to get individual index per species
    if (subjid %in% mouse_list){
      subj_list = mouse_list
    } else if (subjid %in% rat_list){
      subj_list = rat_list
    } else if (subjid %in% nonverbal_list){
      subj_list = nonverbal_list
    }
    return(which(subj_list == subjid))
  }
  df = df %>% group_by(sessid) %>% # find session-specific total_rew_multiplier 
    mutate(total_rew_multi = case_when(subj_choice == 'violation' ~ 0,
                                       subj_choice == 'lottery' ~ 0,
                                       subj_choice == 'surebet' ~ reward_received / sb_mag)) %>% 
    mutate(total_rew_multi = sort(unique(total_rew_multi))[2]) %>% ungroup() %>% 
    arrange(subjid, sessid, trialnum) %>%
    mutate(species = case_when(species_name == 'mouse' ~ 1,
                               species_name == 'rat' ~ 2,
                               species_name == 'nonverbal' ~ 3), # add species index
           individual = unlist(lapply(subjid, which_array))) %>% # add individual index
    rename(reward = reward_received) %>% 
    mutate(prev_reward = lag(reward)) %>%  # add previous reward
    mutate(delta_ev = (total_rew_multi * lottery_mag * lottery_prob) - (total_rew_multi * sb_mag)) %>%
    mutate(prev_viol = lag(viol)) %>% 
    #filter(prev_viol != 1) %>% # remove trials whose previous trial is violation
    mutate(choice = case_when(subj_choice == 'lottery' ~ 1,
                              subj_choice == 'surebet' ~ 0,
                              subj_choice == 'violation' ~ 9)) %>% # encode subject choice
    filter(subj_choice != 'violation') %>% # remove violation trials
    mutate(prev_choice = lag(choice)) %>% # add previous choice
    mutate(prev_outcome_s = case_when(prev_reward > 0 & prev_choice == 1 ~ 'lottery_win',
                                      prev_reward == 0 & prev_choice == 1 ~ 'lottery_lose',
                                      prev_choice == 0 ~ 'surebet',
                                      prev_choice == 9 ~ 'surebet')) %>% # previous violation trials count as surebet
    mutate(prev_outcome = case_when(prev_outcome_s == 'lottery_win' ~ 1,
                                    prev_outcome_s == 'lottery_lose' ~ -1,
                                    prev_outcome_s == 'surebet' ~ 0)) %>%  # add previous outcome code
    filter(trialnum != 1) %>% # remove the first trials
    filter(!is.na(prev_choice)) %>% # remove any other trials contain NA
    filter(!is.na(prev_outcome))
  if (remove_forced){
    df = df %>% filter(forced == 0) # remove forced trials
  }
  if (remove_slow){
    df = df %>% filter(RT < 3) # remove slow trials, assuming no attention is paid to this trial
  }
  df$prev_outcome_s = factor(df$prev_outcome_s, levels = c('lottery_win', 'surebet', 'lottery_lose'))
  write.csv(df, 'figures/csv/preprocessed_all.csv')
  save_by_species(df)
  return(df)
}

save_by_species = function(all_df){
  # save preprocessed data by species for species-only fitting
  df = all_df %>% filter(species_name == 'mouse')
  write.csv(df, 'figures/csv/preprocessed_mouse.csv')
  df = all_df %>% filter(species_name == 'rat')
  write.csv(df, 'figures/csv/preprocessed_rat.csv')
  df = all_df %>% filter(species_name != 'nonverbal')
  write.csv(df, 'figures/csv/preprocessed_animals.csv')
  df = all_df %>% filter(species_name == 'nonverbal')
  write.csv(df, 'figures/csv/preprocessed_nonverbal.csv')
}

get_elpd_csv = function(){
  # get the trial-number-averaged elpd for each subject
  models_list = get_kfold_models()
  elpd_df = data.frame()
  for (model in models_list){
    this_df = read.csv(sprintf('figures/csv/kfold/%s_kfold.csv', model)) %>% mutate(model = model) %>% rename(elpd_se = elpd.1)
    elpd_df = rbind(elpd_df, this_df)
  }
  elpd_df = elpd_df %>% mutate(species = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat'))) %>% 
    mutate(elpd_min = elpd - elpd_se, elpd_max = elpd + elpd_se)
  # df = read.csv('figures/csv/preprocessed_all.csv')
  # bino_df = binomialize(df, individual = FALSE, history = TRUE)
  # bino_df = bino_df %>% group_by(subjid) %>% tally() # get the number of trial types for each subject, history & non-history differs!
  # elpd_df = merge(bino_df, elpd_df, by = 'subjid') %>% 
  #   mutate(elpd = elpd / n, # divide elpd by the number of trial types
  #          species = ifelse(subjid > 5000, 'Human', ifelse(subjid < 2000, 'Mouse', 'Rat'))) %>% 
  #   group_by(subjid, species, model) %>% 
  #   summarise(elpd_mean = mean(elpd), elpd_min = elpd_mean - se(elpd), elpd_max = elpd_mean + se(elpd)) # get mean and CI for each subject
  write.csv(elpd_df, 'figures/csv/elpd.csv')
  return(elpd_df)
}

get_most_freq_csv = function(){
  # filter by the most frequent task parameter except for humans
  df = read.csv('figures/csv/preprocessed_animals.csv')
  df2 = data.frame()
  for (subj in unique(df$subjid)){
    subj_df = df %>% filter(subjid == subj)
    subj_df = subj_df %>% filter(lottery_prob == get_freq_task_param(subj_df, 'lottery_prob'), 
                                 sb_mag == get_freq_task_param(subj_df, 'sb_mag'), 
                                 total_rew_multi == get_freq_task_param(subj_df, 'total_rew_multi'))
    df2 = rbind(df2, subj_df)
  }
  human_df = read.csv('figures/csv/preprocessed_nonverbal.csv')
  df2 = rbind(df2, human_df)
  df2 = df2 %>% mutate(species_name = case_when(species_name == 'mouse' ~ 'Mouse', 
                                                species_name == 'rat' ~ 'Rat', 
                                                species_name == 'nonverbal' ~ 'Human'))
  write.csv(df2, 'figures/csv/preprocessed_all_freq.csv')
  return(df2)
}

get_most_freq_pred_csv = function(model = 'rho-sigma'){
  # filter by the most frequent task parameter from model prediction for a smooth ribbon plot
  df = read.csv('figures/csv/freq_task_params.csv')
  pred_df = read.csv(sprintf('figures/csv/pred/%s_pred.csv', model)) %>% 
    mutate(species = ifelse(subjid > 5000, 'Human', ifelse(subjid > 2000, 'Rat', 'Mouse')))
  df2 = data.frame()
  for (subj in unique(df$subjid)){
    subj_df = df %>% filter(subjid == subj)
    temp_pred_df = pred_df %>% filter(pred_subjid == subj,
                                      pred_lottery_prob == subj_df$lottery_prob,
                                      pred_sb_mag == subj_df$sb_mag,
                                      pred_total_rew_multi == subj_df$total_rew_multi)
    df2 = rbind(df2, temp_pred_df)
  }
  write.csv(df2, sprintf('figures/csv/pred/%s_pred_freq.csv', model))
  return(df2)
}
