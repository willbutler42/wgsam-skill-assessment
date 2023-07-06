## -------------------------------------------------------------------------------
##
## Runner to set up likelihoods
##
## -------------------------------------------------------------------------------

nll_breakdown <- TRUE  # Turn to TRUE to get per-step nll
lik_report <- TRUE

likelihood_actions <- list(
  g3l_understocking(stock_list, nll_breakdown = nll_breakdown, weight = 1e6),
  
  g3l_catchdistribution(
    nll_name = paste0('ldist.',species_name,'.com'),
    obs_data = ldist.com %>% mutate(step = as.numeric(step)),
    fleets = list(com),
    stocks = stock_list,
    function_f = g3l_distribution_sumofsquares(),
    area_group = mfdb::mfdb_group(noba_area = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    nll_name = paste0('ldist.',species_name,'.surQ1'),
    obs_data = ldist.survQ1 %>% mutate(step = as.numeric(step)),
    fleets = list(survQ1),
    stocks = stock_list,
    function_f = g3l_distribution_sumofsquares(),
    area_group = mfdb::mfdb_group(noba_area = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    nll_name = paste0('ldist.',species_name,'.surQ3'),
    obs_data = ldist.survQ3 %>% mutate(step = as.numeric(step)),
    fleets = list(survQ3),
    stocks = stock_list,
    function_f = g3l_distribution_sumofsquares(),
    area_group = mfdb::mfdb_group(noba_area = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_catchdistribution(
    nll_name = paste0('aldist.',species_name,'.surQ1'),
    obs_data = aldist.survQ1 %>% mutate(step = as.numeric(step)),
    fleets = list(survQ1),
    stocks = stock_list,
    function_f = g3l_distribution_sumofsquares(),
    area_group = mfdb::mfdb_group(noba_area = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  # g3l_catchdistribution(
  #   nll_name = paste0('alk.',species_name,'.surQ3'),
  #   obs_data = aldist.survQ3,
  #   fleets = list(survQ3),
  #   stocks = single_stock,
  #   function_f = g3l_distribution_sumofsquares(),
  #   nll_breakdown = nll_breakdown,
  #   report = lik_report),
  
  g3l_abundancedistribution(
    nll_name = paste0('siQ1.',species_name),
    obs_data = si_spring %>% rename(weight = total_weight) %>% mutate(step = as.numeric(step)),
    fleets = list(),
    stocks = stock_list,
    function_f = g3l_distribution_surveyindices_log(),
    ## function_f = g3l_distribution_surveyindices_log(beta = 1) # FOR A FIXED SLOPE
    area_group = mfdb::mfdb_group(noba_area = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),

  g3l_abundancedistribution(
    nll_name = paste0('siQ3.',species_name),
    obs_data = si_fall %>% rename(weight = total_weight) %>% mutate(step = as.numeric(step)),
    fleets = list(),
    stocks = stock_list,
    function_f = g3l_distribution_surveyindices_log(),
    ## function_f = g3l_distribution_surveyindices_log(beta = 1) # FOR A FIXED SLOPE
    area_group = mfdb::mfdb_group(noba_area = 1),
    nll_breakdown = nll_breakdown,
    report = lik_report),
  
  list()
  )
    


