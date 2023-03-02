
gadgetlikelihood(paste0('likelihood_',species_name),gd$dir,missingOkay = TRUE) %>% 
  ## Write a penalty component to the likelihood file
  gadget_update("penalty",
                name = "bounds",
                weight = "10",
                data = data.frame(
                  switch = c("default"),
                  power = c(2),
                  upperW=10000,
                  lowerW=10000,
                  stringsAsFactors = FALSE)) %>%
  gadget_update("understocking",
                name = "understocking",
                weight = "10") %>%
                    
  gadget_update("catchdistribution",
                name = paste0('ldist.',species_name,'.com'),
                weight = 1,
                aggregationlevel = 1,
                data = ldist.com,
                fleetnames = paste0('com',species_name),
                stocknames = stock_names) %>%
    
  gadget_update("catchdistribution",
                name = paste0('ldist.',species_name,'.surQ2'),
                weight = 1,
                data = ldist.survQ2,
                fleetnames = paste0('survQ2',species_name),
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = paste0('ldist.',species_name,'.surQ4'),
                weight = 1,
                data = ldist.survQ4,
                fleetnames = paste0('survQ4',species_name),
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = paste0('alk.',species_name,'.surQ2'),
                weight = 1,
                data = aldist.survQ2,
                fleetnames = paste0('survQ2',species_name),
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = paste0('alk.',species_name,'.surQ4'),
                weight = 1,
                data = aldist.survQ4,
                fleetnames = paste0('survQ4',species_name),
                stocknames = stock_names) %>%
    
  gadget_update("surveyindices",
                name = paste0('siQ2.',species_name),
                weight = 1,
                data = si_spring,
                sitype = "lengths",
                biomass = 1,
                fittype = 'loglinearfit',
                ## fittype = 'fixedslopeloglinearfit',
                ## slope = 1,
                stocknames = stock_names) %>%

  gadget_update("surveyindices",
                name = paste0('siQ4.',species_name),
                weight = 1,
                data = si_fall,
                sitype = "lengths",
                biomass = 1,
                fittype = 'loglinearfit',
                ## fittype = 'fixedslopeloglinearfit',
                ## slope = 1,
                stocknames = stock_names) %>%
                    
  write.gadget.file(gd$dir)
