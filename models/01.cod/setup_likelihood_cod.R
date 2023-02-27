
gadgetlikelihood('likelihood_cod',gd$dir,missingOkay = TRUE) %>% 
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
                name = "ldist.cod.com",
                weight = 1,
                aggregationlevel = 1,
                data = ldist.com,
                fleetnames = "comcod",
                stocknames = stock_names) %>%
    
  gadget_update("catchdistribution",
                name = "ldist.cod.surQ2",
                weight = 1,
                data = ldist.survQ2,
                fleetnames = "survQ2cod",
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = "ldist.cod.surQ4",
                weight = 1,
                data = ldist.survQ4,
                fleetnames = "survQ4cod",
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = "alk.cod.surQ2",
                weight = 1,
                data = aldist.survQ2,
                fleetnames = "survQ2cod",
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = "alk.cod.surQ4",
                weight = 1,
                data = aldist.survQ4,
                fleetnames = "survQ4cod",
                stocknames = stock_names) %>%
    
  gadget_update("surveyindices",
                name = "siQ2.cod",
                weight = 1,
                data = si_spring,
                sitype = "lengths",
                biomass = 1,
                fittype = 'loglinearfit',
                stocknames = stock_names) %>%

  gadget_update("surveyindices",
                name = "siQ4.cod",
                weight = 1,
                data = si_fall,
                sitype = "lengths",
                biomass = 1,
                fittype = 'loglinearfit',
                stocknames = stock_names) %>%
                    
  write.gadget.file(gd$dir)
