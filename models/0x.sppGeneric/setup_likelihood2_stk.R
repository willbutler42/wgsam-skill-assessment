
gadgetlikelihood('likelihood_had',gd$dir,missingOkay = TRUE) %>% 
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
                name = "adist.had.com",
                weight = 1,
                aggregationlevel = 1,
                data = adist.com,
                fleetnames = "comhad",
                stocknames = stock_names) %>%
    
  gadget_update("catchdistribution",
                name = "adist.had.surQ2",
                weight = 1,
                data = adist.survQ2,
                fleetnames = "survQ2had",
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = "adist.had.surQ4",
                weight = 1,
                data = adist.survQ4,
                fleetnames = "survQ4had",
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = "alk.had.surQ2",
                weight = 1,
                data = aldist.survQ2,
                fleetnames = "survQ2had",
                stocknames = stock_names) %>%

  gadget_update("catchdistribution",
                name = "alk.had.surQ4",
                weight = 1,
                data = aldist.survQ4,
                fleetnames = "survQ4had",
                stocknames = stock_names) %>%
    
  gadget_update("surveyindices",
                name = "siQ2.had",
                weight = 1,
                data = si_spring,
                sitype = "lengths",
                biomass = 1,
                fittype = 'loglinearfit',
                ## fittype = 'fixedslopeloglinearfit',
                ## slope = 1,
                stocknames = stock_names) %>%

  gadget_update("surveyindices",
                name = "siQ4.had",
                weight = 1,
                data = si_fall,
                sitype = "lengths",
                biomass = 1,
                fittype = 'loglinearfit',
                ## fittype = 'fixedslopeloglinearfit',
                ## slope = 1,
                stocknames = stock_names) %>%
                    
  write.gadget.file(gd$dir)
