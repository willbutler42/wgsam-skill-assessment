## -----------------------------------------------------------------------------
## Run script for gadget3 models
## -----------------------------------------------------------------------------

library(gadget3)
library(gadgetutils)
library(gadgetplots)

## Using project directory as the working directory
setwd("~/gadget-framework/wgsam-skill-assessment-fork")

fixed_weights <- FALSE

## Directories
base_dir <- 'models/CopyOf0x.sppGeneric'
species_name <- 'had'
vers <- paste0(species_name, '01_g3')

## Load model and parameters
load(file = paste0('models/CopyOf0x.sppGeneric/', vers, '/tmb_model.Rdata'))
load(file = paste0('models/CopyOf0x.sppGeneric/', vers, '/tmb_param.Rdata'))

## Options and settings for iterative
cv_floor <- 0.008

## Groupings for iterative re-weighting
grouping <- list(sind = c(paste0('log_siQ1.', species_name),
                          paste0('log_siQ1.', species_name)))

## Controls for optim
control_list <- list(maxit = 1000, reltol = .Machine$double.eps^2)

#; Component	Type	Weight
#alk.had.surQ1	3	44533.6
#ldist.had.com	3	151772
#ldist.had.surQ1	3	9740.64
#ldist.had.surQ3	3	6769.08
#siQ1.had	1	50.0618
#siQ3.had	1	34.9288
#understocking	2	10
#bounds	8	10

if (fixed_weights){
  
  tmb_param$value$adist_surveyindices_log_siQ1.had_weight <- 50.0618
  tmb_param$value$adist_surveyindices_log_siQ3.had_weight <- 34.9288
  tmb_param$value$cdist_sumofsquares_ldist.had.com_weight <- 151772
  tmb_param$value$cdist_sumofsquares_aldist.had.surQ1_weight <- 44533.6
  tmb_param$value$cdist_sumofsquares_ldist.had.surQ1_weight <- 9740.64
  tmb_param$value$cdist_sumofsquares_ldist.had.surQ3_weight <- 6769.08
  
  ## Compile the objective function
  obj.fun <- gadget3::g3_tmb_adfun(tmb_model, tmb_param)
  
  params_final <- gadgetutils::g3_optim(model = tmb_model,
                                        params = tmb_param,
                                        control = control_list)
  
  ## Get the model fit and plots
  fit <- gadgetutils::g3_fit(tmb_model, params_final)
  save(fit, file = file.path(base_dir, vers, 'OPT/fit.Rdata'))
  gadgetplots::gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html')
  
  
}else{
  
  ## Compile the objective function
  obj.fun <- gadget3::g3_tmb_adfun(tmb_model, tmb_param)
  
  ## Iterative
  params_final <- gadgetutils::g3_iterative(file.path('models/CopyOf0x.sppGeneric', vers),
                                            wgts = 'WGTS',
                                            model = tmb_model,
                                            params.in = tmb_param,
                                            grouping = grouping,
                                            method = 'BFGS',
                                            control = control_list,
                                            use_parscale = TRUE,
                                            shortcut = FALSE,
                                            cv_floor = cv_floor,
                                            resume_final = FALSE)
  
}

## Get the model fit and plots
fit <- gadgetutils::g3_fit(tmb_model, params_final)
save(fit, file = file.path(base_dir, vers, 'WGTS/fit.Rdata'))
gadgetplots::gadget_plots(fit, file.path(base_dir, vers, 'figs'), file_type = 'html')
