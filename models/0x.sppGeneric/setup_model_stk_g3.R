## -----------------------------------------------------------------------------
## Runner to set up the gadget3 stocks and stock actions
## -----------------------------------------------------------------------------

## SETUP STOCK
single_stock <-
  gadget3::g3_stock(c(species = stock), 
                    lengthgroups = seq(
                      round(sppListi %>% .$minLen * 0.5),
                      round(sppListi %>% .$maxLen * 1.1),
                      1
                      )) %>%
  gadget3::g3s_livesonareas(area_g3[c(defaults$area)]) %>% 
  gadget3::g3s_age(minage = defaults$age[[1]],
                   maxage = defaults$age[[length(defaults$age)]])

stock_list <- list(single_stock)

## PARAMETERS FOR THE MODEL
modelpars <- 
  list(
    
    initconditions2 = gadget3::g3_parameterized('init',
                                               by_stock = TRUE,
                                               by_age = TRUE,
                                               scale = gadget3::g3_parameterized('init.scalar')),
    
    ## Initial conditions 
    initconditions = 
      gadget3:::f_substitute(~scalar * init,
                             list(
                               scalar = gadget3::g3_parameterized(name = 'init.scalar', 
                                                                  by_stock = 'species'),
                               init = gadget3::g3_timeareadata(lookup_name = 'init_table',
                                                               df = 
                                                                 bind_rows(data.frame(year = 40,
                                                                                      step = 1,
                                                                                      area = 1,
                                                                                      age = 0,
                                                                                      number = 0),
                                                                           init.num %>% 
                                                                             mutate(age = gsub('age', '', age) %>% as.numeric(),
                                                                                    area = area_g3[c(defaults$area)],
                                                                                    step = as.numeric(step)) %>% 
                                                                             select(year, step, age, area, number)),
                                                               value_field = 'number')
                               )
                               ),
      
    ## Recruitment
    renewal = gadget3::g3_parameterized(name = 'rec',
                                        by_stock = list(single_stock),
                                        by_year = TRUE,
                                        scale = 
                                          gadget3::g3_parameterized(name = 'rec.scalar',
                                                                    by_stock = list(single_stock)),
                                        ifmissing = NaN),
    
    recsd = gadget3::g3_parameterized(name = 'rec.sd', by_stock = 'species'),
    recl = gadget3::g3_parameterized(name = 'recl', by_stock = 'species'),
    
    ## Growth
    linf = gadget3::g3_parameterized(name = 'Linf', by_stock = 'species'),
    k = gadget3::g3_parameterized(name = 'k', by_stock = 'species', scale = 0.01),
    bbin = gadget3::g3_parameterized(name = 'bbin', by_stock = 'species', scale = 1),
    
    ## Length-weight
    walpha = gadget3::g3_parameterized(name = 'walpha', by_stock = 'species', scale = 0.001),
    wbeta = gadget3::g3_parameterized(name = 'wbeta', by_stock = 'species')
    
  )

## SETUP THE STOCK ACTIONS

stock_actions <-  
  list(
    
    ## INITIAL CONDITIONS
    gadget3::g3a_initialconditions_normalparam(
      stock = single_stock,
      factor_f = modelpars$initconditions,
      mean_f = gadget3::g3a_renewal_vonb(
        Linf = modelpars$linf,
        K = modelpars$k,
        recl = modelpars$recl,
        recage = gadget3::g3_stock_def(single_stock, 'minage')
      ),
      stddev_f = gadget3::g3_timeareadata(lookup_name = 'initsd_table',
                                          df = bind_rows(data.frame(age = 0, sd = min(init.sigma$stddev)),
                                                         data.frame(age = as.numeric(substring(init.num$age,4,5)),
                                                                    sd = seq(min(init.sigma$stddev),
                                                                             max(init.sigma$stddev),
                                                                             length.out=length(defaults$age)-1))),
                                          value_field = 'sd'),
      alpha_f = modelpars$walpha,
      beta_f = modelpars$wbeta
    ),
    
    ## NATURAL MORTALITY
    ## We supply a lookup table for natural mortality. This is done using "g3_timeareadata".
    ## The 'value_field' argument tells the function what column of df contains the values.
    
    gadget3::g3a_naturalmortality(
      stock = single_stock,
      mortality_f = gadget3::g3a_naturalmortality_exp(
        param_f = gadget3::g3_timeareadata(lookup_name = 'M_table',
                                           df = Ma,
                                           value_field = 'M'))
      ),

    ## AGEING
    gadget3::g3a_age(stock = single_stock),

    # GROWTH AND MATURITY
    gadget3::g3a_growmature(

      stock = single_stock,
      ## Growth
      impl_f = gadget3::g3a_grow_impl_bbinom(
        delta_len_f =
          gadget3::g3a_grow_lengthvbsimple(
            linf_f = modelpars$linf,
            kappa_f = modelpars$k
            ),
        delta_wgt_f = gadget3::g3a_grow_weightsimple(
          alpha_f = modelpars$walpha,
          beta_f = modelpars$wbeta
          ),
        beta_f = modelpars$bbin,
        maxlengthgroupgrowth = nL.max
        )
      ),

    # RENEWAL
    gadget3::g3a_renewal_normalparam(

      stock = single_stock,
      factor_f = modelpars$renewal,
      mean_f = gadget3::g3a_renewal_vonb(
        Linf = modelpars$linf,
        K = modelpars$k,
        recl = modelpars$recl,
        recage = gadget3::g3_stock_def(single_stock, 'minage')
      ),
      stddev_f = modelpars$recsd,
      alpha_f = modelpars$walpha,
      beta_f = modelpars$wbeta,
      run_f = gadget3:::f_substitute(
        ~cur_step == recstep && age == minage && cur_time > 0 && !cur_year_projection,
        list(minage = gadget3::g3_stock_def(single_stock, 'minage'),
             recstep = as.numeric(cut(sppListi$RecruitMonth, c(1,3,6,9,12), labels=1:4, include.lowest=T))))),
    
    list()
    
    
  )


# stock_actions2 <- model_actions(imm = single_stock,
#                                mat = single_stock,
#                                mlgg = nL.max,
#                                mature = TRUE,
#                                #comp_id = c('species', 'sex'),
#                                parametric_sd = FALSE,
#                                init_mode = 2,
#                                exp_params = c(),
#                                tv_params = c(),
#                                by_age_params = 'M',
#                                recruiting = TRUE)

