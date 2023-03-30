# ---------------------------------------------------------------------
# Set the stock model

## length-weight relationship
lw.constants <- data.frame(a = sppList %>% filter(mfdbSpp == defaults$species) %>% .$WLa,
                           b = sppList %>% filter(mfdbSpp == defaults$species) %>% .$WLb)


## Growth
tmp <- mfdb_sample_count(mdb, c('age','length','species'), list(
       area          = defaults$area,
       timestep      = mfdb_timestep_quarterly,
       year          = defaults$year,
       sampling_type = 'RES',
       data_source   = paste0('aldist_survey_',simName),
       species       = defaults$species,
       length        = defaults$length,
       age           = defaults$age))[[1]] %>%
    left_join(sppList %>% rename(species = mfdbSpp)) %>%
    mutate(len2 = as.numeric(substring(length,4,6)),
           age2 = as.numeric(substring(age,4,5)) + as.numeric(step)/4-0.125) # refine with the actual time of the survey and adj for ageCls
tmp <- tmp %>%
    group_by(year,area,species,ModSim,age2) %>%
    summarise(numTot = sum(number)) %>%
    right_join(tmp) %>%
    mutate(prop = number/numTot)

library(nls.multstart)
grw.constants <- tmp %>%
    ungroup() %>%
    do(broom::tidy(nls_multstart(len2~Linf*(1-exp(-k*(age2-t0))),. ,
        modelweights = prop, # needed because they are not individual data
        nls.control(maxiter = 1000),
        start_lower=c(Linf=sppList %>% filter(mfdbSpp == defaults$species) %>% .$maxLen * 0.8,
                      k=0.1, t0=-2),
        start_upper=c(Linf=sppList %>% filter(mfdbSpp == defaults$species) %>% .$maxLen * 1.2,
                      k=0.4, t0=1),
        iter=20))) %>%
    .$estimate

## ggplot() +
##     geom_point(data=tmp %>% filter(year %in% c(60,80,100)),
##                aes(age2,len2,size=number)) +
##     geom_line(data=data.frame(age = seq(defaults$age[[1]], defaults$age[[length(defaults$age)]], length.out=100)) %>%
##                   mutate(len = grw.constants[1]*(1-exp(-grw.constants[2]*(age-grw.constants[3])))),
##               aes(age,len), col=2) +
##     facet_wrap(~year)

## initial num@age
init.num <- mfdb_sample_count(mdb, c('age'), list(
      area            = defaults$area,
      timestep        = mfdb_timestep_quarterly,
      year            = year_range,
      species         = defaults$species, 
      age             = defaults$age,
      sampling_type   = 'RES',
      data_source     = paste0('anumb_init_',simName)))[[1]] %>%
    mutate(age2 = as.numeric(substring(age,4,5))) %>%
    arrange(age2) %>%
    mutate(age2 = NULL)

## initial conditions
init.sigma <- mfdb_sample_meanlength_stddev(mdb, c('age','species'), list(
    area            = defaults$area,
    timestep        = mfdb_timestep_quarterly,
    year            = year_range,
    species         = defaults$species, 
    age             = defaults$age,
    sampling_type   = 'RES',
    data_source     = paste0('alnumb_init_',simName)))[[1]] %>%
    left_join(sppList %>% rename(species = mfdbSpp)) %>%
    mutate(age2 = as.numeric(substring(age,4,5)) +
               (ifelse(ageGrpSize==1,0,
                ifelse(ageGrpSize==2,1,
                ifelse(ageGrpSize==4,2,NA))))) %>%
    arrange(age2) %>%
    mutate(age2 = NULL)

# manually adjust sd age8+
## init.sigma[init.sigma$age >= 8,"ms"] <- 1.00

## initial recruitment
init.rec <- mfdb_sample_count(mdb, c('age'), list(
      area            = defaults$area,
      timestep        = mfdb_timestep_quarterly,
      year            = year_range[1]-1,
      species         = defaults$species, 
      age             = defaults$age,
      sampling_type   = 'RES',
      data_source     = paste0('logrec_init_',simName)))[[1]]

## Z age0
z0 <- log(exp(init.rec$number)*1e6) - log(init.num %>% filter(age=="age1") %>% .$number)

## M vector (based on an adaptation of Lorenzen eq, see Powers 2014 https://academic.oup.com/icesjms/article/71/7/1629/664136)
ageVec <- 1 : defaults$age[[length(defaults$age)]]
Minf <- exp(1.46-1.01*log(ageVec[length(ageVec)])) # from Hoenig 1983 for fish ln(Z)=1.46-1.01*ln(tmax)
Ma <- data.frame(age = ageVec,
                 M = Minf*(1-exp(-grw.constants[2]*(ageVec-grw.constants[3])))^(-lw.constants$b[1]*0.305)) %>%
    mutate(M = round(M,3))
ggplot(Ma, aes(age,M)) + geom_point() + geom_line() + ylim(0,NA)

## initial guess for maturity ogive parameters
mat.params <- c()

## setup the immature stock first
stk <-
  gadgetstock(stock, gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = defaults$age[[1]],
                maxage = defaults$age[[length(defaults$age)]],
                minlength = round(sppList %>% filter(mfdbSpp == defaults$species) %>% .$minLen * 0.5),
                maxlength = round(sppList %>% filter(mfdbSpp == defaults$species) %>% .$maxLen * 1.1),
                dl = 1,
                livesonareas = 1) %>%
  gadget_update('refweight',
                data=data_frame(length=seq(.[[1]]$minlength,.[[1]]$maxlength,.[[1]]$dl),
                                mean=(1e-3*lw.constants$a[1])*length^lw.constants$b[1])) %>% 
  gadget_update('doesgrow', ## note to self the order of these parameters make difference
                growthparameters=c(linf = paste0('#',species_name,'.Linf'), 
                                   k = paste0('(* 0.01 #',species_name,'.k)'),
                                   alpha = paste0('(* 1e-3 #',species_name, ".walpha)"),
                                   beta =  paste0('#',species_name, ".wbeta")),
                beta = paste0('(* 10 #',species_name,'.bbin)'),
                ## beta = to.gadget.formulae(quote(1e1*had.bbin)),
                maxlengthgroupgrowth = 3) %>% 
  gadget_update('naturalmortality',
                ## c(z0,rep(paste0('#',species_name,'.M'),defaults$age[[length(defaults$age)]]))) %>%
                c(Ma$M[1], Ma$M)) %>% # assume M0 = M1
  gadget_update('initialconditions',
                ## normalcond = data_frame(age = 1:.[[1]]$maxage,
                ##                          area = 1,
                ##                          age.factor = parse(text=sprintf(paste0('exp(-1*(had.M+had.init.F)*%1$s)*had.init.%1$s'), age)) %>% 
                ##                            map(to.gadget.formulae) %>% 
                ##                            unlist(),   
                ##                          area.factor = '#had.init.scalar',
                ##                          ## mean = von_b_formula(age,linf='had.Linf',k='had.k',recl='had.recl'),
                ##                          mean = parse(text=sprintf('had.Linf*(1-exp(-1*(0.01*had.k)*(%1$s-(0.5+log(1-had.recl/had.Linf)/(0.01* had.k)))))',age)) %>% # notice [... -(0.5+log ...] so length scaled considering that rec is in timestep 3
                ##                              map(to.gadget.formulae) %>% 
                ##                              unlist(),   
                ##                          stddev = init.sigma$stddev[age],
                ##                          relcond = 1)) %>%

                ## normalcond = data_frame(age = as.numeric(substring(init.sigma$age,4,5)),
                ##                         area = 1,
                ##                         age.factor = init.sigma$number,   
                ##                         area.factor = '#had.init.scalar',
                ##                         mean = init.sigma$mean,
                ##                         stddev = init.sigma$stddev,
                ##                         relcond = 1)) %>%
                
                normalcond = data_frame(age = as.numeric(substring(init.num$age,4,5)),
                                        area = 1,
                                        age.factor = init.num$number,   
                                        area.factor = paste0('#',stock,'.init.scalar'),
                                        mean = parse(text=sprintf(paste0(species_name,'.Linf*(1-exp(-1*(0.01*',species_name,'.k)*(%1$s-(0.5+log(1-',species_name,'.recl/',species_name,'.Linf)/(0.01*',species_name,'.k)))))'),age)) %>% # notice [... -(0.5+log ...] so length scaled considering that rec is in timestep 3
                                             map(to.gadget.formulae) %>% 
                                             unlist(),
                                        stddev = seq(min(init.sigma$stddev), max(init.sigma$stddev), length.out=length(defaults$age)-1),
                                        relcond = 1)) %>% 
  ## does"something" updates should also allow for other names, e.g. doesrenew -> recruitment etc..
  gadget_update('iseaten',1) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = 4,
                                         area = 1,
                                         age = .[[1]]$minage,
                                         number = parse(text=sprintf(paste0(species_name,'.rec.scalar*',species_name,'.rec.%s'),year)) %>% 
                                           map(to.gadget.formulae) %>% 
                                           unlist(),
                                         ## mean = von_b_formula(age,linf='had.Linf',k='had.k',recl='had.recl'),
                                         mean = parse(text=sprintf(paste0(species_name,'.Linf*(1-exp(-1*(0.01*',species_name,'.k)*(%1$s-(0+log(1-',species_name,'.recl/',species_name,'.Linf)/(0.01*',species_name,'.k)))))'),age)) %>% # notice [... -(0+log ...] so recl is mean length at recr time
                                             map(to.gadget.formulae) %>% 
                                             unlist(),   
                                         stddev = paste0('#',stock_names,'.rec.sd'),
                                         alpha = paste0('(* 1e-3 #',stock_names,'.walpha)'),
                                         beta = paste0('#',stock_names,'.wbeta')))
stk$initialconditions$minage <- 1
## stk$initialconditions$maxage <- 20
## stk$initialconditions$minlength <- 8.5
## stk$initialconditions$maxlength <- 17.5


## write to file
stk %>% 
  write.gadget.file(gd$dir)

