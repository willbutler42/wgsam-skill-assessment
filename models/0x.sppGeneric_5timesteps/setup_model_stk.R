# ---------------------------------------------------------------------
# Set the stock model

## length-weight relationship
lw.constants <- data.frame(a = sppListi %>% .$WLa,
                           b = sppListi %>% .$WLb)

## Growth
tmp <- mfdb_sample_count(mdb, c('age','length','species'), list(
       area          = defaults$area,
       ## timestep      = mfdb_group('1'=1:3),
       ## timestep      = mfdb_unaggregated(),
       timestep      = 3, # month of the survey
       year          = defaults$year,
       sampling_type = 'RES',
       data_source   = paste0('aldist_survey_',simName),
       species       = defaults$species,
       length        = defaults$length,
       age           = defaults$age))[[1]] %>%
    left_join(sppListi %>% rename(species = mfdbSpp)) %>%
    mutate(len2 = as.numeric(substring(length,4,6)),
           ## age2 = as.numeric(substring(age,4,5)) + as.numeric(step)/4-1/(4*2))
           age2 = as.numeric(substring(age,4,5)) + as.numeric(step)/12-1/(12*2)) # refine with the actual time of the survey (to be adj also for ageCls???)
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
        start_lower=c(Linf=sppListi %>% .$maxLen * 0.8,
                      k=0.1, t0=-2),
        start_upper=c(Linf=sppListi %>% .$maxLen * 1.2,
                      k=0.4, t0=1),
        iter=500))) %>%
    .$estimate
grw.constants <- c(grw.constants, grw.constants[1]*(1-exp(-grw.constants[2] * ((sppListi$RecruitMonthAdj/12-1/12/2)-grw.constants[3]))))
names(grw.constants) <- c("Linf","k","t0","recl")

## ggplot() +
##     geom_point(data=tmp %>% filter(year %in% c(60,80,100)),
##                aes(age2,len2,size=number)) +
##     geom_line(data=data.frame(age = seq(defaults$age[[1]], defaults$age[[length(defaults$age)]], length.out=100)) %>%
##                   mutate(len = grw.constants["Linf"]*(1-exp(-grw.constants["k"]*(age-grw.constants["t0"])))),
##               aes(age,len), col=2) +
##     geom_vline(xintercept = (sppListi$RecruitMonthAdj/12-1/12/2), color = 3) +
##     geom_hline(yintercept = grw.constants["recl"], color = 3) +
##     facet_wrap(~year)

## maxlengthgroupgrowth as x3 the mean length growth from recl
## dL = (Linf-Li)*(1-exp(-Kdt))
nL.max <- ceiling(1.5*floor((grw.constants["Linf"]-grw.constants["recl"]) * (1-exp(-grw.constants["k"]*0.25))))

## initial num@age
init.num <- mfdb_sample_count(mdb, c('age'), list(
      area            = defaults$area,
      timestep        = defaults$timestep,
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
    timestep        = defaults$timestep,
    year            = year_range,
    species         = defaults$species, 
    age             = defaults$age,
    sampling_type   = 'RES',
    data_source     = paste0('alnumb_init_',simName)))[[1]] %>%
    left_join(sppListi %>% rename(species = mfdbSpp)) %>%
    mutate(age2 = as.numeric(substring(age,4,5)) +
               (ifelse(ageGrpSize==1,0,
                ifelse(ageGrpSize==2,1,
                ifelse(ageGrpSize==4,2,NA))))) %>%
    arrange(age2) %>%
    mutate(age2 = NULL)

init.num <- init.num %>% filter(age %in% names(unlist(defaults$age))[-1])
## init.sigma <- init.sigma[-1,]

# manually adjust sd age8+
## init.sigma[init.sigma$age >= 8,"ms"] <- 1.00

## initial recruitment
init.rec <- mfdb_sample_count(mdb, c('age'), list(
      area            = defaults$area,
      timestep        = defaults$timestep,
      year            = 1, # avgRec stored in year1
      species         = defaults$species, 
      age             = defaults$age,
      sampling_type   = 'RES',
      data_source     = paste0('logrec_avg_',simName)))[[1]]

## Z age0 ----> Z = (log(N0)-log(N1))/dt
## z0 <- (log(exp(init.rec$number)*1e6) - log(init.num %>% filter(age=="age1") %>% .$number))/(1-(sppListi$SpawnMonth/12-1/12/2))

## M vector (based on an adaptation of Lorenzen eq, see Powers 2014 https://academic.oup.com/icesjms/article/71/7/1629/664136)
## ageVec <- 1 : defaults$age[[length(defaults$age)]]
ageVec <- unlist(defaults$age)
Minf <- exp(1.46-1.01*log(ageVec[length(ageVec)])) # from Hoenig 1983 for fish ln(Z)=1.46-1.01*ln(tmax)
Ma <- data.frame(age = ageVec,
                 M = Minf*(1-exp(-grw.constants["k"]*(ageVec-grw.constants["t0"])))^(-lw.constants$b[1]*0.305)) %>%
    mutate(M = round(M,3))
ggplot(Ma, aes(age,M)) + geom_point() + geom_line() + ylim(0,NA)
# constrain M@age <= 4
Ma <- Ma %>%
    mutate(M = ifelse(M>1.5 | is.na(M), 1.5, M))
ggplot(Ma, aes(age,M)) + geom_point() + geom_line() + ylim(0,NA)

## initial guess for maturity ogive parameters
mat.params <- c()

## SETUP MODELS
source(paste0('setup_model_stk_g', gadget_version, '.R'))

