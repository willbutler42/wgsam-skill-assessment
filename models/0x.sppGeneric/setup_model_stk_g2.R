## setup the immature stock first
stk <-
  gadgetstock(stock, gd$dir,missingOkay = TRUE) %>%
  gadget_update('stock',
                minage = defaults$age[[1]],
                maxage = defaults$age[[length(defaults$age)]],
                minlength = round(sppListi %>% .$minLen * 0.5),
                maxlength = round(sppListi %>% .$maxLen * 1.1),
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
                beta = paste0('(* 1e0 #',species_name,'.bbin)'),
                maxlengthgroupgrowth = nL.max) %>% 
  gadget_update('naturalmortality',
                ## c(z0,rep(paste0('#',species_name,'.M'),defaults$age[[length(defaults$age)]]))) %>%
                c(Ma$M[1], Ma$M)) %>% # assume M0 = M1
  ## c(z0, Ma$M)) %>%
  gadget_update('initialconditions',
                normalcond = data_frame(age = as.numeric(substring(init.num$age,4,5)),
                                        area = 1,
                                        age.factor = init.num$number,   
                                        area.factor = paste0('#',stock,'.init.scalar'),
                                        mean = parse(text=sprintf(paste0(species_name,'.Linf*(1-exp(-1*(0.01*',species_name,'.k)*(%1$s-(0.5+log(1-',species_name,'.recl/',species_name,'.Linf)/(0.01*',species_name,'.k)))))'),age)) %>% # notice [... -(0.5+log ...] so length scaled considering that rec is in timestep 3
                                          map(to.gadget.formulae) %>% 
                                          unlist(),
                                        stddev = seq(min(init.sigma$stddev), max(init.sigma$stddev), length.out=length(defaults$age)-1),
                                        relcond = 1)) %>% 
  gadget_update('iseaten',1) %>% 
  gadget_update('doesrenew',
                normalparam = data_frame(year = year_range,
                                         step = cut(sppListi$RecruitMonth, c(1,3,6,9,12), labels=1:4, include.lowest=T),
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

