## convenience functions
von_b_formula <- function(a,linf='Linf',k='k',recl='recl'){
  a %>% 
    map(~infuser::infuse("{{linf}} * (1 - exp(-1 * (0.01 * {{k}}) * ({{a}} - (1 + log(1 - {{recl}}/{{linf}})/(0.01 * {{k}})))))",
                         a=.,linf=linf,k=k,recl=recl)) %>% 
    map(~parse(text=.) %>% 
          map(to.gadget.formulae)) %>% 
    unlist()
}

init_guess <- function(dat,pattern, value,  lower, upper, optimise){
  dat[grepl(pattern,dat$switch),'value'] <- value
  dat[grepl(pattern,dat$switch),'upper'] <- upper
  dat[grepl(pattern,dat$switch),'lower'] <- lower
  dat[grepl(pattern,dat$switch),'optimise'] <- optimise
  return(dat)
}
