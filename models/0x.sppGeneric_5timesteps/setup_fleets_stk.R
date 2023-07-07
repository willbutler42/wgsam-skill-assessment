# ---------------------------------------------------------------------
# Retrieve catches STK
caton <- mfdb_sample_totalweight(mdb, c('species','data_source'), list(
    area          = "noba_area",
    timestep      = defaults$timestep,
    year          = year_range,
    species       = defaults$species,
    sampling_type = 'LND',
    data_source   = paste0('catches_',simName)))[[1]] %>%
    mutate(area = 1, fleet = paste0('com',species_name)) %>%
    select(year,step,area,fleet,total_weight) %>%
    mutate(total_weight = total_weight * 1e3) # convert ton2kg
caton[1:3,]

## ggplot(caton %>% group_by(year) %>% summarise(total_weight=sum(total_weight))) +
##     geom_line(aes(year,total_weight)) +
##     theme_bw()

com.catch <- structure(rbind(caton),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
# make spring and fall surveys
surQ1.catch <- 
  structure(rbind(data.frame(year=year_range,step=1,area=1,fleet=paste0('survQ1',species_name),number=1)),
            area_group = mfdb_group(`1` = 1))

surQ3.catch <- 
  structure(rbind(data.frame(year=year_range,step=3,area=1,fleet=paste0('survQ3',species_name),number=1)),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------

## SETUP THE FLEET FILES OR ACTIONS
source(paste0('setup_fleets_stk_g', gadget_version, '.R'))
