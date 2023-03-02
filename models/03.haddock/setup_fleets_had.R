# ---------------------------------------------------------------------
# Retrieve catches HAD
caton <- mfdb_sample_totalweight(mdb, c('species','data_source'), list(
    area          = "noba_area",
    timestep      = mfdb_timestep_quarterly,
    year          = year_range,
    species       = 'HAD',
    sampling_type = 'LND',
    data_source   = paste0('catches_',simName)))[[1]] %>%
    mutate(area = 1, fleet = 'comhad') %>%
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
surQ2.catch <- 
  structure(rbind(data.frame(year=year_range,step=2,area=1,fleet="survQ2had",number=1)),
            area_group = mfdb_group(`1` = 1))

surQ4.catch <- 
  structure(rbind(data.frame(year=year_range,step=4,area=1,fleet="survQ4had",number=1)),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
## write to file
tmp <- gadgetfleet('Modelfiles/fleet_had',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'survQ2had',
                suitability =
                paste0('\n',
                         paste(c('had'),
                         ## paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               '#had.surQ2.alpha','#had.surQ2.l50',
                               collapse='\n')),
                data = surQ2.catch) %>%
  gadget_update('totalfleet',
                name = 'survQ4had',
                suitability =
                paste0('\n',
                         paste(c('had'),
                         ## paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               '#had.surQ4.alpha','#had.surQ4.l50',
                               collapse='\n')),
                data = surQ4.catch) %>%
  gadget_update('totalfleet',
                name = 'comhad',
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(c('had'),
                         ## paste(c('hadimm','hadmat'),
                               'function','exponentiall50',
                               '#had.com.alpha','#had.com.l50',
                               collapse='\n')),
                data = com.catch)

write.gadget.file(tmp, gd$dir)
