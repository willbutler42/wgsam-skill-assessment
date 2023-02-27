# ---------------------------------------------------------------------
# Retrieve catches COD
caton <- mfdb_sample_totalweight(mdb, c('species','data_source'), list(
    area          = "noba_area",
    timestep      = mfdb_timestep_quarterly,
    year          = year_range,
    species       = 'COD',
    sampling_type = 'LND',
    data_source   = paste0('catches_',simName)))[[1]] %>%
    mutate(area = 1, fleet = 'comcod') %>%
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
  structure(rbind(data.frame(year=year_range,step=2,area=1,fleet="survQ2cod",number=1)),
            area_group = mfdb_group(`1` = 1))

surQ4.catch <- 
  structure(rbind(data.frame(year=year_range,step=4,area=1,fleet="survQ4cod",number=1)),
            area_group = mfdb_group(`1` = 1))

# ---------------------------------------------------------------------
## write to file
tmp <- gadgetfleet('Modelfiles/fleet_cod',gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = 'survQ2cod',
                suitability =
                paste0('\n',
                         paste(c('cod'),
                         ## paste(c('codimm','codmat'),
                               'function','exponentiall50',
                               '#cod.surQ2.alpha','#cod.surQ2.l50',
                               collapse='\n')),
                data = surQ2.catch) %>%
  gadget_update('totalfleet',
                name = 'survQ4cod',
                suitability =
                paste0('\n',
                         paste(c('cod'),
                         ## paste(c('codimm','codmat'),
                               'function','exponentiall50',
                               '#cod.surQ4.alpha','#cod.surQ4.l50',
                               collapse='\n')),
                data = surQ4.catch) %>%
  gadget_update('totalfleet',
                name = 'comcod',
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(c('cod'),
                         ## paste(c('codimm','codmat'),
                               'function','exponentiall50',
                               '#cod.com.alpha','#cod.com.l50',
                               collapse='\n')),
                data = com.catch)

write.gadget.file(tmp, gd$dir)
