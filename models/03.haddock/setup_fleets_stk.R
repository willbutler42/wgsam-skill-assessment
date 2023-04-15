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
## write to file
tmp <- gadgetfleet(paste0('Modelfiles/fleet_',species_name),gd$dir,missingOkay = TRUE) %>% 
  gadget_update('totalfleet',
                name = paste0('survQ1',species_name),
                suitability =
                paste0('\n',
                         paste(stock_names,
                               'function','exponentiall50',
                               paste0('#',species_name,'.surQ1.alpha'), paste0('#',species_name,'.surQ1.l50'),
                               collapse='\n')),
                data = surQ1.catch) %>%
  gadget_update('totalfleet',
                name = paste0('survQ3',species_name),
                suitability =
                paste0('\n',
                         paste(stock_names,
                               'function','exponentiall50',
                               paste0('#',species_name,'.surQ3.alpha'), paste0('#',species_name,'.surQ3.l50'),
                               collapse='\n')),
                data = surQ3.catch) %>%
  gadget_update('totalfleet',
                name = paste0('com',species_name),
                ## livesonareas = 1,
                suitability = 
                  paste0('\n',
                         paste(stock_names,
                               'function','exponentiall50',
                               paste0('#',species_name,'.com.alpha'), paste0('#',species_name,'.com.l50'),
                               collapse='\n')),
                data = com.catch)

write.gadget.file(tmp, gd$dir)
