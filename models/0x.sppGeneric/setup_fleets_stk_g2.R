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
