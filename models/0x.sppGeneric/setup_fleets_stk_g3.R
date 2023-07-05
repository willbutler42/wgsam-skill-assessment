
## FLEET OBJECTS
## What is the fleet name? And where does it fish?
com <- 
  gadget3::g3_fleet('com') %>% 
  gadget3::g3s_livesonareas(area_g3[c('noba_area')])

survQ1 <- 
  gadget3::g3_fleet('survQ1') %>% 
  gadget3::g3s_livesonareas(area_g3[c('noba_area')])

survQ3 <- 
  gadget3::g3_fleet('survQ3') %>% 
  gadget3::g3s_livesonareas(area_g3[c('noba_area')])

## -----------------------------------------------------------------------------

## FLEET ACTIONS
## Setup the selectivities and catch functions

fleet_actions <- 
  list(
    com %>% 
      gadget3::g3a_predate_fleet(prey_stocks = stock_list,
                                 suitabilities = 
                                   stock_list %>% 
                                   set_names(.,map(.,'name')) %>% 
                                   map(function(x){
                                     if (FALSE){ 
                                       g3_suitability_andersen(
                                         p0 = g3_parameterized('andersen.p0', by_stock = 'species'),
                                         p1 = g3_parameterized('com.p1', by_stock = 'species'),
                                         p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
                                         p3 = g3_parameterized('com.p3', by_stock = 'species'),
                                         p4 = g3_parameterized('com.p4', by_stock = 'species'),
                                         p5 = g3_parameterized('andersen.L', by_stock = 'species')
                                         )
                                       }else{ ## S-shaped selectivity
                                         gadget3::g3_suitability_exponentiall50(
                                           alpha = g3_parameterized('com.alpha', by_stock = 'species'),
                                           l50 = g3_parameterized('com.l50', by_stock = 'species')
                                           ) 
                                         }
                                     }),
                                 catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                   gadget3::g3_timeareadata(lookup_name = 'com_landings', 
                                                            df = 
                                                              com.catch %>%
                                                              mutate(step = as.numeric(step)) %>% 
                                                              select(-fleet)))),
    survQ1 %>% 
      gadget3::g3a_predate_fleet(prey_stocks = stock_list,
                                 suitabilities = 
                                   stock_list %>% 
                                   set_names(.,map(.,'name')) %>% 
                                   map(function(x){
                                     if (FALSE){
                                       g3_suitability_andersen(
                                         p0 = g3_parameterized('andersen.p0', by_stock = 'species'),
                                         p1 = g3_parameterized('surQ1.p1', by_stock = 'species'),
                                         p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
                                         p3 = g3_parameterized('surQ1.p3', by_stock = 'species'),
                                         p4 = g3_parameterized('surQ1.p4', by_stock = 'species'),
                                         p5 = g3_parameterized('andersen.L', by_stock = 'species')
                                         )
                                       }else{
                                         gadget3::g3_suitability_exponentiall50(
                                           alpha = g3_parameterized('surQ1.alpha', by_stock = 'species'),
                                           l50 = g3_parameterized('surQ1.l50', by_stock = 'species')
                                         )
                                         }
                                     }),
                                 catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                   gadget3::g3_timeareadata(lookup_name = 'survQ1_landings', 
                                                            df = surQ1.catch %>% select(-fleet),
                                                            value_field = 'number'))),
    survQ3 %>% 
      gadget3::g3a_predate_fleet(prey_stocks = stock_list,
                                 suitabilities = 
                                   stock_list %>% 
                                   set_names(.,map(.,'name')) %>% 
                                   map(function(x){
                                     if (FALSE){
                                       g3_suitability_andersen(
                                         p0 = g3_parameterized('andersen.p0', by_stock = 'species'),
                                         p1 = g3_parameterized('surQ3.p1', by_stock = 'species'),
                                         p2 = g3_parameterized('andersen.p2', by_stock = 'species'),
                                         p3 = g3_parameterized('surQ3.p3', by_stock = 'species'),
                                         p4 = g3_parameterized('surQ3.p4', by_stock = 'species'),
                                         p5 = g3_parameterized('andersen.L', by_stock = 'species')
                                         )
                                       }else{
                                         gadget3::g3_suitability_exponentiall50(
                                           alpha = g3_parameterized('surQ3.alpha', by_stock = 'species'),
                                           l50 = g3_parameterized('surQ3.l50', by_stock = 'species')
                                           ) 
                                         }
                                     }),
                                 catchability_f = gadget3::g3a_predate_catchability_totalfleet(
                                   gadget3::g3_timeareadata(lookup_name = 'survQ3_landings', 
                                                            df = surQ3.catch %>% select(-fleet),
                                                            value_field = 'number'))),
    list()
  )



