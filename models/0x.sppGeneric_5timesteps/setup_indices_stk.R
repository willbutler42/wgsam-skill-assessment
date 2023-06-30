minage <- stk[[1]]$minage
maxage <- stk[[1]]$maxage
maxlength <- stk[[1]]$maxlength 
minlength <- stk[[1]]$minlength
dl <- 1

ll <- mfdb_interval("all",c(minlength,maxlength),
                    open_ended = c("upper","lower"))
names(ll) <- "all"
## names(ll) <- c("all","all")

aa <- mfdb_interval("all",c(minage,maxage), open_ended = c("upper","lower"))
names(aa) <- "all"

# ---------------------------------------------------------------------
# Query index abundance surveys Spring
si_spring <- mfdb_sample_totalweight(mdb, c('length'), list(
## si_spring <- mfdb_survey_index_mean(mdb, c('length'), list(
## si_spring <- mfdb_sample_count(mdb, c('data_source'), list(
    area          = defaults$area,
    timestep      = defaults$timestep,
    year          = defaults$year,
    ## index_type   = paste0('BTS_spring_',defaults$species),
    ## data_source   = paste0('BTS_spring_',simName,"_",defaults$species),
    data_source   = paste0('index_BTS_spring_',simName),
    species       = defaults$species,
    length        = ll))[[1]]
si_spring[1:4,]

## si_spring$area <- "area1"
## names(attributes(si_spring)$area) <- "area1"
## attributes(si_spring)$area$area1 <- "area1"
## attributes(si_spring)$length$all <- c(minlength,maxlength)

## ggplot(si_spring) +
##     geom_line(aes(year,total_weight))

# ---------------------------------------------------------------------
# Query index abundance surveys Fall
si_fall <- mfdb_sample_totalweight(mdb, c('length'), list(
    area          = defaults$area,
    timestep      = defaults$timestep,
    year          = defaults$year,
    data_source   = paste0('index_BTS_fall_',simName),
    species       = defaults$species,
    length        = ll))[[1]]
si_fall[1:4,]

