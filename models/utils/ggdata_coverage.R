ggDataCoverage <- function(x, col=NULL, ...){
    # x: gadget.fit object returned from 'fit.gadget()'

    tmp <- vector(3, mode="list")
    # catchdist LH component
    tmp1 <- x$catchdist.fleets
    tmp1$timestep <- tmp1$year + tmp1$step/max(tmp1$step) - 1/max(tmp1$step)/2
    tmp[[1]] <- tmp1 %>% data.frame()
    # surveyIndex LH component
    tmp2 <- x$sidat
    tmp2$timestep <- tmp2$year + tmp2$step/max(tmp2$step) - 1/max(tmp2$step)/2
    tmp[[2]] <- tmp2 %>% data.frame()
    # stockdist LH component
    tmp3 <- x$stockdist
    if(!is.null(tmp3)){ # only here because this is the LH that sometimes is missing
        tmp3$timestep <- tmp3$year + tmp3$step/max(tmp3$step) - 1/max(tmp3$step)/2}
    tmp[[3]] <- tmp3 %>% data.frame()

    # all LH component
    if(nrow(tmp[[3]]) > 0){
      tmp <- rbind(tmp[[1]][,c("timestep","name")],
                   tmp[[2]][,c("timestep","name")],
                   tmp[[3]][,c("timestep","name")])
    } else {
      tmp <- rbind(tmp[[1]][,c("timestep","name")],
                   tmp[[2]][,c("timestep","name")])}
    ## tmp <- rbind(tmp1[,c("timestep","name")],tmp2[,c("timestep","name")],tmp3[,c("timestep","name")])  
    tmp$compFac <- as.factor(tmp$name)

    if(is.null(col)){
        ggplot(tmp, aes(timestep,compFac)) +
            geom_point(aes(col=compFac)) +
            theme(legend.position = "none") +
            scale_y_discrete(labels=aes(tmp$compFac))
    } else {
        ggplot(tmp, aes(timestep,compFac)) +
            geom_point(col=col) +
            theme(legend.position = "none") +
            scale_y_discrete(labels=aes(tmp$compFac))
    }
}
