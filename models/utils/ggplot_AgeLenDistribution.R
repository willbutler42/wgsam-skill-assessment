# plot age and length distributions, observed and predicted (also for comparing several models)
require(ggplot2)
require(dplyr)

ggLenDist <- function(Gfit, compName, yrs=NULL, q=NULL, ...){

    tmp <- Gfit$catchdist.fleets[Gfit$catchdist.fleets$name==compName,]
    if(!is.null(yrs)==TRUE){
        tmp <- filter(tmp, year %in% yrs)
    } else { NULL }
    if(!is.null(q)==TRUE){
        tmp <- filter(tmp, step %in% q)
    } else { NULL }

    if(is.null(tmp$model)){
        tmp <- tmp %>% group_by(avg.length,year,step) %>%
               summarise(obs=sum(observed,na.rm=T),
                         pre=sum(predicted,na.rm=T))
    # plot
    ggplot(tmp, aes(avg.length,obs)) +
        geom_bar(stat="identity") +
        geom_line(data=tmp, aes(avg.length,pre), colour=2) +
        facet_wrap(~year+step) +
        NULL
    } else {
        tmp <- tmp %>% group_by(avg.length,model,year,step) %>%
               summarise(obs=sum(observed,na.rm=T),
                         pre=sum(predicted,na.rm=T))
        modNames <- unique(tmp$model)
        
        # check that likelihood data are identical (***CHECK FOR BETTER CODING)
        vec <- NULL
        for(i in 2:length(modNames)){
            out <- identical(tmp[tmp$model==modNames[1],"obs"],tmp[tmp$model==modNames[i],"obs"])
            vec <- c(vec,out)
        }
        vec <- sum(as.numeric(vec==F))
        if(vec>0){
            stop("difference in the likelihood data")
        } else {NULL}
        
    # plot
    ggplot(filter(tmp,model==modNames[1]), aes(avg.length,obs)) +
        geom_bar(stat="identity") +
        geom_line(data=tmp, aes(avg.length,pre,col=model)) +
        facet_wrap(~year+step) +
        NULL
    }
}


ggAgeDist <- function(Gfit, compName, yrs=NULL, q=NULL, ...){

    tmp <- Gfit$catchdist.fleets[Gfit$catchdist.fleets$name==compName,]
    if(!is.null(yrs)==TRUE){
        tmp <- filter(tmp, year %in% yrs)
    } else { NULL }
    if(!is.null(q)==TRUE){
        tmp <- filter(tmp, step %in% q)
    } else { NULL }
    
    tmp$age2 <- as.numeric(as.character(substring(tmp$age,4,5)))

    if(is.null(tmp$model)){
        tmp <- tmp %>% group_by(age2,year,step) %>%
               summarise(obs=sum(observed,na.rm=T),
                         pre=sum(predicted,na.rm=T))
    # plot
    ggplot(tmp, aes(age2,obs)) +
        geom_bar(stat="identity") +
        geom_line(data=tmp, aes(age2,pre), colour=2) +
        facet_wrap(~year+step) +
        NULL
    } else {
        tmp <- tmp %>% group_by(age2,model,year,step) %>%
               summarise(obs=sum(observed,na.rm=T),
                         pre=sum(predicted,na.rm=T))
        modNames <- unique(tmp$model)

        # check that likelihood data are identical (***CHECK FOR BETTER CODING)
        vec <- NULL
        for(i in 2:length(modNames)){
            out <- identical(tmp[tmp$model==modNames[1],"obs"],tmp[tmp$model==modNames[i],"obs"])
            vec <- c(vec,out)
        }
        vec <- sum(as.numeric(vec==F))
        if(vec>0){
            stop("difference in the likelihood data")
        } else {NULL}
        
    # plot
    ggplot(filter(tmp,model==modNames[1]), aes(age2,obs)) +
        geom_bar(stat="identity") +
        geom_line(data=tmp, aes(age2,pre,col=model)) +
        facet_wrap(~year+step) +
        NULL
    }
}
