# plot age and length distributions from the modelled population (also multimodel but only LenDist)
require(ggplot2)
require(dplyr)
require(tidyr)

ggLenDistStk <- function(Gfit, stkName, yrs=NULL, q=NULL, biomass=FALSE, ...){

    tmp <- Gfit$stock.full
    
    if(!is.null(yrs)==TRUE){
        tmp <- filter(tmp, year %in% yrs)
    } else { NULL }
    if(!is.null(q)==TRUE){
        tmp <- filter(tmp, step %in% q)
    } else { NULL }

    if(biomass==FALSE){
        tmp$value <- tmp$number
    } else {
        tmp$value <- tmp$number * tmp$mean_weight
    }

    # plot
    if(is.null(tmp$model)){
    tmp <- tmp %>% group_by(length,year,step) %>%
               summarise(value=sum(value,na.rm=T))
    ggplot(tmp, aes(length,value)) +
        geom_bar(stat="identity", ...) +
        facet_wrap(~year+step) +
        NULL
    } else {
    tmp <- tmp %>% group_by(length,model,year,step) %>%
        summarise(value=sum(value,na.rm=T))
    ggplot(tmp) +
        geom_line(aes(length,value,col=model),...) +
        facet_wrap(~year+step) +
        NULL
}}


ggAgeDistStk <- function(Gfit, stkName, yrs=NULL, biomass=FALSE, ...){

    tmp <- Gfit$out.fit[[paste(stkName,"std",sep=".")]]
    
    if(!is.null(yrs)==TRUE){
        tmp <- filter(tmp, year %in% yrs)
    } else { NULL }
    # at the moment only q=1 available
    ## if(!is.null(q)==TRUE){
    ##     tmp <- filter(tmp, step %in% q)
    ## } else { NULL }
    # at the moment only area=1 available
    ## if(!is.null(area)==TRUE){
    ##     tmp <- filter(tmp, area %in% q)
    ## } else { NULL }

    # plot
    if(biomass==FALSE){
        ggplot(tmp, aes(age,number)) +
            geom_bar(stat="identity") +
            facet_wrap(~year) +
            NULL
    } else {
        tmp$biomass <- tmp$number * tmp$mean_weight
        ggplot(tmp, aes(age,biomass)) +
            geom_bar(stat="identity") +
            facet_wrap(~year) +
            NULL
    }
}


ggAgeDistStk2 <- function(Gfit, stkName, yrs=NULL, ageVec=NULL, plusGroup=NULL, biomass=FALSE, ...){

    require(dplyr)
    require(reshape2)
    require(RColorBrewer)
    require(ggplot2)
    
    ## myColorPalette <- rep(brewer.pal(12,"Paired"),100)
    myColorPalette <- rep(brewer.pal(11,"Spectral"),100)
    
    tmp <- Gfit$stock.std %>% filter(stock %in% stkName)
    ## tmp$biomass <- tmp$number * tmp$mean_weight
    tmp <- tmp %>%
        mutate(biomass = number * mean_weight) %>%
        group_by(year,step,area,age) %>%
        summarise(number = sum(number),
                  biomass = sum(biomass))
    
    if(!is.null(yrs)==TRUE){
        tmp <- filter(tmp, year %in% yrs)
    } else { NULL }
    YrIni <- min(tmp$year,na.rm=T)
    YrFin <- max(tmp$year,na.rm=T)
    # ------
    # at the moment only q=1 available
    ## if(!is.null(q)==TRUE){
    ##     tmp <- filter(tmp, step %in% q)
    ## } else { NULL }
    # ------
    # at the moment only area=1 available
    ## if(!is.null(area)==TRUE){
    ##     tmp <- filter(tmp, area %in% q)
    ## } else { NULL }
    # ------
    if(!is.null(ageVec)==TRUE){
        tmp <- filter(tmp, age %in% ageVec)
    } else { NULL }
    tmp$yc <- tmp$year-tmp$age
    # ------
    if(!is.null(plusGroup)==TRUE){
        tmp$age <- ifelse(tmp$age>=plusGroup, plusGroup, tmp$age)
        tmp <- tmp %>%
               group_by(year,step,area,age,yc) %>%
               summarise(number=sum(number,na.rm=T),
                         biomass=sum(biomass,na.rm=T))
        tmp$yc <- tmp$year-tmp$age
        tmp$age <- ifelse(nchar(tmp$age)==1,paste("0",tmp$age,sep=""),tmp$age)
        tmp$age <- ifelse(tmp$age==max(tmp$age), paste(tmp$age,"+",sep=""), tmp$age)
    } else { tmp$yc <- tmp$year-tmp$age
             tmp$age <- ifelse(nchar(tmp$age)==1,paste("0",tmp$age,sep=""),tmp$age)}
    
    # plot
    n <- length(unique(tmp$yc))
    if(biomass==FALSE){
        ggplot(tmp, aes(year, number, fill=factor(yc))) +
          theme_bw() +
          geom_bar(stat = "identity") +
          expand_limits(x = c(YrIni,YrFin), y = 0) +
          scale_fill_manual(values=myColorPalette[1:n]) +
          facet_grid(age ~ ., scale = "free_y") +
          theme(legend.position = "none")
          ## theme(legend.position = "none",axis.text.y=element_blank())
    } else {
        ggplot(tmp, aes(year, biomass, fill=factor(yc))) +
          theme_bw() +
          geom_bar(stat = "identity") +
          expand_limits(x = c(YrIni,YrFin), y = 0) +
          scale_fill_manual(values=myColorPalette[1:n]) +
          facet_grid(age ~ ., scale = "free_y") +
          theme(legend.position = "none")
          ## theme(legend.position = "none",axis.text.y=element_blank())
    }
}
