# ------------------------------------------
# function to add model run name on each plot
require(ggplot2)

ggRunName <- function(){
runName <- getwd()
runName <- strsplit(runName, split="/")[[1]]
runName <- paste(runName[length(runName)-1],runName[length(runName)],sep="/")
    cap <- labs(caption=runName)
    return(cap)
}
ggRunNameSize <- function(x){
    cap <- theme(plot.caption=element_text(size=x))
    return(cap)
}

# example
## ggplot() + ggRunName + ggRunNameSize
