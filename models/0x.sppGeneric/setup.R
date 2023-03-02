library(mfdb)
library(tidyverse)
library(Rgadget)
library(mskeyrun)

rm(list=ls())

# 11 spp list with extra biological data
sppLookup <- data.frame(nobaSpp = c("BWH","CAP","GRH","HAD","LRD","MAC","NCO","PCO","RED","SAI","SSH"),
                        mfdbSpp = c("WHB","CAP","GLH","HAD","PLA","MAC","COD","POC","RED","SAI","HER"),
                        ageGrpSize = c(1,1,2,2,2,2,2,2,4,2,2),
                        ageMax = c(10,5,20,20,20,20,20,10,40,20,20),
                        minLen = c(13,3,56,12,7,14,20,10,7,25,5),
                        maxLen = c(47,23,199,129,71,65,138,35,39,196,51))
sppList <- left_join(simBiolPar, sppLookup %>% rename(Code=nobaSpp))

## Create a gadget directory, define some defaults to use with our queries below
dirName <- "had01"
system(paste("rm -r", dirName))
if(sum(match(list.files(),dirName), na.rm=T)==1){
    print(paste("folder",dirName,"exists"))
} else {gd <- gadget_directory(dirName)}


mdb <- mfdb('Barents', db_params=list(dbname="noba"))

simName <- "NOBA_sacc_38"
    
year_range <- 40:120
base_dir <- 'had'
stock <- 'had'
stock_names <- c(stock)
species_name <- 'had'

# define 'default' spatial and temporal aggregation
defaults <- list(
    year = year_range,
    area = "noba_area",
    timestep = mfdb_timestep_quarterly,
    species = 'HAD',
    length=mfdb_interval("len", seq(12, 129, by = 1)),
    age=mfdb_group('age0'=0,'age1'=1,'age2'=2,'age3'=3,'age4'=4,'age5'=5,'age6'=6,'age7'=7,'age8'=8,'age9'=9,'age10'=10,
                   'age11'=11,'age12'=12,'age13'=13,'age14'=14,'age15'=15,'age16'=16,'age17'=17,'age18'=18,'age19'=19,'age20'=20)
)

ageCls <- mfdb_group('age1'=1:2,'age3'=3:4,'age5'=5:6,'age7'=7:8,'age9'=9:10,'age11'=11:12,'age13'=13:14,'age15'=15:16,'age17'=17:18,'age19'=19:20)

gadgetfile('Modelfiles/time',
           file_type = 'time',
           components = list(list(firstyear = min(defaults$year),
                                  firststep=1,
                                  lastyear=max(defaults$year),
                                  laststep=4,
                                  notimesteps=c(4,3,3,3,3)))) %>% 
write.gadget.file(gd$dir)

## Write out areafile and update mainfile with areafile location
area <- expand.grid(year=min(defaults$year):max(defaults$year),
                    step=1:4,
                    area="noba_area",
                    mean=5)
area <- arrange(area,year,step)

# get temperature ...
## tmp <- read.gadget.file("~/Share/ICES/WGSAM/2019/keyrun/keyrun.10/Modelfiles/","area")[[2]]
## tmp <- unique(tmp[,c("year","mean")])
## area <- area %>%
##     select(-mean) %>%
##     left_join(tmp,by="year") %>%
##     mutate(mean=ifelse(is.na(mean), tmp$mean[dim(tmp)[1]], mean))
## rm(tmp)

gadget_areafile(
  size = mfdb_area_size(mdb, defaults)[[1]],
  ## temperature = mfdb_temperature(mdb, defaults)[[1]]) %>% 
  temperature = area) %>% 
gadget_dir_write(gd,.)

source('utils.R')
source('setup_fleets_stk.R')
source('setup_model_stk.R')
source('setup_catchdistribution_stk.R')
## source('setup_catchstatistic_stk.R')
source('setup_indices_stk.R')
source('setup_likelihood_stk.R')


Sys.setenv(GADGET_WORKING_DIR=normalizePath(gd$dir))
callGadget(s=1,log = 'init.log') #ignore.stderr = FALSE,

## update the input parameters with sane initial guesses
read.gadget.parameters(sprintf('%s/params.out',gd$dir)) %>% 
  init_guess(paste0(stock_names,'.rec.[0-9]'),1,0.001,1000,1) %>%
  ## mutate(value = ifelse(switch == paste0(stock_names,'.rec.1'), 1e-4 * exp(init.rec$number), value),
  ##        optimise = ifelse(switch == paste0(stock_names,'.rec.1'), 0, optimise)) %>%
  ## init_guess(paste0(stock_names,'.init.[0-9]'),1,0.001,1000,1) %>%
  init_guess(paste0(stock_names,'.rec.scalar'), 1e5,1,1e8,0) %>% 
  init_guess(paste0(stock_names,'.init.scalar'), 1e-4,1e-5,1e8,0) %>%
  init_guess(paste0(stock_names,'.recl'), 20,10,40,1) %>%
  init_guess(paste0(stock_names,'.rec.sd'), 4, 0.01, 15,0) %>%
  init_guess(paste0(stock_names,'.Linf'), grw.constants[1], 60, 140,0) %>%
  init_guess(paste0(stock_names,'.k'), 1e2 * grw.constants[2], 0.1, 100,1) %>%
  init_guess(paste0(stock_names,'.bbin'), 0.9, 0.001, 50, 1) %>% 
  init_guess(paste0(stock_names,'.com.alpha'), 0.9,  0.1, 3, 1) %>% 
  init_guess(paste0(stock_names,'.com.l50'), 40, 20, 100, 1) %>% 
  init_guess(paste0(stock_names,'.surQ2.alpha'), 0.9,  0.1, 2, 1) %>% 
  init_guess(paste0(stock_names,'.surQ2.l50'), 40, 10, 80, 1) %>% 
  init_guess(paste0(stock_names,'.surQ4.alpha'), 0.9,  0.1, 2, 1) %>% 
  init_guess(paste0(stock_names,'.surQ4.l50'), 40, 10, 80, 1) %>% 
  init_guess(paste0(stock_names,'.walpha'), lw.constants$a, 0.001, 0.1, 0) %>% 
  init_guess(paste0(stock_names,'.wbeta'), lw.constants$b, 2, 4, 0) %>% 
  init_guess(paste0(stock_names,'.M'), 0.2, 0.001, 1, 0) %>% 
  ## init_guess('had.init.F',0.3,0.1,1,0) %>%
write.gadget.parameters(.,file=sprintf('%s/params.in',gd$dir))


file.copy(sprintf('%s/optinfofile','./'),gd$dir)
## file.copy(sprintf('optinfofile','./'),gd$dir)

file.copy(sprintf('%s/run.R','./'),gd$dir)
## file.copy(sprintf('%s/run.R','./'),gd$dir)

mfdb_disconnect(mdb)

## set(gd$dir)
## source("run.R")
