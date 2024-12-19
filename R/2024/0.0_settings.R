##### my packages ################################################################################
## CRAN
cran.packages <- c('ggplot2','gridExtra','viridis','plyr','reshape2','lubridate',"data.table","dplyr")
install.this <- cran.packages[!(cran.packages %in% utils::installed.packages()[,"Package"])]
if(length(install.this)>1) install.packages(install.this)
dummy <- lapply(cran.packages, require, character.only = TRUE)

## github
git.packages <- c( 'DFOdata','catchR','CCAM')
install.this <- git.packages[!(git.packages %in% utils::installed.packages()[,"Package"])]
if('DFOdata' %in% install.this)  devtools::install_github("iml-assess/DFOdata")
if('catchR' %in% install.this)  devtools::install_github("iml-assess/catchR/eli-parallel")
if('CCAM' %in% install.this)  devtools::install_github("elisvb/CCAM")
dummy <- lapply(git.packages, require, character.only = TRUE)


source("../../R/2024/read.ziff.R")

##### my directories ################################################################################
dir.ziff <- 'W:/Ziff/Format CSV/ZIF_versiontotale_annuel'# dir to store csv ziff
dir.nafo <- '../../data/nafo/'# dir to store NAFO data downloaded online
dir.rdat <- '../../Rdata/'    # dir to store Rdata files 
dir.dat  <- '../../data/'

##### my stock #####################################################################################
my.species <- 'mackerel'                # no capitals
my.nafo <- 2:4
my.year <- 1968:2024                   # asessment years

dir.rdat <- paste0(dir.rdat,max(my.year),'/')             # Rdata will saved by assessment end year
dir.create(dir.rdat,recursive = T,showWarnings = F)

##### my ggplot theme ################################################################################
theme_set(theme_mackerel())             # theme_mackerel from catchR

