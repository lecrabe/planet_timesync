########################################
# include all the needed packages here #
options(stringsAsFactors = F)
packages <- function(x){
  x <- as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")     
    require(x,character.only=TRUE)   } }

## Packages for geospatial data handling
packages(raster)
packages(rgeos)
packages(rgdal)
packages(Formula)
packages(gdalUtils)

## Packages for data table handling
packages(xtable)
packages(DT)
packages(dismo)
packages(stringr)
packages(plyr)

## Packages for graphics and interactive maps
packages(ggplot2)
packages(leaflet)
packages(leaflet.extras)
packages(RColorBrewer)
packages(data.table)


###### Modify those 3 inputs as necessary
plandir   <- paste0(normalizePath("~"),"/","planet_data/")                            # the folder where you want to extract and generate the timesyncs. Will be created if it doesn't exist
scriptdir <- paste0(normalizePath("~"),"/","planet_timesync/scripts/")

###### Should run from there on
sp_dir    <- paste0(plandir,"samples/")
ts_dir    <- paste0(plandir,"timesync/")

dir.create(plandir,showWarnings = F,recursive = T)
dir.create(ts_dir,showWarnings = F)
dir.create(sp_dir,showWarnings = F)


source(paste0(scriptdir,"parameters.R"),echo = T)

##### List the available downloads and subset by how_many
list_dwnd    <- list.files(dwndir,pattern="sample_")
list_samples <- list_dwnd

length(list_samples)