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
dwndir    <- paste0(normalizePath("~"),"/","planet_search_order_download/downloads/") # the folder containing the planet download
plandir   <- paste0(normalizePath("~"),"/","planet_data/")                            # the folder where you want to extract and generate the time series. Will be created if it doesn't exist
how_many  <- 10                                                                       # number of samples you want to run the timesync creation on, randomly chosen  

##### Define the array dimension for the snippets (by default is 10 rows x 20 columns, in Landscape orientation)
dim_v_grid <- 10
dim_h_grid <- 20
dimind     <- 100

###### Should run from there on
sp_dir    <- paste0(plandir,"samples/")
ts_dir    <- paste0(plandir,"timesync/")
nd_dir    <- paste0(plandir,"ndvi_ts/")

dir.create(plandir,showWarnings = F,recursive = T)
dir.create(ts_dir,showWarnings = F)
dir.create(sp_dir,showWarnings = F)
dir.create(nd_dir,showWarnings = F)

##### List the available downloads and subset by how_many
list_dwnd    <- list.files(dwndir,pattern="sample_")
list_samples <- list_dwnd
list_samples <- sample(list_dwnd,how_many)
list_ids     <- substr(list_samples,8,nchar(list_downd))

length(list_samples)
i <- 1

##### Loop through samples
#for(i in 1:length(list_samples)){
sample    <- list_samples[i]

sp_in_dir <- paste0(dwndir,sample,"/")    # input folder
sampledir <- paste0(sp_dir,sample,"/")    # output folder for the unzipping
sp_nd_dir <- paste0(nd_dir,sample,"/")    # output folder for the NDVI stack
png_name  <- paste0(ts_dir,sample,".png") # output name for the time sync

dir.create(sp_nd_dir,showWarnings = F)

if(!file.exists(png_name)){
  tryCatch({
    
    #### Unzip archives and rename the files
    system(sprintf("unzip -o %s -d %s  ",
                   paste0(sp_in_dir,sample,"*.zip"),
                   sampledir))
    
    list_tif <- list.files(paste0(sampledir,"files"),pattern=glob2rx("*.tif"))
    list     <- list_tif[!(grepl("udm",list_tif))]
    
    df        <- data.frame(cbind(1:length(list),list))
    names(df) <- c("index","planet_id")
    df$type   <- "none"
    
    df[grepl("_RE",  df$planet_id),"type"] <- "RE" 
    df[grepl("_BGR", df$planet_id),"type"] <- "BG" 
    df[grepl("_3B_", df$planet_id),"type"] <- "3B" 
    df[grepl("_RGB_",df$planet_id),"type"] <- "RG" 
    
    df$bands <- 0
    
    df[grepl("Visual",  df$planet_id),"bands"]         <- 3
    df[grepl("Analytic_clip",df$planet_id),"bands"]    <- 4
    df[grepl("3B_Analytic_clip",df$planet_id),"bands"] <- 3
    df[grepl("AnalyticMS",df$planet_id),"bands"]       <- 4
    df[df$type == "RE","bands"]                        <- 5
    
    table(df$type,df$bands)
    head(df)
    
    nrow(df[df$type == "none" | df$bands == 0,]) == 0
    
    df$date <- "none"
    df[df$type == "RE","date"] <- str_split_fixed(df[df$type == "RE","planet_id"],"_",3)[,2]
    df[df$type == "BG","date"] <- str_split_fixed(df[df$type == "BG","planet_id"],"_",4)[,3]
    df[df$type == "RG","date"] <- str_split_fixed(df[df$type == "RG","planet_id"],"_",4)[,3]
    df[df$type == "3B","date"] <- str_split_fixed(df[df$type == "3B","planet_id"],"_",2)[,1]
    
    df[df$type == "3B","date"] <- paste0(substr(df[df$type == "3B","date"],1,4),"-",
                                         substr(df[df$type == "3B","date"],5,6),"-",
                                         substr(df[df$type == "3B","date"],7,8))
    df$date
    
    df$ndvi  <- paste0("ndvi_",df$date,".tif")
    
    df       <- df[df$bands %in% c(4,5),]
    df       <- arrange(df,date)
    df$index <- row(df)[,1]
    head(df)
    
    write.table(df$date,paste0(sp_nd_dir,"dates.csv"),row.names = F,col.names = F,quote = FALSE)
    
    
    ##### Open the PNG file
    png(file=  png_name,
        width= dimind*dim_h_grid,
        height=dimind*dim_v_grid)
    
    par(mfrow = c(dim_v_grid,dim_h_grid))
    par(mar=c(0,0,2,0))
    
    ##### Read the first image in the list and define the extent
    raster <- brick(paste0(sampledir,"files/",df[1,"planet_id"]))
    e <- extent(raster)
    
    margins <- extent(
      e@xmin-1,
      e@xmax+1,
      e@ymin-1,
      e@ymax+1)
    
    index <- 1
    
    ####################################################################
    ################# Loop through the acquisitions
    for(index in df$index){
      
      ###### Print the empty outside frame
      plot(margins,axes=F,xlab="",ylab="")
      
      image  <- df[index,"planet_id"]
      date   <- df[index,"date"]
      ndvi_b <- df[index,"bands"]
      ndvi_n <- df[index,"ndvi"]
      
      print(date)
      
      tryCatch({
        
        ###### Plot and EXPORT the NDVI
        planet <- brick(paste0(sampledir,"files/",image))
        e      <- extent(planet)
        nir    <- raster(planet,ndvi_b)
        red    <- raster(planet,3)
        ndvi   <- (nir-red)/(nir+red)
        
        #plotRGB(planet,stretch="hist")
        #nbands(planet)
        plot(ndvi,add=T)
        writeRaster(ndvi,paste0(sp_nd_dir,ndvi_n))
        
        
      },error=function(e){print(paste0("no image available"))})
      
      title(main=paste0(date),font.main=1,cex.main=1,line=0,adj=0.05)
    }
    
    ###### Close the PNG
    dev.off()
    
    system(sprintf("gdalbuildvrt %s %s -separate",
                   paste0(sp_nd_dir,"stack.vrt"),
                   paste0(sp_nd_dir,"ndvi*.tif")
    ))
    
  },error=function(e){print(paste0("No Download existing"))})
  
}
#}
