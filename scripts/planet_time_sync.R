



##### Loop through samples
for(i in 1:length(list_samples)){
  sample    <- list_samples[i]
  
  sp_in_dir <- paste0(dwndir,sample,"/")    # input folder
  sampledir <- paste0(sp_dir,sample,"/")    # output folder for the unzipping
  png_name  <- paste0(ts_dir,sample,".png") # output name for the time sync
  
  if(!file.exists(png_name)){
    tryCatch({
      
      #### Unzip archives and rename the files
      system(sprintf("unzip -o %s -d %s  ",
                     paste0(sp_in_dir,sample,"*.zip"),
                     sampledir))
      
      #### Get the list of all TIF archives      
      list_tif <- list.files(paste0(sampledir,"files"),pattern=glob2rx("*.tif"))
      
      #### Take out UDM files
      list     <- list_tif[!(grepl("udm",list_tif))]
      
      #### Create a DataFrame
      df        <- data.frame(cbind(1:length(list),list))
      names(df) <- c("index","planet_id")
      
      #### Precise the type of archive
      df$type   <- "none"
      
      df[grepl("_RE",  df$planet_id),"type"] <- "RE" 
      df[grepl("_BGR", df$planet_id),"type"] <- "BG" 
      df[grepl("_3B_", df$planet_id),"type"] <- "3B" 
      df[grepl("_RGB_",df$planet_id),"type"] <- "RG" 
      
      ##### Precise the number of bands of archive
      df$bands <- 0
      
      df[grepl("Visual",          df$planet_id),"bands"] <- 3
      df[grepl("Analytic_clip",   df$planet_id),"bands"] <- 4
      df[grepl("3B_Analytic_clip",df$planet_id),"bands"] <- 3
      df[grepl("AnalyticMS",      df$planet_id),"bands"] <- 4
      df[df$type == "RE",                       "bands"] <- 5
      
      
      ##### Check dataset composition
      print(table(df$type,df$bands))
      nrow(df[df$type == "none" | df$bands == 0,]) == 0
      
      
      ##### Extract and format the date
      df$date <- "none"
      df[df$type == "RE","date"] <- str_split_fixed(df[df$type == "RE","planet_id"],"_",3)[,2]
      df[df$type == "BG","date"] <- str_split_fixed(df[df$type == "BG","planet_id"],"_",4)[,3]
      df[df$type == "RG","date"] <- str_split_fixed(df[df$type == "RG","planet_id"],"_",4)[,3]
      df[df$type == "3B","date"] <- str_split_fixed(df[df$type == "3B","planet_id"],"_",2)[,1]
      
      df[df$type == "3B","date"] <- paste0(substr(df[df$type == "3B","date"],1,4),"-",
                                           substr(df[df$type == "3B","date"],5,6),"-",
                                           substr(df[df$type == "3B","date"],7,8))
      
      ##### Sort by chronological order
      df       <- arrange(df,date)
      df$index <- row(df)[,1]
      
      
      print(paste0("Creating time sync for ",sample))
      
      ##### Open the PNG file
      png(file=  png_name,
          width= dimind*dim_h_grid,
          height=dimind*dim_v_grid)
      
      ##### Precise the array structure of the PNG + margins
      par(mfrow = c(dim_v_grid,dim_h_grid))
      par(mar=c(0,0,2,0))
      
      
      ##### Read the first image in the list and define the extent + margins
      raster <- brick(paste0(sampledir,"files/",df[1,"planet_id"]))
      e      <- extent(raster)
      
      margins <- extent(
        e@xmin-1,
        e@xmax+1,
        e@ymin-1,
        e@ymax+1)
      
      ####################################################################
      ################# Loop through the years from START to END
      for(year in year_start:year_end){
        print(year)
        
        ###### Print the empty outside frame
        plot(margins,axes=F,xlab="",ylab="")
        
        ###### Check if an image exists for the year and grep it, extract the date
        index  <- df[grepl(year,df$date),"index"]
        
        image  <- df[index,"planet_id"]
        date   <- df[index,"date"]
        
        tryCatch({
          
          ###### Read image
          raster <- brick(paste0(sampledir,"files/",image))
          
          ###### Plot the image RGB with a stretched histogram
          plotRGB(raster,stretch="hist",add=T)
          
          ###### Title equals the exact date
          title(main=paste0(date),font.main=1,cex.main=2,line=0,adj=0.05)
          
        },error=function(e){
          message =paste0("No image available in ",year)
          print(e)
          })
        
        
      }
      
      ###### Close the PNG
      dev.off()
      
    },error=function(e){print(paste0("No Download existing"))})
    
  }
}
