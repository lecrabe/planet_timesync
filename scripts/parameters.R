#####   Folder containing the planet download in case
#dwndir    <- paste0(normalizePath("~"),"/","planet_search_order_download/downloads/") # the folder containing the planet download
dwndir    <- paste0(normalizePath("~"),"/","planet_data/downloads/") 


##### Define the array dimension for the snippets (by default is 2 rows x 6 columns, in Landscape orientation)
dim_v_grid <- 2
dim_h_grid <- 6
dimind     <- 400


##### Define beginning and end year
year_start <- 2009
year_end   <- 2020