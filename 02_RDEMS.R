source("00_Base.R")

#PARÁMETROS
lasdir <- choose_folder(caption = "Select las dir:") #input dir
output <- choose_folder(caption = "Select output dir:") #output dir
#Read and display a catalog of las files
lascat <- catalog(lasdir)
lasfiles <- lascat@data$filename
lascat@crs <- sp::CRS(epsg)

pblapply(lasfiles, TSE, res = 0.5, method = "knnidw", epsg = "+init=epsg:25829", 
       output = output, filterLas = "-drop_z_below 0")
