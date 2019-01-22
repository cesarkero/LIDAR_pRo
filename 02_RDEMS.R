source("00_Base.R")

lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
output <- choose_folder(caption = "Select output dir:") #output dir
files <- list.files(lasdir, pattern="*.LAZ|*.LAS", full.names=T, recursive = T) #creates a list of files
files


output <- "C:/GitHub/R_DEM_LIDAR_products/OUTPUT"
epsg <- "+init=epsg:25829"
delaunay <- T
knnidw <- F
kriging <- F
res <- 0.5

#Read and display a catalog of las files
lascat <- catalog(lasdir)
lasfiles <- lascat@data$filename
lascat@crs <- sp::CRS(epsg)

for (i in lasfiles){
    lasName <- tools::file_path_sans_ext(basename(i)) #lasname
    
    #read and remove values below 0
    filt <- "-drop_z_below 0"
    las = readLAS(i, filter = filt)

    #mdt
    if (delaunay == T & knnidw == F & kriging == F){
        mdt <- grid_terrain(las, res = res, method = "delaunay") # fastest
    } else if(delaunay == F & knnidw == T & kriging == F) {
        mdt <- grid_terrain(las, res = res, method = "knnidw", k = 5, p = 2) #medium speed
    } else if (delaunay == F & knnidw == F & kriging == T){
        mdt <- grid_terrain(las, res = res, method = "kriging", k = 5) #SLOWEST
    }
    
    lasnormalize(las,mdt) #normilizar para solo dejar altura de elementos sobre el suelo
    
    #mds
    mds = grid_canopy(las, res = res)

    #export raster files
    r1 <- as.raster(mdt)
    r2 <- as.raster(mds)
    crs(r1) <- epsg
    crs(r2) <- epsg
    writeRaster(r1,
                filename=paste(output,"/", lasName, "_mdt.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    writeRaster(r2,
                filename=paste(output,"/", lasName, "_mds.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
}