libraries <- c("rgeos","lidR", "raster", "rgdal", "gridExtra", "rgdal", "rstudioapi",
               "httpuv", "leaflet", "mapview", "rgeos", "pbapply")

lapply(libraries, function (x) {if(x %in% rownames(installed.packages()) == FALSE) {install.packages(x)}})
lapply(libraries, require,  character.only=T)

#_______________________________________________________________________________
#General functions
choose_folder = function(caption = 'Select _______ file/directory:') {
    if (Sys.info()[[1]] == "Windows") {
        if (length(grep('lidar | las | laz', caption, ignore.case = T)) != 0) {
            path <- choose.dir(default = "C:/GitHub/LIDAR_pRo/LIDAR/PNOA_2010_CLA",
                               caption = "Select folder with las/laz files:")
            return (path)
        } else if (length(grep('output', caption, ignore.case = T)) != 0) {
            path <- choose.dir(default = "C:/GitHub/LIDAR_pRo/OUTPUT",
                               caption = "Select output folder:")
            return (path)
        } else {
            path <- choose.dir()
            return (path)
        }
    } else if (Sys.info()[[1]] == "Linux"){
        path <- rstudioapi::selectDirectory(caption = caption)
        return (path)
    } else {
        return ("Unknown choose_dir system")
    }
}

#_______________________________________________________________________________
#Functions for 01_DEMS
#-------------------------------------------------------------------------------
#TSE 
#Terrain, surface and elevation rasters from lidar classified las or laz
TSE <- function (lasfile , res = 1, method = 1, epsg = "+init=epsg:25829", 
                 output = "C:/GitHub/LIDAR_pRo/OUTPUT") {
    
    lasName <- tools::file_path_sans_ext(basename(lasfile)) #lasname
    las <- readLAS(lasfile, filter = "-drop_z_below 0") #read and filter below 0 values
    
    #methods for grid_terrain from fastest to slowest
    method <- ifelse(method == 1, "delaunay", ifelse(method == 2, "knnidw", "kriging")) 
    
    # DTM
    DTM <- as.raster(grid_terrain(las, res = res, method = method)) 
    crs(DTM) <- epsg
    writeRaster(DTM, filename=paste(output,"/", "DTM_", lasName, ".tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    
    # DSM here
    DSM <- NA
    
    # DEM 
    lasnormalize(las,DTM)
    DEM <- as.raster(grid_canopy(las, res = res)) #DEM
    crs(DEM) <- epsg
    writeRaster(DEM, filename=paste(output,"/", "DEM_", lasName, ".tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    
    return (list(lasName, DTM, DSM, DEM))
}

#-------------------------------------------------------------------------------
#Trees tops (PRUEBAS)
TreeTops <- function (cluster , ws) {
    
    las <- readLAS(cluster)
    if (is.empty(las)) return(NULL)
    # Find the tree tops using a user-developed method (here simply a LMF).
    ttops <- tree_detection(las, lmf(ws))
    # ttops is a SpatialPointsDataFrame that contains the tree tops in our region of interest
    # plus the trees tops in the buffered area. We need to remove the buffer otherwise we will get
    # some trees more than once.
    bbox  <- raster::extent(cluster)
    ttops <- raster::crop(ttops, bbox)
    return(ttops)
}

#-------------------------------------------------------------------------------
# rumple (PRUEBAS)
rumple_index_surface = function(cluster, res)
{
    las = readLAS(cluster)
    if (is.empty(las)) return(NULL)
    las    <- lasfiltersurfacepoints(las, 1)
    rumple <- grid_metrics(las, rumple_index(X,Y,Z), res)
    bbox   <- raster::extent(cluster)
    rumple <- raster::crop(rumple, bbox)
    return(rumple)
}
#_______________________________________________________________________________
#Functions for 02_LAZS_class



#_______________________________________________________________________________
#Functions for 03_R-VIS



#_______________________________________________________________________________
#Functions for R-Vis-3D