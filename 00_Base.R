# devtools::install_github("Jean-Romain/lidR", force = TRUE)

libraries <- c("rlang","rgeos","raster","rgdal","gridExtra","rstudioapi",
               "httpuv","leaflet","rgeos","pbapply","sp","sf","lidR","stringr")

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

#-------------------------------------------------------------------------------
#hillshade ( from raster)
hs <- function (x, alt = 40, az = 270) {
    slope = terrain(x, opt='slope')
    aspect = terrain(x, opt='aspect')
    hs = hillShade(slope, aspect, alt, az)
    return (hs)
}

#-------------------------------------------------------------------------------

#_______________________________________________________________________________
#Functions for 01_RLAZS


#_______________________________________________________________________________
#Functions for 02_RDEMS
#-------------------------------------------------------------------------------
#TSE 
#Terrain, surface and elevation rasters from lidar classified las or laz
#generates hillshades from dtm and dsm
TSE <- function (lasfile , res = 1, method = "knnidw", k = 5, p = 2,
                 epsg = "+init=epsg:25829", 
                 output = "C:/GitHub/LIDAR_pRo/OUTPUT",
                 filterLas = "-drop_z_below 0") {
    
    lasName <- tools::file_path_sans_ext(basename(lasfile)) #lasname
    
    #read and apply filter
    las = readLAS(lasfile, filter = filterLas)
    
    #mdt
    if (method == "delaunay"){
        mdt <- grid_terrain(las, res = res, method = "delaunay") # fastest
    } else if(method == "knnidw") {
        mdt <- grid_terrain(las, res = res, method = "knnidw", k = k, p = p) #medium speed
    } else if (method == "kriging"){
        mdt <- grid_terrain(las, res = res, method = "kriging", k = k) #SLOWEST
    }
    r1 <- as.raster(mdt)
    crs(r1) <- epsg
    
    #mds
    mds <- grid_canopy(las, res = res, na.fill = "knnidw", k = k, p = p)
    r2 <- as.raster(mds)
    crs(r2) <- epsg
    
    #mde
    lasnormalize(las, mdt)
    mde <- grid_canopy(las, res = res, na.fill = "knnidw", k = k, p = p)
    r3 <- as.raster(mde)
    crs(r3) <- epsg
    lasunnormalize(las)
    
    #hillshade for r1
    slope = terrain(r1, opt='slope')
    aspect = terrain(r1, opt='aspect')
    hs1 = hillShade(slope, aspect, 40, 270)
    #hillshade for r2
    slope = terrain(r2, opt='slope')
    aspect = terrain(r2, opt='aspect')
    hs2 = hillShade(slope, aspect, 40, 270)
    
    #export raster files
    writeRaster(r1,
                filename=paste(output, lasName, "_mdt.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    writeRaster(hs1,
                filename=paste(output, lasName, "_mdt_hs.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    
    writeRaster(r2,
                filename=paste(output, lasName, "_mds.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    writeRaster(hs2,
                filename=paste(output, lasName, "_mds_hs.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    
    writeRaster(r3,
                filename=paste(output, lasName, "_mde.tif", sep=''),
                datatype="FLT4S",
                overwrite=T)
    gc()
    # return (list(lasName, r1, r2, r3))
}

#-------------------------------------------------------------------------------
#TSEcatalog
#para acceder al nombre del archivo hay que usar catalog@data$filename
#Terrain, surface and elevation rasters from LIDAR catalog
#generates hillshades from dtm and dsm
TSEcatalog <- function (lascat , res = 1, method = "kriging(k = 5L)",
                 epsg = "+init=epsg:25829") {
    #mdt
    mdt <- grid_terrain(lascat, res = res, algorithm = method) #SLOWEST
    
    r1 <- as.raster(mdt)
    crs(r1) <- epsg
    
    #mds
    mds <- grid_canopy(lascat, res = res, na.fill = "knnidw", k = k, p = p)
    r2 <- as.raster(mds)
    crs(r2) <- epsg
    
    #mde
    lasnormalize(lascat, mdt)
    mde <- grid_canopy(lascat, res = res, na.fill = "knnidw", k = k, p = p)
    r3 <- as.raster(mde)
    crs(r3) <- epsg
    lasunnormalize(lascat)

    return (list(r1, r2, r3))
}

#_______________________________________________________________________________
#Functions for 03_R-VIS



#_______________________________________________________________________________
#Functions for R-Vis-3D