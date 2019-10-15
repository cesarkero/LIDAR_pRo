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
hs <- function (x, alt = 40, az = 315) {
    slope = terrain(x, opt='slope')
    aspect = terrain(x, opt='aspect')
    hs = hillShade(slope, aspect, alt, az)
    return (hs)
}

#-------------------------------------------------------------------------------
#Get the folderpath of a filepath or folderpath
folderpath <- function(path) {
    x <- setdiff(strsplit(path,"/|\\\\")[[1]], "")
    paste0(paste(x[-length(x)], collapse = "/"),"/")
}

#_______________________________________________________________________________
# Functions for CATALOG
#_______________________________________________________________________________

#-------------------------------------------------------------------------------
# LASCATALOG
#-------------------------------------------------------------------------------
# Creates a catalog with all files with pattern, returns catalog and writes gpkg
# lasdir is a folder where .laz files are (is going to be mapped recursively)
# outputdir is a folder where all lidar product will be saved
# pattern will filter the lidar files based on extension or something in the filename
# clipcat to execute or not a clip over the catalog
# clipcatbuf in case you want to clip with an extra buffer over the clipcatshape
# clipcatshape is the shp or gpkg to make the clip 
lascatalog <- function(lasdir, outputdir, pattern = '*COL.laz$|*COL.LAZ$', catname = "Catalog_COL",
                       clipcat = TRUE, clipcatbuf = FALSE, clipbuf = 1000, clipcatshape,
                       cat_chunk_buffer = 20,
                       cores = 4,
                       progress = TRUE,
                       laz_compression = TRUE,
                       epsg = "+init=epsg:25829",
                       retilecatalog = FALSE, tile_chunk_buffer = 10, tile_chunk_size = 1000,
                       filterask = FALSE, filter = "-keep_first -drop_z_below 2"){
    
    cat <- catalog(list.files(path = lasdir, recursive = TRUE, pattern = pattern, full.names = TRUE))
    projection(cat) <- epsg
    opt_progress(cat) <- progress #see progress
    opt_laz_compression(cat) <- laz_compression #laz compression
    opt_cores(cat) <- cores #change cores option
    
    if (clipcat == TRUE) {
        opt_output_files(cat) <- paste0(output, "Catalog_clip")
        if (clipcatbuf == TRUE) {
            cat = lasclip(cat, buffer(clipcatshape, width=1000)) #clip from buffer
        } else {
            cat = lasclip(cat, clipcatshape)
        }
    }
    
    if (retilecatalog == FALSE){
        opt_chunk_buffer(cat) <- cat_chunk_buffer #change buffer size
        writeOGR(as.spatial(cat), paste0(outputdir,paste0(catname,".gpkg")),"catalog",driver="GPKG") #geopackage
        save(cat, file = paste0(lasdir,paste0(catname,".RData")))
        return(cat)
    } else {
        #crear nueva carpeta de retile en output
        name <- paste0(output, basename(lasdir),"_retile_", toString(tile_chunk_size), "/")
        dir.create(name)
        cat <- catalog(list.files(path = lasdir, pattern = pattern, full.names = TRUE, recursive = TRUE))
        projection(cat) <- epsg
        opt_output_files(cat) <- paste0(name, basename(lasdir), "_","{XLEFT}","_","{YTOP}")
        opt_chunk_buffer(cat) <- tile_chunk_buffer
        opt_chunk_size(cat) <- tile_chunk_size
        if (filterask == TRUE){
            opt_filter(cat) <- filter
        }
        opt_laz_compression(cat) <- laz_compression #laz compression
        newcat <- catalog_retile(cat)
        writeOGR(as.spatial(newcat), paste0(name,paste0(catname,".gpkg")),"catalog", driver="GPKG") #geopackage
        save(cat, file = paste0(lasdir,paste0(catname,".RData")))
        return(newcat)
    }
}

#-------------------------------------------------------------------------------
# RETILE CATALOG (TO HIGHER OR SMALLER EXTENSION)
#-------------------------------------------------------------------------------
# INCORPORADO A LA FUNCION ANTERIOR
retilecatalog <- function(lasdir, buffer = 10, chunk_size = 500,
                          filter = "-keep_first -drop_z_below 2", lazcomp = TRUE){
    cat <- lascatalog()
    opt_output_files(cat) <- paste0(name, basename(lasdir), "_","{XLEFT}","_","{YTOP}")
    opt_chunk_buffer(cat) <- buffer
    opt_chunk_size(cat) <- chunk_size
    opt_filter(cat) <- filter
    opt_laz_compression(cat) <- lazcomp
    newcat <- catalog_retile(cat)
    return()
}

#_______________________________________________________________________________
#Functions for PRODUCTS
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