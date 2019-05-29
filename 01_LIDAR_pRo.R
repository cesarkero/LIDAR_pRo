source("00_Base.R")

# lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
# output <- choose_folder(caption = "Select output dir:") #output dir

lasdir <- "C:/Users/cac/Desktop/Fernandina/LIDAR/"
outcat <- "C:/Users/cac/Desktop/Fernandina/"
output <- "C:/Users/cac/Desktop/Fernandina/LIDAR_pRo/"
epsg = "+init=epsg:25829"
res = 1

#set catalog and global options
cat <- catalog(list.files(lasdir, pattern = '*CIR.laz$', full.names = TRUE, recursive = TRUE))
# cat@crs <- sp::CRS(epsg) #COMPROBAR SI ES NECESARIO TRAS LEER EL CATALOGO
opt_chunk_buffer(cat) <- 20 #change buffer size
opt_cores(cat) <- 2 #change cores option
opt_progress(cat) <- TRUE #see progress
opt_laz_compression(cat) <- TRUE #laz compression
summary(cat)

#-------------------------------------------------------------------------------
# EXPORT CATALOG
# rgdal::writeOGR(as.spatial(cat), paste0(outcat,"catalog.shp"),"catalog", driver="ESRI Shapefile") # shp
# st_write(as.spatial(cat), paste0(outcat,"catalog.gpkg"), "LIDAR_CNIG_2015") 
writeOGR(as.spatial(cat), paste0(outcat,"catalog.gpkg"),"LIDAR_CNIG_2015", driver="GPKG") #geopackage

#-------------------------------------------------------------------------------
# Apply a function over files (NO USAR DE MOMENTO)
# Terrain, surface and elevation rasters from lidar classified las or laz
# generates hillshades from dtm and dsm
# Read and display a catalog of las files
# lasfiles <- cat@data$filename
# pblapply(lasfiles, TSE, res = res, method = "knnidw", k = 5, p = 2, epsg = epsg, 
#          output = output, filterLas = "-drop_z_below 0")

#-------------------------------------------------------------------------------
# CREATE LIDAR_pRo
#-------------------------------------------------------------------------------
# MDT PROCESS (FUNCIONA)
MDT_output <- paste0(output,"MDT_", str_pad(res, 3, "left", pad = "0"), "/")
opt_output_files(cat) <- paste0(MDT_output,"{ORIGINALFILENAME}") #set filepaths
MDT <- grid_terrain(cat, res = res, algorithm = "knnidw"(k = 5, p = 2)) 
# plot(MDT)

# hillshade for MDT
slope = terrain(MDT, opt='slope')
aspect = terrain(MDT, opt='aspect')
MDT_hs = hillShade(slope, aspect, 40, 315)
# plot(MDT_hs)
# export raster files
writeRaster(MDT_hs,
            filename=paste(MDT_output,"/", "grid_terrain_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)
gc()

#-------------------------------------------------------------------------------
# MDS PROCESS (FUNCIONA)
MDS_output <- paste0(output,"MDS_", str_pad(res, 3, "left", pad = "0"), "/")
opt_output_files(cat) <- paste0(MDS_output,"{ORIGINALFILENAME}") #set filepaths
# Method 1: point to raster. ojo, crea vacios donde no hay vegetacion.
# MDS <- grid_canopy(cat, res = res, p2r())
# Method 2: Basic triangulation and rasterization of first returns
MDS <- grid_canopy(cat, res = res, dsmtin())
# Method 3: Khosravipour et al. pitfree algorithm
# MDS <- grid_canopy(cat, res = res, pitfree(c(0,2,5,10,15), c(0, 1.5)))

# hillshade for MDE
slope = terrain(MDS, opt='slope')
aspect = terrain(MDS, opt='aspect')
MDS_hs = hillShade(slope, aspect, 40, 315)
# plot(MDS_hs)
# export raster files
writeRaster(MDS_hs,
            filename=paste(MDS_output,"/", "MDS_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)
gc()

#-------------------------------------------------------------------------------
# MDE PROCESS
MDE_output <- paste0(output,"/MDE/")
opt_output_files(cat) <- paste0(MDE_output,"{ORIGINALFILENAME}") #set filepaths

# Normalization
MDE <- lasnormalize(cat, tin())
MDE <- lasnormalize(cat[1,], tin())

#Benchmark of normalization
system.time({ 
    opt_output_files(cat) <- paste0(MDE_output,"tile_{ID}") #set filepaths
    opt_chunk_buffer(cat) <- 10
    opt_chunk_size(cat) <- 1000
    MDE = lasnormalize(cat[1,], tin()) 
})

#Benchmark of normalization with previous retile
system.time({ 
    opt_chunk_buffer(cat) <- 10
    opt_chunk_size(cat) <- 500
    opt_output_files(cat) <- paste0(MDE_output,"tile_{ID}") #set filepaths
    cat_retile <- catalog_retile(cat[1,])
    opt_output_files(cat_retile) <- paste0(MDE_output,"{ORIGINALFILENAME}") #set filepaths
    MDE_retile <- lasnormalize(cat_retile, tin()) 
})


#-------------------------------------------------------------------------------
# BENCHMARK
# Method 1: point to raster
MDE1 = grid_canopy(MDE, 1, p2r())
plot(MDE1, col = height.colors(50))

# Method 2: point to raster replacing each return by a disc of x dimensions
MDE2 = grid_canopy(MDE, 1, p2r(0.5))
plot(MDE2, col = height.colors(50))

# Method 3: point to raster replacing each return by a disc of x dimensions at higher res
MDE3 = grid_canopy(MDE, 0.5, p2r(0.5))
plot(MDE3, col = height.colors(50))

# Method 4: interpolation from TIN (rapido y no crea agujeros)
MDE4 = grid_canopy(MDE, 1, dsmtin())
plot(MDE4, col = height.colors(50))

# Method 5: interpolation with pitfree (algorith of interpolation an several layers)
MDE5 = grid_canopy(MDE, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0,1)))
plot(MDE5, col = height.colors(50))

# Method 6: interpolation with pitgree and subcircles
MDE6 = grid_canopy(MDE, 1, pitfree(c(0,2,5,10,15), c(0,1), subcircle = 0.5))
plot(MDE6, col = height.colors(50))

# Benchmark
opt_output_files(cat) <- paste0(MDE_output,"1") #set filepaths
system.time({ MDE1 = grid_canopy(MDE, 1, p2r()) })
opt_output_files(cat) <- paste0(MDE_output,"2") #set filepaths
system.time({ MDE2 = grid_canopy(MDE, 1, p2r(0.5)) })
opt_output_files(cat) <- paste0(MDE_output,"3") #set filepaths
system.time({ MDE3 = grid_canopy(MDE, 0.5, p2r(0.5)) })
opt_output_files(cat) <- paste0(MDE_output,"4") #set filepaths
system.time({ MDE4 = grid_canopy(MDE, 1, dsmtin()) })
opt_output_files(cat) <- paste0(MDE_output,"5") #set filepaths
system.time({ MDE5 = grid_canopy(MDE, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0,1))) })
opt_output_files(cat) <- paste0(MDE_output,"6") #set filepaths
system.time({ MDE6 = grid_canopy(MDE, 1, pitfree(c(0,2,5,10,15), c(0,1), subcircle = 0.5)) })


#hillshade for MDE
slope = terrain(MDS, opt='slope')
aspect = terrain(MDS, opt='aspect')
MDE_hs = hillShade(slope, aspect, 40, 270)
#export raster files
writeRaster(MDE_hs,
            filename=paste(output,"/", "MDE_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)