source("00_Base.R")

lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
output <- choose_folder(caption = "Select output dir:") #output dir

lasdir <- "C:/Users/cac/Google Drive/LIDAR/PNOA_2010_Lote5_CYL/CLA-CIR"
output <- "C:/Users/cac/Google Drive/LIDAR/PNOA_2010_Lote5_CYL/CLA-CIR"
res = 1

#set catalog and global options
cat <- catalog(lasdir)
opt_chunk_buffer(cat) <- 20 #change buffer size
opt_cores(cat) <- 3 #change cores option
opt_progress(cat) <- TRUE #see progress
opt_laz_compression(cat) <- TRUE #laz compression
summary(cat)

#-------------------------------------------------------------------------------
#MDT PROCESS
MDT_output <- paste0(output,"/MDT/")
opt_output_files(cat) <- paste0(MDT_output,"{ORIGINALFILENAME}") #set filepaths
MDT <- grid_terrain(cat, res = res, algorithm = "knnidw"(k = 5, p = 2)) 
#hillshade for MDT
slope = terrain(MDT, opt='slope')
aspect = terrain(MDT, opt='aspect')
MDT_hs = hillShade(slope, aspect, 40, 270)
#export raster files
writeRaster(MDT_hs,
            filename=paste(output,"/", "grid_terrain_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)

#-------------------------------------------------------------------------------
#MDE PROCESS
MDE_output <- paste0(output,"/MDE/")
opt_output_files(cat) <- paste0(MDE_output,"{ORIGINALFILENAME}") #set filepaths
cat
# Method 1: point to raster
MDE <- grid_canopy(cat, res = res, p2r())
# Method 2: Basic triangulation and rasterization of first returns
# MDE <- grid_canopy(cat, res = res, dsmtin())
# Method 3: Khosravipour et al. pitfree algorithm
# MDE <- grid_canopy(cat, res = res, pitfree(c(0,2,5,10,15), c(0, 1.5)))

#hillshade for MDE
slope = terrain(MDE, opt='slope')
aspect = terrain(MDE, opt='aspect')
MDE_hs = hillShade(slope, aspect, 40, 270)
#export raster files
writeRaster(MDE_hs,
            filename=paste(output,"/", "grid_canopy_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)

#-------------------------------------------------------------------------------
# MDS PROCESS
MDS_output <- paste0(output,"/MDS/")
opt_output_files(cat) <- paste0(MDS_output,"{ORIGINALFILENAME}") #set filepaths
summary(cat)

# Normalization
MDS <- lasnormalize(cat[1,], tin())

#Benchmark of normalization
system.time({ 
    opt_output_files(cat) <- paste0(MDS_output,"tile_{ID}") #set filepaths
    opt_chunk_buffer(cat) <- 10
    opt_chunk_size(cat) <- 1000
    MDS = lasnormalize(cat[1,], tin()) 
})

#Benchmark of normalization with previous retile
system.time({ 
    opt_chunk_buffer(cat) <- 10
    opt_chunk_size(cat) <- 500
    opt_output_files(cat) <- paste0(MDS_output,"tile_{ID}") #set filepaths
    cat_retile <- catalog_retile(cat[1,])
    opt_output_files(cat_retile) <- paste0(MDS_output,"{ORIGINALFILENAME}") #set filepaths
    MDS_retile <- lasnormalize(cat_retile, tin()) 
})


# Method 1: point to raster
MDS1 = grid_canopy(MDS, 1, p2r())
plot(MDS, col = height.colors(50))

# Method 2: point to raster replacing each return by a disc of x dimensions
MDS2 = grid_canopy(MDS, 1, p2r(0.5))
plot(MDS2, col = height.colors(50))

# Method 3: point to raster replacing each return by a disc of x dimensions at higher res
MDS3 = grid_canopy(MDS, 0.5, p2r(0.5))
plot(MDS3, col = height.colors(50))

# Method 4: interpolation from TIN
MDS4 = grid_canopy(MDS, 1, dsmtin())
plot(MDS4, col = height.colors(50))

# Method 5: interpolation with pitfree (algorith of interpolation an several layers)
MDS5 = grid_canopy(MDS, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0,1)))
plot(MDS5, col = height.colors(50))

# Method 6: interpolation with pitgree and subcircles
MDS6 = grid_canopy(MDS, 1, pitfree(c(0,2,5,10,15), c(0,1), subcircle = 0.5))
plot(MDS6, col = height.colors(50))

# Benchmark
opt_output_files(cat) <- paste0(MDS_output,"1") #set filepaths
system.time({ MDS1 = grid_canopy(MDS, 1, p2r()) })
opt_output_files(cat) <- paste0(MDS_output,"2") #set filepaths
system.time({ MDS2 = grid_canopy(MDS, 1, p2r(0.5)) })
opt_output_files(cat) <- paste0(MDS_output,"3") #set filepaths
system.time({ MDS3 = grid_canopy(MDS, 0.5, p2r(0.5)) })
opt_output_files(cat) <- paste0(MDS_output,"4") #set filepaths
system.time({ MDS4 = grid_canopy(MDS, 1, dsmtin()) })
opt_output_files(cat) <- paste0(MDS_output,"5") #set filepaths
system.time({ MDS5 = grid_canopy(MDS, 1, pitfree(thresholds = c(0,2,5,10,15), max_edge = c(0,1))) })
opt_output_files(cat) <- paste0(MDS_output,"6") #set filepaths
system.time({ MDS6 = grid_canopy(MDS, 1, pitfree(c(0,2,5,10,15), c(0,1), subcircle = 0.5)) })


#hillshade for MDE
slope = terrain(MDE, opt='slope')
aspect = terrain(MDE, opt='aspect')
MDE_hs = hillShade(slope, aspect, 40, 270)
#export raster files
writeRaster(MDE_hs,
            filename=paste(output,"/", "grid_canopy_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)