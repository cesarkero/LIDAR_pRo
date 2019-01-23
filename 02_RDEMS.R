source("00_Base.R")

lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
output <- choose_folder(caption = "Select output dir:") #output dir
files <- list.files(lasdir, pattern="*.LAZ|*.LAS", full.names=T, recursive = T) #creates a list of files
files.cat <- catalog(lasdir)

#Function could be done in parallel maybe

Rasters <- lapply(files.cat, TSE, res = 1, method = 1, epsg = "+init=epsg:25829" )
res=0.25
method = "delaunay"
epsg = "+init=epsg:25829"
lasName <- tools::file_path_sans_ext(basename(files[1]))
DTM <- as.raster(grid_terrain(files.cat, res = res, method = method)) 
DTM
crs(DTM) <- epsg
writeRaster(DTM, filename=paste(output,"/", "DTM_", lasName, ".tif", sep=''),
            datatype="FLT4S",
            overwrite=T)
#TreeTops
# For this dummy example, the chunk size is 80 m and the buffer is 10 m using a single core.

opt_chunk_buffer(files.cat) <- 10
opt_cores(files.cat)        <- 1L
opt_chunk_size(files.cat)   <- 80            # small because this is a dummy example.
opt_select(files.cat)       <- "xyz"         # read only the coordinates.

opt    <- list(raster_alignment = 20)   # catalog_apply will throw an error if buffer = 0
output <- catalog_apply(project, my_tree_detection_method, ws = 5, .options = opt)
Trees <- catalog_apply(files.cat, TreeTops, ws=1)
