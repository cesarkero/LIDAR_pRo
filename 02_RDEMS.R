source("00_Base.R")

lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
output <- choose_folder(caption = "Select output dir:") #output dir
files <- list.files(lasdir, pattern="*.LAZ|*.LAS", full.names=T, recursive = T) #creates a list of files
files.cat <- catalog(lasdir)
class(files.cat)

#Function could be done in parallel maybe
Rasters <- lapply(files, TSE, res = 1, method = 1, epsg = "+init=epsg:25829" )

#TreeTops
# For this dummy example, the chunk size is 80 m and the buffer is 10 m using a single core.
?catalog
opt_chunk_buffer(files.cat) <- 10
opt_cores(files.cat)        <- 1L
opt_chunk_size(files.cat)   <- 80            # small because this is a dummy example.
opt_select(files.cat)       <- "xyz"         # read only the coordinates.
opt_filter(files.cat)       <- "-keep_first" # read only first returns.

opt    <- list(need_buffer = TRUE)   # catalog_apply will throw an error if buffer = 0
output <- catalog_apply(project, my_tree_detection_method, ws = 5, .options = opt)
Trees <- catalog_apply(files.cat, TreeTops, ws=1)
