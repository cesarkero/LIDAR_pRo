source("00_Base.R")

lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
output <- choose_folder(caption = "Select output dir:") #output dir

#Crear catálogo - hacer que los clusters coincidan con las dimensiones de las teselas 
#esto sirve para que haya los mismos archivos que procesos
#ojo porque no se ha encontrado como filtrar valores en el catalogo
cat <- catalog(list.files(lasdir, pattern = '*COL.laz$', full.names = TRUE, recursive = TRUE))
opt_chunk_buffer(cat) <- 20 #change buffer size
opt_cores(cat) <- 3 #change cores option
opt_progress(cat) <- TRUE #see progress
opt_laz_compression(cat) <- TRUE #laz compression
opt_output_files(cat) <- output #output files
summary(cat)

rasters <- catalog_apply(cat, TSEcatalog, res = 1, method = "knnidw", epsg = "+init=epsg:25829", 
         output = output)



#export raster files
writeRaster(r1,
            filename=paste(output,"/", lasName, "_mdt.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)
writeRaster(hs1,
            filename=paste(output,"/", lasName, "_mdt_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)

writeRaster(r2,
            filename=paste(output,"/", lasName, "_mds.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)
writeRaster(hs2,
            filename=paste(output,"/", lasName, "_mds_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)

writeRaster(r3,
            filename=paste(output,"/", lasName, "_mde.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)