source("00_Base.R")

lasdir <- choose_folder(caption = "Select las dir:") #select dir to las or laz
output <- choose_folder(caption = "Select output dir:") #output dir

#Crear catálogo - hacer que los clusters coincidan con las dimensiones de las teselas 
#esto sirve para que haya los mismos archivos que procesos
#ojo porque no se ha encontrado como filtrar valores en el catalogo
cat <- catalog(lasdir)
cores(cat) <- 3
buffer(cat) <- 20
tiling_size(cat) <- round(cat@data$`Max X`[1] - cat@data$`Min X`[1])
output_files(cat) <- output

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