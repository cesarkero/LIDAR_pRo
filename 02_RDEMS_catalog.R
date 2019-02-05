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


#funcion sobre catalog para producir mdt unido y su hillshades
#ojo, al ejecutar sobre muchos archivos pues generaría tifs elefantiásicos
mdt <- grid_terrain(cat, res = 1, method = "knnidw", k = 5, p = 2) #medium speed
r1 <- as.raster(mdt)
crs(r1) <- "+init=epsg:25829"

#hillshade for r1
slope = terrain(r1, opt='slope')
aspect = terrain(r1, opt='aspect')
hs1 = hillShade(slope, aspect, 40, 270)

#export raster files
writeRaster(r1,
            filename=paste(output,"/", "MDT.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)
writeRaster(hs1,
            filename=paste(output,"/", "MDT_hs.tif", sep=''),
            datatype="FLT4S",
            overwrite=T)

