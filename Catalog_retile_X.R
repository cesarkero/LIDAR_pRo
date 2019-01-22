#setting environment
library(lidR)
library(raster)
getwd()
Owd <- getwd()
Nwd <- setwd("Z:/De sastre/CAC/github/LIDAR_products")
epsg <- "+init=epsg:25829"

#Dirs
laspath <- "Z:/De sastre/CAC/github/LIDAR_products/LIDAR_cnig/PNOA_2010_CLA/PNOA_2010_Lote10_GAL_534-4750_ORT_CLA_COL.LAZ"
lasdir <- "Z:/De sastre/CAC/github/LIDAR_products/LIDAR_cnig/PNOA_2010_CLA"
rdir <- "Z:/De sastre/CAC/github/LIDAR_products/Raster/"
#Read and display a catalog of las files
ctg <- catalog(lasdir)
ctg@crs <- sp::CRS(epsg)
ctg
plot(ctg)

#retile (1000x1000m wide and 50m buffer)
buffer(ctg) <- 0
ctg
by_file(ctg) <- F
ctg
retiles_path <- "Z:/De sastre/CAC/github/LIDAR_products/LIDAR_retiles"
newctg <- catalog_reshape(ctg,
                          size = 1000,
                          path=retiles_path,
                          prefix="Retile_",
                          ext="laz")

