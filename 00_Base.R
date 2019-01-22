libraries <- c("rgeos","lidR", "raster", "rgdal", "gridExtra", "rgdal", "rstudioapi",
               "httpuv", "leaflet", "mapview", "rgeos")

lapply(libraries, function (x) {if(x %in% rownames(installed.packages()) == FALSE) {install.packages(x)}})
lapply(libraries, require,  character.only=T)

#_______________________________________________________________________________
#General functions
choose_folder = function(caption = 'Select _______ file/directory:') {
    if (Sys.info()[[1]] == "Windows") {
        if (length(grep('lidar | las | laz', caption, ignore.case = T)) != 0) {
            path <- choose.dir(default = "C:/Users/cac/Dropbox/GitHub_Workout/LIDAR_pRo/LIDAR/PNOA_2010_CLA",
                               caption = "Select folder with las/laz files:")
            return (path)
        } else if (length(grep('output', caption, ignore.case = T)) != 0) {
            path <- choose.dir(default = "C:/Users/cac/Dropbox/GitHub_Workout/LIDAR_pRo/OUTPUT",
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

#_______________________________________________________________________________
#Functions for 01_DEMS



#_______________________________________________________________________________
#Functions for 02_LAZS_class



#_______________________________________________________________________________
#Functions for 03_R-VIS



#_______________________________________________________________________________
#Functions for R-Vis-3D