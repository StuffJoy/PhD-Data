# preparing sampling cell grid
# takes several minutes to run on laptop -- depending on available cores

rm(list=ls())
library(Hmisc)
library(raster)
library(parallel)

ncores = 12

source('parameters.r')

gadm = readRDS("data/cartography/ESP_adm2.rds")

gadm_prov = gadm[which(!gadm$NAME_1 %in% c("Ceuta y Melilla", "Islas Canarias")),]
proj4string(gadm_prov) = LONLAT # this only changes the format of the string so that it matches others in the code. Safe to ignore warning.
gadm_prov_utm = spTransform(gadm_prov, PROJ4_UTM_SPAIN)
grid <- raster(round((extent(gadm_prov)+1)/.1)*.1) 
cell_res = 0.025

res(grid) <- cell_res
proj4string(grid)=LONLAT
gridpolygon <- rasterToPolygons(grid)
spain_gp=gridpolygon[gadm_prov,]
ids=mclapply(1:length(spain_gp), function(i) c(paste(spain_gp[i,]@polygons[[1]]@labpt - (cell_res/2), collapse="_"),spain_gp[i,]@polygons[[1]]@ID), mc.cores=ncores)
ids_mat = do.call('rbind', ids)
spain_cells = ids_mat[,1]
spain_cells_df = data.frame(TigacellID=ids_mat[,1])
row.names(spain_cells_df) = ids_mat[,2]
spain_gpdf = SpatialPolygonsDataFrame(spain_gp,spain_cells_df)

saveRDS(spain_gpdf, 'data/cartography/spain_grid_small.Rds')