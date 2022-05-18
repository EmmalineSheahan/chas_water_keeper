library(raster)
library(rgdal)
library(dplyr)
library(caTools)
library(rgeos)

source('./scripts/run_first_functions.R')
load('./data/wk_master.Rdata')

# impervious surface data
imperv <- raster('./urban/LC16IMP.tif')
imperv
plot(imperv)

imperv_crs <- proj4string(imperv)
imperv_map <- spTransform(new_crop, CRSobj = crs(imperv_crs))
extent(imperv_map)
new_extent <- extent(matrix(c(2304600, 2354720, 298739, 390531), nrow = 2, byrow = T))

imperv_crop <- crop(imperv, new_extent)
plot(imperv_crop)
plot(imperv_map, add = T)

imperv2 <- projectRaster(from = imperv_crop, crs = crs(wanted_crs_2))
imperv3 <- mask(imperv2, new_crop)

pdf('./figures/urbanization_by_site.pdf')
plot(imperv3)
plot(new_crop, add = T)
plot(wk_sp, col = "red", pch = 10, cex = 1.3, add = T)
dev.off()

# creating buffer around points
buff <- gBuffer(wk_sp, width = 0.01)

plot(imperv3)
plot(new_crop, add = T)
plot(wk_sp, col = "red", pch = 10, cex = 1.3, add = T)
plot(buff, col = "blue", add = T)

# extracting values within buffer
t <- extract(imperv3, buff)

# creating list of spatial points which correspond to each site
View(wk_data)
sites <- data.frame(wk_data$Sample_ID, wk_data$Longitude, wk_data$Latitude)
sites <- sites[1:20,]
colnames(sites) <- c("Sample_ID", "Longitude", "Latitude")

site_sp_list <- vector("list", length = dim(sites)[1])
for (i in 1:20) {
  t <- sites[i,]
  coordinates(t) <- ~Longitude+Latitude
  proj4string(t) <- wanted_crs_2
  site_sp_list[[i]] <- t
}

# get average impervious surface value for all pixels inside the buffer for each site
imperv_list <- vector("list", length = 20) 
  for (i in 1:20) {
    buff <- gBuffer(site_sp_list[[i]], width = 0.01)
    imperv_values <- unlist(extract(imperv3, buff))
    imperv_mean <- mean(imperv_values, na.rm = T)
    final <- c(site_sp_list[[i]]$Sample_ID, imperv_mean)
    imperv_list[[i]] <- final
  }

imperv_vec <- matrix(unlist(imperv_list), ncol = 2, byrow = T)
imperv_df <- data.frame(imperv_vec)
colnames(imperv_df) <- c("Sample_ID", "Mean_Impervious_Surface")
imperv_df$Mean_Impervious_Surface <- as.numeric(imperv_df$Mean_Impervious_Surface)

Impervious_Index <- c("Mid", "Mid", "Low", "Low", "Low", "Low", "High", "Low", "Low", "High", "Mid",
                        "Mid", "Low", "Low", "Mid", "Mid", "Mid", "Low", "Low", "Mid")
imperv_df <- cbind(imperv_df, Impervious_Index)

save(imperv_df, file = './data/imperv_df.Rdata')
write.csv(imperv_df, file = './data/impervious_surface_data.csv')

# developed area
developed <- raster('./urban/LC16DEV.tif')

developed_crs <- proj4string(developed)
developed_map <- spTransform(new_crop, CRSobj = crs(developed_crs))
extent(developed_map)
new_extent <- extent(matrix(c(2304600, 2354720, 298739, 390531), nrow = 2, byrow = T))

developed_crop <- crop(developed, new_extent)
plot(developed_crop)
plot(developed_map, add = T)

developed2 <- projectRaster(from = developed_crop, crs = crs(wanted_crs_2))
developed3 <- mask(developed2, new_crop)

pdf('./figures/developed_area_by_site.pdf')
plot(developed3)
plot(new_crop, add = T)
plot(wk_sp, col = "red", pch = 10, cex = 1.3, add = T)
dev.off()

# checking degree of correlation between impervious surface and developed area
urb_cor <- corLocal(imperv3, developed3)
pdf('./figures/urb_cor.pdf')
plot(urb_cor)
plot(new_crop, add = T)
plot(wk_sp, col = "red", pch = 10, cex = 1.3, add = T)
dev.off()