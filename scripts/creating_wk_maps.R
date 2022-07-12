library(maps)
library(maptools)
library(rgdal)
library(raster)
library(dplyr)
library(gstat)
library(rgeos)
library(ggplot2)
library(ggspatial)
library(RColorBrewer)

source('./scripts/run_first_functions.R')
load('./data/wk_master.Rdata')

# testing interpolated grid
  x_range <- as.numeric(c(-80.0075, -79.84617))
  y_range <- as.numeric(c(32.65034, 32.90124))
  grd_wk <- expand.grid(x = seq(from = x_range[1],
                             to = x_range[2], 
                             by = 0.005),
                     y = seq(from = y_range[1], to = y_range[2], 
                             by = 0.005))  # expand points to grid
  class(grd_wk)
  
  coordinates(grd_wk) <- ~x + y
  gridded(grd_wk) <- TRUE
  plot(grd_wk, cex = 1.5, col = "grey")
  
  temp_data_test <- wk_data %>% filter(Date_Extracted == "7/14/2021")
  coordinates(temp_data_test) <- ~Longitude+Latitude
  
  idw_pow1 <- gstat::idw(formula = Chlorophyll_conc ~ 1,
                         locations = temp_data_test,
                         newdata = grd_wk,
                         idp = 1)
  wanted_ras <- raster(idw_pow1)
  proj4string(wanted_ras) <- wanted_crs_2
  
  pdf('./figures/test.pdf')
  plot(wanted_ras,
       col = terrain.colors(55, rev = T), main = "test title",
       legend.args = list(text = "Legend title", side = 3, line = 1), horizontal = T, axes = F, box = F)
  title(sub = "subtitle", adj = 0.5, line = -23.5)
  plot(new_crop, add = T, col = "white", border = "black")
  plot(wk_sp, col = "red", pch = 10, cex = 1.3, add = T)
  dev.off()

# testing make_cwk_map function
max(wk_data$Chlorophyll_conc, na.rm = T)
min(wk_data$Chlorophyll_conc, na.rm = T)
chl_range <- c(0,50)

make_cwk_map(wk_data, "7/14/2021", 'Chlorophyll_conc', chl_range, is.reversed = F, wanted_bias = 1.3,
             "function_test", "Chlorophyll Concentration", "7/14/2021", 
             "Chlorophyll Concentration (μg/L)")

date_list <- unique(wk_data$Date_Extracted)
date_hyph <- gsub(pattern = '/', x = date_list, replacement = '-')

# creating chlorophyll maps
# first have to find min and max raster cell values
which(wk_master$Chlorophyll_conc == min(wk_master$Chlorophyll_conc, na.rm = T))
wk_master[51,]

# min week is 9/29/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "9/29/2021") %>% 
  filter(!is.na(Chlorophyll_conc))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Chlorophyll_conc ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# the min of the range is 2.44674

which(wk_master$Chlorophyll_conc == max(wk_master$Chlorophyll_conc, na.rm = T))
wk_master[38,]

# max week is 6/30/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "6/30/2021") %>% 
  filter(!is.na(Chlorophyll_conc))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Chlorophyll_conc ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max of the range is 32.06804

chl_range <- c(2.44674, 32.06804)
date_list <- unique(wk_master$Date_Extracted)
date_hyph <- gsub(pattern = '/', x = date_list, replacement = '-')

pdf('./figures/Chlorophyll_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'Chlorophyll_conc', chl_range, is.reversed = F, wanted_bias = 1.3,
               "Chlorophyll Concentration", date_list[i], 
               "Chlorophyll Concentration (μg/L)")
}
dev.off()

# chlorophyll for waterkeeper presentation
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
chl_range1 <- c(1,12)

for (i in seq_along(date1)) {
  make_cwk_map(wk_data, date1[i], 'Chlorophyll_conc', chl_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_chlorophyll_cwk'), 
               "Chlorophyll Concentration", date1[i], 
               "Chlorophyll Concentration (μg/L)")
}

# creating phosphate maps
# finding min and max values for scale
which(wk_master$Phosphate == max(wk_master$Phosphate, na.rm = T))
wk_master[263,]

# max date is 8/4/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/4/2021") %>% 
  filter(!is.na(Phosphate))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Phosphate ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 3.582951

which(wk_master$Phosphate == min(wk_master$Phosphate, na.rm = T))
wk_master[184,]

#min week is 7/7/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "7/7/2021") %>% 
  filter(!is.na(Phosphate))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Phosphate ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 0.005751042

phos_range = c(0.005751042, 3.582951)
date_list2 <- date_list[10:26]
date_hyph2 <- date_hyph[10:26]

pdf('./figures/Phosphate_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'Phosphate', phos_range, is.reversed = F, wanted_bias = 1.6,
               "Phosphate Concentration", date_list2[i], 
               "Phosphate (uM)")
}
dev.off()

# phophate maps for waterkeeper presentation
phos_range1 <- c(0,0.8)

for (i in seq_along(date1)) {
  make_cwk_map(wk_data, date1[i], 'Phosphate', phos_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_phosphate_cwk'), 
               "Phosphate Concentration", date1[i], 
               "Phosphate (uM)")
}

# creating Nitrate maps
# finding min and max values for scale
which(wk_master$Nitrate == max(wk_master$Nitrate, na.rm = T))
wk_master[243,]

# max date is 7/28/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "7/28/2021") %>% 
  filter(!is.na(Nitrate))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Nitrate ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 34.53649

which(wk_master$Nitrate == min(wk_master$Nitrate, na.rm = T))
wk_master[469,]

#min week is 10/13/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/13/2021") %>% 
  filter(!is.na(Nitrate))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Nitrate ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 1.871686

nit_range = c(1.871686, 34.53649)
date_list2 <- date_list[10:26]
date_hyph2 <- date_hyph[10:26]

pdf('./figures/Nitrate_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'Nitrate', nit_range, is.reversed = F, wanted_bias = 1.2,
               "Nitrate Concentration", date_list2[i], 
               "Nitrate (uM)")
}
dev.off()


# creating temperature maps
which(wk_master$Temp == max(wk_master$Temp, na.rm = T))
wk_master[288,]

# 8/11/2021 is the max week
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/11/2021") %>% 
  filter(!is.na(Temp))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Temp ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 31.07203

which(wk_master$Temp == min(wk_master$Temp, na.rm = T))
wk_master[490,]

# 10/20/2021 is the min week
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/20/2021") %>% 
  filter(!is.na(Temp))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Temp ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 20.79316

temp_range <- c(20.79316, 31.07203)

pdf('./figures/Temperature_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'Temp', temp_range, is.reversed = F, wanted_bias = 1, 
               "Water Temperature", date_list[i], 
               "Temperature (°C)")
}
dev.off()

# creating salinity maps
which(wk_master$Salinity == max(wk_master$Salinity, na.rm = T))
wk_master[489,]

# 10/20/2021 is the max week
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/20/2021") %>% 
  filter(!is.na(Salinity))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Salinity ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 34.77527

which(wk_master$Salinity == min(wk_master$Salinity, na.rm = T))
wk_master[310,]

# min week is 8/18/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/18/2021") %>% 
  filter(!is.na(Salinity))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Salinity ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 3.336175

sal_range = c(3.336175,34.77527)

date_list3 <- date_list[7:26]
date_hyph3 <- date_hyph[7:26]

pdf('./figures/Salinity_All_Weeks.pdf')
for (i in seq_along(date_list3)) {
  make_cwk_map(wk_master, date_list3[i], 'Salinity', sal_range, is.reversed = F, wanted_bias = 1,
               "Salinity", date_list3[i], 
               "Salinity (ppt)")
}
dev.off()

# creating tidal height maps
which(wk_master$Tidal_height == max(wk_master$Tidal_height, na.rm = T))
wk_master[443,]

# max week is 10/6/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/6/2021") %>% 
  filter(!is.na(Tidal_height))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Tidal_height ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 6.740524

which(wk_master$Tidal_height == min(wk_data$Tidal_height, na.rm = T))
wk_master[236,]

# min week is 7/21/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "7/21/2021") %>% 
  filter(!is.na(Tidal_height))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Tidal_height ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 0.2042045

tide_range <- c(0.2042045, 6.740524)

pdf('./figures/Tidal_height_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'Tidal_height', tide_range, is.reversed = F, wanted_bias = 1,
               "Tidal Height", date_list[i], 
               "Tidal Height (ft)")
}
dev.off()

# phycocyanin maps
wk_master2 <- wk_master[1:500,]
which(wk_master2$Phycocyanin_conc == max(wk_master2$Phycocyanin_conc, na.rm = T))
wk_master2[490,]

# max week is 10/20/2021, leaving out 10/27/2021 because its an outliar
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/20/2021") %>% 
  filter(!is.na(Phycocyanin_conc))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Phycocyanin_conc ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 1.732977

which(wk_master2$Phycocyanin_conc == min(wk_master2$Phycocyanin_conc, na.rm = T))
wk_master[427,]

# min week is 9/29/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "9/29/2021") %>% 
  filter(!is.na(Phycocyanin_conc))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Phycocyanin_conc ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 0.004500305
date_list4 <- date_list[-8]
date_list5 <- date_list4[-25]
pc_range1 <- c(0.004500305, 1.732977)

pdf('./figures/Phycocyanin_All_Weeks.pdf')
for (i in seq_along(date_list5)) {
  make_cwk_map(wk_master, date_list4[i], 'Phycocyanin_conc', pc_range1, is.reversed = F, wanted_bias = 1.5,
               "Phycocyanin Concentration", date_list4[i], 
               "Phycocyanin Concentration (μg/L)")
}
dev.off()

# Phycoerytherin maps
which(wk_master$Phycoerytherin_conc == max(wk_master$Phycoerytherin_conc, na.rm =T))
wk_master[310,]

# max week is 8/18/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/18/2021") %>% 
  filter(!is.na(Phycoerytherin_conc))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Phycoerytherin_conc ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is  0.04760804

which(wk_master$Phycoerytherin_conc == min(wk_master$Phycoerytherin_conc, na.rm = T))
wk_master[1,]

# min week is 4/28/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "4/28/2021") %>% 
  filter(!is.na(Phycoerytherin_conc))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Phycoerytherin_conc ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 0.0003884927

phycoerytherin_range <- c(0.0003884927,0.04760804)

pdf('./figures/Phycoerytherin_All_Weeks.pdf')
for (i in seq_along(date_list4)) {
  make_cwk_map(wk_master, date_list4[i], 'Phycoerytherin_conc', phycoerytherin_range, is.reversed = F, wanted_bias = 1.5,
               "Phycoeytherin Concentration", date_list4[i], 
               "Phycoerytherin Concentration (μg/L)")
}
dev.off()

# phycoerytherin for waterkeeper presentation
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
pe_range1 <- c(0.00127,0.0173)

for (i in seq_along(date1)) {
  make_cwk_map(wk_data, date1[i], 'Phycoerytherin_conc', pe_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_phycoerytherin_cwk'), 
               "Phycoerytherin Concentration", date1[i], 
               "Phycoerytherin Concentration (μg/L)")
}

# PE_PC_ratio maps
which(wk_master$PE_PC_ratio == max(wk_master$PE_PC_ratio, na.rm =T))
wk_master[215,]

# max week is 7/14/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "7/14/2021") %>% 
  filter(!is.na(PE_PC_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PE_PC_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 0.6281306

which(wk_master$PE_PC_ratio == min(wk_master$PE_PC_ratio, na.rm =T))
wk_master[425,]

# min week is 9/29/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "9/29/2021") %>% 
  filter(!is.na(PE_PC_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PE_PC_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is -0.8019533

PE_PC_range <- c(-0.8019533, 0.6281306)

pdf('./figures/PE_PC_ratio_All_Weeks.pdf')
for (i in seq_along(date_list4)) {
  make_cwk_map(wk_master, date_list4[i], 'PE_PC_ratio', PE_PC_range, is.reversed = F, wanted_bias = 1,
               "Phycoeytherin to Phycocyanin Ratio", date_list4[i], 
               "PE:PC")
}
dev.off()

# PE_Chla_ratio maps
which(wk_master$PE_Chla_ratio == max(wk_master$PE_Chla_ratio, na.rm =T))
wk_master[310,]

# max week is 8/18/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/18/2021") %>% 
  filter(!is.na(PE_Chla_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PE_Chla_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 0.02040985

which(wk_master$PE_Chla_ratio == min(wk_master$PE_Chla_ratio, na.rm =T))
wk_master[426,]

# min week is 9/29/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "9/29/2021") %>% 
  filter(!is.na(PE_Chla_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PE_Chla_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is -0.0001327357

PE_Chla_range <- c(-0.0001327357, 0.02040985)

pdf('./figures/PE_Chla_ratio_All_Weeks.pdf')
for (i in seq_along(date_list4)) {
  make_cwk_map(wk_master, date_list4[i], 'PE_Chla_ratio', PE_Chla_range, is.reversed = F, wanted_bias = 1,
               "Phycoeytherin to Chlorophyll a Ratio", date_list4[i], 
               "PE:Chlorophyll a")
}
dev.off()

# PC_Chla_ratio maps
which(wk_master2$PC_Chla_ratio == max(wk_master2$PC_Chla_ratio, na.rm =T))
wk_master[490,]

# max week is 10/20/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/20/2021") %>% 
  filter(!is.na(PC_Chla_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PC_Chla_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 0.4214755

which(wk_master2$PC_Chla_ratio == min(wk_master$PC_Chla_ratio, na.rm =T))
wk_master[427,]

# min week is 9/29/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "9/29/2021") %>% 
  filter(!is.na(PC_Chla_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PC_Chla_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is -0.03899377

PC_Chla_range <- c(-0.03899377, 0.4214755)

pdf('./figures/PC_Chla_ratio_All_Weeks.pdf')
for (i in seq_along(date_list5)) {
  make_cwk_map(wk_master, date_list5[i], 'PC_Chla_ratio', PC_Chla_range, is.reversed = F, wanted_bias = 1,
               "Phycocyanin to Chlorophyll a Ratio", date_list5[i], 
               "PC:Chlorophyll a")
}
dev.off()

# N:P ratio maps
which(wk_master$N_to_P_ratio == max(wk_master$N_to_P_ratio, na.rm =T))
wk_master[484,]

# max week is 10/20/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "10/20/2021") %>% 
  filter(!is.na(N_to_P_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = N_to_P_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 657.4483

which(wk_master$N_to_P_ratio == min(wk_master$N_to_P_ratio, na.rm =T))
wk_master[509,]

# min week is 7/7/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "7/7/2021") %>% 
  filter(!is.na(N_to_P_ratio))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = N_to_P_ratio ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 7.22204

NP_range <- c( 7.22204, 657.4483)

pdf('./figures/Nitrate_to_Phosphate_ratio_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'N_to_P_ratio', NP_range, is.reversed = F, wanted_bias = 1,
               "Nitrate to Phosphate Ratio", date_list2[i], 
               "N:P")
}
dev.off()

# pH maps
which(wk_master$pH == max(wk_master$pH, na.rm =T))
wk_master[270,]

# max week is 8/4/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/4/2021") %>% 
  filter(!is.na(pH))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = pH ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 9.230936

which(wk_master$pH == min(wk_master$pH, na.rm =T))
wk_master[295,]

# min week is 8/11/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/11/2021") %>% 
  filter(!is.na(pH))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = pH ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is 7.826373

ph_range <- c( 7.826373, 9.230936)

pdf('./figures/pH_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'pH', ph_range, is.reversed = F, wanted_bias = 1,
               "pH", date_list2[i], 
               "pH")
}
dev.off()

# Alkalinity maps
which(wk_master$Alkalinity == max(wk_master$Alkalinity, na.rm =T))
wk_master[269,]

# max week is 8/4/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "8/4/2021") %>% 
  filter(!is.na(Alkalinity))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Alkalinity ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max value is 3.414022

which(wk_master$Alkalinity == min(wk_master$Alkalinity, na.rm =T))
wk_master[232,]

# min week is 7/21/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "7/21/2021") %>% 
  filter(!is.na(Alkalinity))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Alkalinity ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# min value is -1.440647

alk_range <- c( -1.440647, 3.414022)

pdf('./figures/Alkalinity_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'Alkalinity', alk_range, is.reversed = F, wanted_bias = 1,
               "Alkalinity", date_list2[i], 
               "Alkalinity (meq/L")
}
dev.off()