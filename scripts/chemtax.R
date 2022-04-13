library(ggplot2)
library(dplyr)

source('./scripts/run_first_functions.R')
load('./data/wk_master.Rdata')

# creating maps for phytoplankton community composition
date_list <- unique(wk_master$Date_Extracted)

# maps for diatoms
# first have to find min and max raster cell values
which(wk_master$Diatoms.DinoA == min(wk_master$Diatoms.DinoA, na.rm = T))
wk_master[250,]

# min week is 7/28/2021
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
  filter(!is.na(Diatoms.DinoA))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Diatoms.DinoA ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# the min of the range is 13.29838

which(wk_master$Diatoms.DinoA == max(wk_master$Diatoms.DinoA, na.rm = T))
wk_master[59,]

# max week is 5/19/2021
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid
coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE
temp_data_test <- wk_master %>% filter(Date_Extracted == "5/19/2021") %>% 
  filter(!is.na(Diatoms.DinoA))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Diatoms.DinoA ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max of the range is 85.26433

diatom_range <- c(13.29838, 85.26433)

pdf('./figures/Diatoms_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'Diatoms.DinoA', diatom_range, is.reversed = F, wanted_bias = 1,
               "Diatom Proportion of Community Composition", date_list[i], 
               "Percent of total Chlorophyll a (μg/L)")
}
dev.off()

# maps for cyanos
# first have to find min and max raster cell values
which(wk_master$Cyanos == min(wk_master$Cyanos, na.rm = T))
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
  filter(!is.na(Cyanos))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Cyanos ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# the min of the range is 0.3803486

which(wk_master$Cyanos == max(wk_master$Cyanos, na.rm = T))
wk_master[250,]

# max week is 7/28/2021
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
  filter(!is.na(Cyanos))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Cyanos ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max of the range is 42.04661

cyano_range <- c(0.3803486, 42.04661)

pdf('./figures/Cyanos_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'Cyanos', cyano_range, is.reversed = F, wanted_bias = 1,
               "Cyanobacteria Proportion of Community Composition", date_list[i], 
               "Percent of total Chlorophyll a (μg/L)")
}
dev.off()

# maps for prasinosA
# first have to find min and max raster cell values
which(wk_master$PrasinoA == min(wk_master$PrasinoA, na.rm = T))
wk_master[10,]

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
  filter(!is.na(PrasinoA))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PrasinoA ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# the min of the range is 0.01359382

which(wk_master$PrasinoA == max(wk_master$PrasinoA, na.rm = T))
wk_master[425,]

# max week is 9/29/2021
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
  filter(!is.na(PrasinoA))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PrasinoA ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max of the range is 28.94053

prasinoa_range <- c(0.01359382, 28.94053)

pdf('./figures/PrasinoA_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'PrasinoA', prasinoa_range, is.reversed = F, wanted_bias = 1,
               "PrasinoA Proportion of Community Composition", date_list[i], 
               "Percent of total Chlorophyll a (μg/L)")
}
dev.off()

# maps for PrasinoB
# first have to find min and max raster cell values
which(wk_master$PrasinoB == min(wk_master$PrasinoB, na.rm = T))
wk_master[1,]

# min week is 7/14/2021
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
  filter(!is.na(PrasinoB))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PrasinoB ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# the min of the range is 0

which(wk_master$PrasinoB == max(wk_master$PrasinoB, na.rm = T))
wk_master[290,]

# max week is 8/11/2021
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
  filter(!is.na(PrasinoB))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = PrasinoB ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max of the range is 50.54679

prasinob_range <- c(0, 50.54679)

pdf('./figures/PrasinoB_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'PrasinoB', prasinob_range, is.reversed = F, wanted_bias = 1,
               "PrasinoB Proportion of Community Composition", date_list[i], 
               "Percent of total Chlorophyll a (μg/L)")
}
dev.off()

# Maps for Euglenos
# first have to find min and max raster cell values
which(wk_master$Eugleno == min(wk_master$Eugleno, na.rm = T))
wk_master[152,]

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
  filter(!is.na(Eugleno))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Eugleno ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)

# the min of the range is 0.007364535

which(wk_master$Eugleno == max(wk_master$Eugleno, na.rm = T))
wk_master[420,]

# max week is 9/29/2021
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
  filter(!is.na(Eugleno))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = Eugleno ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2
max(wanted_ras@data@values)

# max of the range is 27.75855

eugleno_range <- c(0.007364535, 27.75855)

pdf('./figures/Eugleno_All_Weeks.pdf')
for (i in seq_along(date_list)) {
  make_cwk_map(wk_master, date_list[i], 'Eugleno', eugleno_range, is.reversed = F, wanted_bias = 1,
               "Eugleno Proportion of Community Composition", date_list[i], 
               "Percent of total Chlorophyll a (μg/L)")
}
dev.off()