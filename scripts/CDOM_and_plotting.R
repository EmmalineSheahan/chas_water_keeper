library(ggplot2)

source('./scripts/run_first_functions.R')

# read in data
load('./data/wk_master.Rdata')

# CDOM maps
# 300-600 slope

# find max and min for scale
which(wk_master$cdom_slope_a == 
        max(wk_master$cdom_slope_a, na.rm = T))
wk_master[355,]
# max week is 9/1/2021

which(wk_master$cdom_slope_a == 
        min(wk_master$cdom_slope_a, na.rm = T))
wk_master[338,]
# min week is 8/25/2021

# finding max and min raster values for scale
# building grid for interpolation
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid

coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE

temp_data_test <- wk_master %>% filter(Date_Extracted == "9/1/2021") %>% 
  filter(!is.na(cdom_slope_a))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_slope_a ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for 300-600 slope is 0.01736901

x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid

coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE

temp_data_test <- wk_master %>% filter(Date_Extracted == "8/25/2021") %>% 
  filter(!is.na(cdom_slope_a))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_slope_a ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

min(wanted_ras@data@values)
# min for 300-600 slope is 0.004725421

# maps for 300-600 slope
date_list <- unique(wk_master$Date_Extracted)
date_list2 <- date_list[10:26]

cdom_range_slope_a <- c(0.004725421, 0.01736901)

pdf('./figures/cdom_300-600-slope_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_slope_a', cdom_range_slope_a, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Slope of Absorbance from 300-600 nm)")
}
dev.off()

# 350-500 slope

# find max and min for scale
which(wk_master$cdom_slope_b == 
        max(wk_master$cdom_slope_b, na.rm = T))
wk_master[469,]
# max week is 10/13/2021

which(wk_master$cdom_slope_b == 
        min(wk_master$cdom_slope_b, na.rm = T))
wk_master[338,]
# min week is 8/25/2021

# finding max and min raster values for scale
# building grid for interpolation
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
  filter(!is.na(cdom_slope_b))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_slope_b ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for 350-500 slope is 0.02278825

x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid

coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE

temp_data_test <- wk_master %>% filter(Date_Extracted == "8/25/2021") %>% 
  filter(!is.na(cdom_slope_b))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_slope_b ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

min(wanted_ras@data@values)
# min for 350-500 slope is 0.005474688

# maps for 350-500 slope
cdom_range_slope_b <- c(0.005474688, 0.02278825)

pdf('./figures/cdom_350-500-slope_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_slope_b', cdom_range_slope_b, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Slope of Absorbance from 350-500 nm)")
}
dev.off()
# 300-450 slope

# find max and min for scale
which(wk_master$cdom_slope_c == 
        max(wk_master$cdom_slope_c, na.rm = T))
wk_master[469,]
# max week is 10/13/2021

which(wk_master$cdom_slope_c == 
        min(wk_master$cdom_slope_c, na.rm = T))
wk_master[228,]
# min week is 7/21/2021

# finding max and min raster values for scale
# building grid for interpolation
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
  filter(!is.na(cdom_slope_c))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_slope_c ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for 300-450 slope is 0.0200302

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
  filter(!is.na(cdom_slope_c))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_slope_c ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

min(wanted_ras@data@values)
# min for 300-450 slope is 0.007096165

# maps for 350-500 slope
cdom_range_slope_c <- c(0.007096165, 0.0200302)

pdf('./figures/cdom_300-450-slope_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_slope_c', cdom_range_slope_c, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Slope of Absorbance from 300-450 nm)")
}
dev.off()

# absorbance at 350

# find max and min for scale
which(wk_master$cdom_abs_a == 
        max(wk_master$cdom_abs_a, na.rm = T))
wk_master[250,]
# max week is 7/28/2021

which(wk_master$cdom_abs_a == 
        min(wk_master$cdom_abs_a, na.rm = T))
wk_master[219,]
# min week is 7/14/2021

# finding max and min raster values for scale
# building grid for interpolation
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
  filter(!is.na(cdom_abs_a))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow2 <- gstat::idw(formula = cdom_abs_a ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1, na.action = na.exclude)
wanted_ras <- raster(idw_pow2)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for absorbance at 300 nm is 27.48953

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
  filter(!is.na(cdom_abs_a))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_abs_a ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

min(wanted_ras@data@values)
# min for absorbance at 350 nm is 3.553744

# maps for absorbance at 350 nm
cdom_range_abs_a <- c(3.553744, 27.48953)

pdf('./figures/cdom_abs-350_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_abs_a', cdom_range_abs_a, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Absorbance at 350 nm)")
}
dev.off()

# Absorbance at 355 nm

# find max and min for scale
which(wk_master$cdom_abs_b == 
        max(wk_master$cdom_abs_b, na.rm = T))
wk_master[250,]
# max week is 7/28/2021

which(wk_master$cdom_abs_b == 
        min(wk_master$cdom_abs_b, na.rm = T))
wk_master[219,]
# min week is 7/14/2021

# finding max and min raster values for scale
# building grid for interpolation
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
  filter(!is.na(cdom_abs_b))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow2 <- gstat::idw(formula = cdom_abs_b ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1, na.action = na.exclude)
wanted_ras <- raster(idw_pow2)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for absorbance at 355 nm is 25.5089

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
  filter(!is.na(cdom_abs_b))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_abs_b ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

min(wanted_ras@data@values)
# min for absorbance at 355 nm is  3.309506

# maps for absorbance at 355 nm
cdom_range_abs_b <- c(3.309506, 25.5089)

pdf('./figures/cdom_abs-355_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_abs_b', cdom_range_abs_b, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Absorbance at 355 nm)")
}
dev.off()

# absorbance at 375
# find max and min for scale
which(wk_master$cdom_abs_c == 
        max(wk_master$cdom_abs_c, na.rm = T))
wk_master[243,]
# max week is 7/28/2021

which(wk_master$cdom_abs_c == 
        min(wk_master$cdom_abs_c, na.rm = T))
wk_master[461,]
# min week is 10/13/2021

# finding max and min raster values for scale
# building grid for interpolation
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
  filter(!is.na(cdom_abs_c))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow2 <- gstat::idw(formula = cdom_abs_c ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1, na.action = na.exclude)
wanted_ras <- raster(idw_pow2)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for absorbance at 375 nm is 21.46379

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
  filter(!is.na(cdom_abs_c))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow1 <- gstat::idw(formula = cdom_abs_c ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1)
wanted_ras <- raster(idw_pow1)
proj4string(wanted_ras) <- wanted_crs_2

min(wanted_ras@data@values)
# min for absorbance at 375 nm is 1.978168

# maps for absorbance at 375 nm
cdom_range_abs_c <- c(1.978168, 21.46379)

pdf('./figures/cdom_abs-375_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_abs_c', cdom_range_abs_c, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Absorbance at 375 nm)")
}
dev.off()

# absorbance at 412 nm
# find max and min for scale
which(wk_master$cdom_abs_d == 
        max(wk_master$cdom_abs_d, na.rm = T))
wk_master[338,]
# max week is 8/25/2021

which(wk_master$cdom_abs_d == 
        min(wk_master$cdom_abs_d, na.rm = T))
wk_master[329,]
# min week is 8/25/2021

# finding max and min raster values for scale
# building grid for interpolation
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid

coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE

temp_data_test <- wk_master %>% filter(Date_Extracted == "8/25/2021") %>%
  filter(!is.na(cdom_abs_d))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow2 <- gstat::idw(formula = cdom_abs_d ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1, na.action = na.exclude)
wanted_ras <- raster(idw_pow2)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for absorbance at 412 nm is 13.37321
min(wanted_ras@data@values)
# min for absorbance at 412 nm is 1.275421

# maps for absorbance at 412 nm
cdom_range_abs_d <- c(1.275421, 13.37321)

pdf('./figures/cdom_abs-412_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_abs_d', cdom_range_abs_d, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Absorbance at 412 nm)")
}
dev.off()

# Absorbance at 440 nm
# find max and min for scale
which(wk_master$cdom_abs_e == 
        max(wk_master$cdom_abs_e, na.rm = T))
wk_master[338,]
# max week is 8/25/2021

which(wk_master$cdom_abs_e == 
        min(wk_master$cdom_abs_e, na.rm = T))
wk_master[469,]
# min week is 10/13/2021

# finding max and min raster values for scale
# building grid for interpolation
x_range <- as.numeric(c(-80.0075, -79.84617))
y_range <- as.numeric(c(32.65034, 32.90124))
grd_wk <- expand.grid(x = seq(from = x_range[1],
                              to = x_range[2], 
                              by = 0.005),
                      y = seq(from = y_range[1], to = y_range[2], 
                              by = 0.005))  # expand points to grid

coordinates(grd_wk) <- ~x + y
gridded(grd_wk) <- TRUE

temp_data_test <- wk_master %>% filter(Date_Extracted == "8/25/2021") %>%
  filter(!is.na(cdom_abs_e))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow2 <- gstat::idw(formula = cdom_abs_e ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1, na.action = na.exclude)
wanted_ras <- raster(idw_pow2)
proj4string(wanted_ras) <- wanted_crs_2

max(wanted_ras@data@values)
# max for absorbance at 440 nm is 12.16305

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
  filter(!is.na(cdom_abs_e))
coordinates(temp_data_test) <- ~Longitude+Latitude

idw_pow2 <- gstat::idw(formula = cdom_abs_e ~ 1,
                       locations = temp_data_test,
                       newdata = grd_wk,
                       idp = 1, na.action = na.exclude)
wanted_ras <- raster(idw_pow2)
proj4string(wanted_ras) <- wanted_crs_2
min(wanted_ras@data@values)
# min for absorbance at 440 nm is 0.5403681

# maps for absorbance at 440 nm
cdom_range_abs_e <- c(0.5403681, 12.16305)

pdf('./figures/cdom_abs-440_All_Weeks.pdf')
for (i in seq_along(date_list2)) {
  make_cwk_map(wk_master, date_list2[i], 'cdom_abs_e', cdom_range_abs_e, is.reversed = F, wanted_bias = 1,
               "CDOM", date_list2[i], 
               "CDOM (Absorbance at 440 nm)")
}
dev.off()