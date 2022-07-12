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

# creating needed crs
epsg <- make_EPSG()
wanted_crs <- epsg %>% filter(code == 4326)
wanted_crs <- wanted_crs$prj4
wanted_crs_2 <- "+proj=longlat +datum=WGS84 +units=m"

# read in water keeper data to create spatial points for plotting
wk_data <- read.csv('./data/WK_Data.csv')
colnames(wk_data) <- c("Sample_ID", "Date_Extracted", "Phycoerytherin_conc", 
                       "Phycocyanin_conc", "Chlorophyll_conc", 
                       "PE_PC_ratio", "PE_Chla_ratio", "PC_Chla_ratio", "Temp", "Salinity", "Nitrate", "Phosphate",
                       "Tidal_height", "Latitude", "Longitude")
wk_data$Phycoerytherin_conc <- as.numeric(wk_data$Phycoerytherin_conc)
wk_data$PE_PC_ratio <- as.numeric(wk_data$PE_PC_ratio)
wk_data$Temp <- as.numeric(wk_data$Temp)
wk_data$Phosphate <- as.numeric(wk_data$Phosphate)
wk_data$Salinity <- as.numeric(wk_data$Salinity)

# transform into spatial points
wk_sp <- wk_data
coordinates(wk_sp) <- ~Longitude+Latitude
proj4string(wk_sp) <- CRS(wanted_crs)
wk_sp <- spTransform(wk_sp, CRSobj = CRS(wanted_crs_2))
extent(wk_sp)
ne_ext <- extent(-80.0075, -79.84617, 32.65034, 32.90124)

# creating new_crop map
test_sf <- shapefile('./Water_Around_Charleston.shp')
plot(test_sf)
test_sf <- spTransform(test_sf, CRSobj = CRS(wanted_crs_2))
test_sf_crop <- crop(test_sf, ne_ext)
plot(test_sf_crop)

to_clip <- shapefile('./use_in_r_to_clip.shp')
to_clip <- spTransform(to_clip, CRSobj = CRS(wanted_crs_2))
to_clip <- crop(to_clip, ne_ext)

new_crop <- gDifference(to_clip, test_sf_crop)
plot(new_crop, col = "black")
plot(wk_sp, col = "red", add = T)

# new_crop will be used for map making

# Building make_cwk_map function to generate maps by date and variable
# wanted_dataset = dataframe from which information is pulled
# wanted_date = date for map in m/dd/yyyy format, as a character string
# wanted_var = variable name to be displayed in the map as written in dataframe as character
# wanted_range = set the range for the legend
# is.reversed = true or false if the color ramp is reversed  
# wanted_bias = number indicating color scale bias
# plot_title = title for plot as character
# plot_sub = subtitle for date placement on the map as character
# leg_title = title for the legend as character
make_cwk_map <- function(wanted_dataset, wanted_date, wanted_var, wanted_range, is.reversed, wanted_bias,
                         plot_title, plot_sub, leg_title) {
  x_range <- as.numeric(c(-80.0075, -79.84617))
  y_range <- as.numeric(c(32.65034, 32.90124))
  grd_wk <- expand.grid(x = seq(from = x_range[1],
                                to = x_range[2], 
                                by = 0.005),
                        y = seq(from = y_range[1], to = y_range[2], 
                                by = 0.005))  # expand points to grid
  
  coordinates(grd_wk) <- ~x + y
  gridded(grd_wk) <- TRUE
  temp_data_test <- wanted_dataset %>% filter(Date_Extracted == wanted_date) %>% 
    filter(!is.na(eval(parse(text = paste0(wanted_var)))))
  coordinates(temp_data_test) <- ~Longitude+Latitude
  idw_pow1 <- gstat::idw(formula = eval(parse(text = paste0(wanted_var, ' ~ 1'))),
                         locations = temp_data_test,
                         newdata = grd_wk,
                         idp = 1)
  wanted_ras <- raster(idw_pow1)
  proj4string(wanted_ras) <- wanted_crs_2
  
  cols <- c("#CC00FF", "#3333FF", "#33FFFF", "#33CC33", "#99FF33", "#FFFF00", "#FF9900", "#990000")
  pal <- colorRampPalette(cols, bias = wanted_bias)
  
  plot(wanted_ras, zlim = wanted_range,
       col = pal(55), main = plot_title,
       legend.args = list(text = leg_title, side = 3, line = 1), horizontal = T, axes = F, box = F)
  title(sub = plot_sub, adj = 0.5, line = -23.5)
  plot(new_crop, add = T, col = "white", border = "black")
  plot(wk_sp, col = "red", pch = 10, cex = 1.3, add = T)
}

# cor_ggplot function
# wanted_data = the dataset from which variables are plotted
# wanted_date = specific date to be examined in character
# x_val = x axis variable in character
# y_val = y axis variable in character
# x_lab = x axis label in character
# y_lab = y axis label in character
# plot_title = title for plot, date
cor_ggplot <- function(wanted_data, wanted_date, x_val, y_val, x_lab, y_lab, plot_title) {
  tmp_dat <- wanted_data %>% filter(Date_Extracted == wanted_date)
  x = eval(parse(text = paste0('tmp_dat$', x_val)))
  y = eval(parse(text = paste0('tmp_dat$', y_val)))
  lin <- lm(data = tmp_dat, y ~ x)
  p <- ggplot(data = tmp_dat, mapping = aes(x, y)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(plot_title) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 17), axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16))
  print(p)
  print(lin)
  return(lin)
}

# site_ggplot function
# wanted_data = the dataset from which variables are plotted
# wanted_site = the site to be examined in character
# x_val = x axis variable in character
# y_val = y axis variable in character
# x_lab = x axis label in character
# y_lab = y axis label in character
# plot_title = title for plot, date
site_ggplot <- function(wanted_data, wanted_site, x_val, y_val, x_lab, y_lab, plot_title) {
  tmp_dat <- wanted_data %>% filter(Sample_ID == wanted_site) %>%
  filter(!is.na(eval(parse(text = paste0(x_val))))) %>%
    filter(!is.na(eval(parse(text = paste0(y_val)))))
  x = as.numeric(eval(parse(text = paste0('tmp_dat$', x_val))))
  y = as.numeric(eval(parse(text = paste0('tmp_dat$', y_val))))
  lin <- lm(data = tmp_dat, y ~ x)
  p <- ggplot(data = tmp_dat, mapping = aes(x, y)) +
    geom_point() +
    geom_smooth(method = 'lm') +
    xlab(x_lab) +
    ylab(y_lab) +
    ggtitle(plot_title) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5, size = 17), axis.title.x = element_text(size = 16), 
          axis.title.y = element_text(size = 16))
  print(p)
   print(lin)
  return(lin)
}

# line_ggplot function
# wanted_dat = data set from which to pull
# wanted_site = site to look at
# wanted_var = variable in character form
# y_lab = y axis label
# plot_title = plot title
line_ggplot <- function(wanted_data, wanted_site, wanted_var, y_lab, plot_title) {
temp_dat <- wanted_data %>% filter(Sample_ID == wanted_site)
temp_dat <- temp_dat[order(as.Date(temp_dat$Date_Extracted, format="%m/%d/%Y")),]
temp_dat$Date_Extracted <- factor(temp_dat$Date_Extracted, levels = unique(temp_dat$Date_Extracted))
y = eval(parse(text = paste0('temp_dat$', wanted_var)))
p <- ggplot(data = temp_dat, aes(x = Date_Extracted, y = y, group = 1)) +
  geom_line(color="red")+
  geom_point() +
  xlab("Date") +
  ylab(y_lab) +
  ggtitle(plot_title) +
  theme(axis.text.x = element_text(angle = 90))
print(p)
}