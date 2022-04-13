# read in pigment data
wk_pig <- read.csv('./WK_pig.csv')
colnames(wk_pig)
for_merge <- cbind(wk_data$Sample_ID, wk_data$Date_Extracted, wk_data$Latitude, wk_data$Longitude)
for_merge <- data.frame(for_merge)
colnames(for_merge) <- c("Sample_ID", "Date_Extracted", "Latitude", "Longitude")
r1 <- for_merge[201:220,]
r2 <- for_merge[281:300,]
r3 <- for_merge[401:420,]
for_merge_real <- rbind(r1, r2, r3)
wk_pig_real <- merge(wk_pig, for_merge_real, by = c("Sample_ID", "Date_Extracted"))
wk_pig_real$Longitude <- as.numeric(wk_pig_real$Longitude)
wk_pig_real$Latitude <- as.numeric(wk_pig_real$Latitude)

# zeaxanthin amount maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
zea_range1 <- c(32.26, 735.2)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Zea_Amount', zea_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_zea_amount_cwk'), 
               "Zeaxanthin Concentration", date1[i], 
               "Zeaxanthin Concentration (ng/L)")
}

# zeaxanthin total chla maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
zea_range2 <- c(0.01, 0.1614)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Zea_Tchla', zea_range2, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_zea_tchla_cwk'), 
               "Zeaxanthin Total Chlorophyll a Ratio", date1[i], 
               "Zeaxanthin Total Chlorophyll a Ratio")
}

# chlorphyll b amount maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
chlb_range1 <- c(200.317, 848.0794)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Chl_b_Amount', chlb_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_chlb_amount_cwk'), 
               "Chlorophyll b Concentration", date1[i], 
               "Chlorophyll b Concentration (ng/L)")
}

# chlb total chla maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
chlb_range2 <- c(0.0196358, .2187802)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Chlb_Tchla', chlb_range2, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_chlb_tchla_cwk'), 
               "Chlorophyll b Total Chlorophyll a Ratio", date1[i], 
               "Chlorophyll b Total Chlorophyll a Ratio") 
}  

# fucoxanthin amount maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
fuco_range1 <- c(142.6646, 755.0515)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Fuco_Amount', fuco_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_fuco_amount_cwk'), 
               "Fucoxanthin Concentration", date1[i], 
               "Fucoxanthin Concentration (ng/L)")
}

# fuco total chla maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
fuco_range2 <- c(0.01007078, 0.1070599)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Fuco_TChla', fuco_range2, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_fuco_tchla_cwk'), 
               "Fucoxanthin Total Chlorophyll a Ratio", date1[i], 
               "Fucoxanthin Total Chlorophyll a Ratio") 
}

#Tchla amount maps
date1 <- c("7/14/2021", "8/11/2021", "9/22/2021")
date1hyp <- c("7-14-2021", "8-11-2021", "9-22-2021")
tchla_range1 <- c(1.529920, 19.61389)

for (i in seq_along(date1)) {
  make_cwk_map(wk_pig_real, date1[i], 'Tchla_.ug.L.', tchla_range1, is.reversed = F, wanted_bias = 1,
               paste0(date1hyp[i], '_tchl_amount_cwk'), 
               "Total Chlorophyll a Concentration", date1[i], 
               "Total Chlorophyll a Concentration (μg/L)")
}