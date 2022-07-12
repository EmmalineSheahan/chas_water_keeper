library(dplyr)

# creating a master dataframe with all variables

# reading in all csv files
wk_dat <- read.csv('./data/WK_Data.csv')
wk_pig <- read.csv('./data/WK_pig.csv')
wk_cdom <- read.csv('./data/WK_CDOM_Abs_Data_master.csv')
wk_tides_rain <- read.csv('./data/tides_and_rain.csv')
wk_chemtax <- read.csv('./data/WK_Chemtax_Final.csv')

# changing col_names and making numeric
colnames(wk_dat)
colnames(wk_dat) <- c("Sample_ID", "Date_Extracted", "Phycoerytherin_conc", 
                       "Phycocyanin_conc", "Chlorophyll_conc", 
                       "PE_PC_ratio", "PE_Chla_ratio", "PC_Chla_ratio", "Temp", 
                       "Salinity", "Nitrate", "Phosphate",
                       "Tidal_height", "Latitude", "Longitude")
wk_dat$Phycoerytherin_conc <- as.numeric(wk_dat$Phycoerytherin_conc)
wk_dat$PE_PC_ratio <- as.numeric(wk_dat$PE_PC_ratio)
wk_dat$Temp <- as.numeric(wk_dat$Temp)
wk_dat$Phosphate <- as.numeric(wk_dat$Phosphate)
wk_dat$Salinity <- as.numeric(wk_dat$Salinity)
wk_dat$Nitrate <- as.numeric(wk_dat$Nitrate)
wk_dat$Longitude <- as.numeric(wk_dat$Longitude)
wk_dat$Latitude <- as.numeric(wk_dat$Latitude)

colnames(wk_pig)

colnames(wk_cdom)
colnam_new <- c("Sample_ID", "Date_Extracted", "cdom_slope_a", "cdom_slope_b", 
                "cdom_slope_c",
                "cdom_abs_a", "cdom_abs_b", "cdom_abs_c", 
                "cdom_abs_d", "cdom_abs_e", "delete")
colnames(wk_cdom) <- colnam_new
wk_cdom <- wk_cdom[,1:10]

colnames(wk_tides_rain)
new_col <- c("Sample_ID", "Date_Extracted", "Salinity", "Adjusted_Tidal_height", "in_or_out", "Time_collected", "Tide_time", 
             "delete", "Day_rain", "Week_rain", "days_five_rain", "days_three_rain")
colnames(wk_tides_rain) <- new_col
wk_tides_rain1 <- wk_tides_rain[,1:7]
wk_tides_rain2 <- wk_tides_rain[,9:12]
wk_tides_rain <- cbind(wk_tides_rain1, wk_tides_rain2)
wk_tides_rain$Salinity <- as.numeric(wk_tides_rain$Salinity)
wk_tides_rain$Adjusted_Tidal_height <- as.numeric(wk_tides_rain$Adjusted_Tidal_height)
wk_tides_rain$Day_rain <- as.numeric(wk_tides_rain$Day_rain)
wk_tides_rain$Week_rain <- as.numeric(wk_tides_rain$Week_rain)
wk_tides_rain$days_three_rain <- as.numeric(wk_tides_rain$days_three_rain)
wk_tides_rain$days_five_rain <- as.numeric(wk_tides_rain$days_five_rain)

# merge data frames
nrow(wk_dat)
nrow(wk_pig)

# going to wait to merge wk_pig for the other weeks
nrow(wk_cdom)
nrow(wk_tides_rain)

wk_master1 <- merge(wk_dat, wk_tides_rain, by = c("Sample_ID", "Date_Extracted", "Salinity"))
head(wk_master1)

wk_dat$Date_Extracted
dates_add <- c(rep("4/28/2021", 20), rep("5/5/2021", 20), rep("5/19/2021", 20), rep("5/26/2021", 20), rep("6/2/2021", 20),
               rep("6/9/2021", 20), rep("6/16/2021", 20), rep("6/23/2021", 20), rep("6/30/2021", 20))
length(dates_add)
colnames(wk_cdom)
samples_add <- rep(c("AR1", "AR2", "AR3", "CC1", "CH1", "CH2", "CH3", "CS1", "FB1", "FC1", "HC1", "HC2", "JIC1",
                     "JIC2", "SC1", "SC2", "SC3", "SR1", "WC1", "WR1"), times = 9)
length(samples_add)
to_add <- matrix(nrow = 180, ncol = 8)
to_add2 <- cbind(samples_add, dates_add, to_add)
to_add2 <- data.frame(to_add2)
colnames(to_add2) <- colnames(wk_cdom)
str(to_add2)
to_add2$cdom_slope_a <- as.numeric(to_add2$cdom_slope_a)
to_add2$cdom_slope_b <- as.numeric(to_add2$cdom_slope_b)
to_add2$cdom_slope_c <- as.numeric(to_add2$cdom_slope_c)
to_add2$cdom_abs_a <- as.numeric(to_add2$cdom_abs_a)
to_add2$cdom_abs_b <- as.numeric(to_add2$cdom_abs_b)
to_add2$cdom_abs_c <- as.numeric(to_add2$cdom_abs_c)
to_add2$cdom_abs_d <- as.numeric(to_add2$cdom_abs_d)
to_add2$cdom_abs_e <- as.numeric(to_add2$cdom_abs_e)
wk_cdom2 <- rbind(to_add2, wk_cdom)

dim(wk_master1)
dim(wk_cdom2)

wk_master <- merge(wk_master1, wk_cdom2, by = c("Date_Extracted", "Sample_ID"))

# removing negative values
wk_master$Phycocyanin_conc[wk_master$Phycocyanin_conc < 0] <- 0
wk_master$Phycoerytherin_conc[wk_master$Phycoerytherin_conc < 0] <- 0

# removing weeks 8/18 and 8/25 for phycocyanin
wk_master$Phycocyanin_conc[361:400] <- NA

# merging in chemtax data
dim(wk_chemtax)
dim(wk_master)
wk_master <- merge(wk_master, wk_chemtax, by = c("Date_Extracted", "Sample_ID"), all.x = T)

wk_master <- wk_master[order(as.Date(wk_master$Date_Extracted, format="%m/%d/%Y")),]

# adding a column for N:P ratio
nit <- wk_master$Nitrate
pho <- wk_master$Phosphate
length(nit)
length(pho)

nit_pho <- c(nit/pho)
length(nit_pho)
N_to_P_ratio <- nit_pho
wk_master <- cbind(wk_master, N_to_P_ratio)
wk_master$N_to_P_ratio[wk_master$N_to_P_ratio == Inf] <- 0

save(wk_master, file = './data/wk_master.Rdata')

# adding new columns and tide classes
wk_new <- read.csv('./data/WK_final_data.csv')
colnames(wk_new)
dim(wk_new)
dim(wk_master)
colnames(wk_master)

for_bind <- cbind(wk_new$Tide.Class, wk_new$pH, wk_new$Alkalinity, wk_new$TCO2)
colnames(for_bind) <- c("Tide_Class", "pH", "Alkalinity", "TCO2")
wk_master <- cbind(wk_master, for_bind)
dim(wk_master)
head(wk_master)
wk_master$pH <- as.numeric(wk_master$pH)
wk_master$Alkalinity <- as.numeric(wk_master$Alkalinity)
wk_master$TCO2 <- as.numeric(wk_master$TCO2)

save(wk_master, file = './data/wk_master.Rdata')
