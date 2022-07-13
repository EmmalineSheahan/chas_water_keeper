library(dplyr)
load('./data/wk_master.Rdata')

# reading in stats files

filelist <- list.files('./stats/')
for (i in seq_along(filelist)) {
  load(paste0('./stats/', filelist[i]))
}

# creating data table for stats by site
# investigating stats objects
length(site_nitrate_v_chlorophyll_stats)
t <- summary(site_nitrate_v_chlorophyll_stats[[20]])
coef(t)[2]
t$r.squared
t$coefficients[2,4]

# make_site_df function puts wanted stats into a df
# wanted_stats = stat list
# x_name = x variable as character
# y_name = y variable as character
make_site_df <- function(wanted_stats, x_name, y_name) {
new_df <- matrix(ncol = 4, nrow = 20)
new_df <- data.frame(new_df)
colnames(new_df) <- c("x_var", "y_var", "R_Squared", "P_value")
site <- unique(wk_master$Sample_ID)
for (i in seq_along(site)) {
  t <- summary(wanted_stats[[i]])
  new_df$x_var[i] <- x_name
  new_df$y_var[i] <- y_name
  new_df$R_Squared[i] <- t$r.squared
  new_df$P_value[i] <- t$coefficients[2,4]
}
new_df <- cbind(site, new_df)
return(new_df)
}

# Nitrate v Chlorophyll
nitrate_v_chloro <- make_site_df(site_nitrate_v_chlorophyll_stats, "Nitrate", "Chlorophyll Concentration")

# Nitrate v Phycocyanin
nitrate_v_pc <- make_site_df(site_nitrate_v_phycocyanin_stats, "Nitrate", "Phycocyanin Concentration")

# Nitrate v Phycoerytherin
nitrate_v_pe <- make_site_df(site_nitrate_v_phycoerytherin_stats, "Nitrate", "Phycoerytherin Concentration")

# Phosphate v Chlorophyll
phosphate_v_chloro <- make_site_df(site_phosphate_v_chlorophyll_stats, "Phosphate", "Chlorophyll Concentration")

# Phosphate v Phycocyanin
phosphate_v_pc <- make_site_df(site_phosphate_v_phycocyanin_stats, "Phosphate", "Phycocyanin Concentration")

# Phosphate v Phycoerytherin
phosphate_v_pe <- make_site_df(site_phosphate_v_phycoerytherin_stats, "Phosphate", "Phycoerytherin Concentration")

# Phycocyanin v cyanos
pc_v_cyanos <- make_site_df(site_phycocyanin_v_cyanos_stats, "Phycocyanin Concentration", 
                            "Proportion of community attributible to cyanos")

# Phycoerytherin v cyanos
pe_v_cyanos <- make_site_df(site_phycoerytherin_v_cyanos_stats, "Phycoerytherin Concentration", 
                            "Proportion of community attributible to cyanos")

# Salinity v Chlorophyll
salinity_v_chloro <- make_site_df(site_salinity_v_chlorophyll_stats, "Salinity", "Chlorophyll Concentration")

# Salinity v Phycocyanin
salinity_v_pc <- make_site_df(site_salinity_v_phycocyanin_stats, "Salinity", "Phycocyanin Concentration")

# Salinity v Phycoerytherin
salinity_v_pe <- make_site_df(site_salinity_v_phycoerytherin_stats, "Salinity", "Phycoerytherin Concentration")

# Temp v Chlorophyll
temp_v_chloro <- make_site_df(site_temp_v_chlorophyll_stats, "Temperature", "Chlorophyll Concentration")

# Temp v Phycocyanin
temp_v_pc <- make_site_df(site_temp_v_phycocyanin_stats, "Temperature", "Phycocyanin Concentration")

# Temp v Phycoerytherin
temp_v_pe <- make_site_df(site_temp_v_phycoerytherin_stats, "Temperature", "Phycoerytherin Concentration")

# Tide v Chlorophyll
tide_v_chloro <- make_site_df(site_tide_v_chlorophyll_stats, "Tidal Height", "Chlorophyll Concentration")

# Tide v Phycocyanin
tide_v_pc <- make_site_df(site_tide_v_phycocyanin_stats, "Tidal Height", "Phycocyanin Concentration")

# Tide v Phycoerytherin
tide_v_pe <- make_site_df(site_tide_v_phycoerytherin_stats, "Tidal Height", "Phycoerytherin Concentration")

# Tide v CDOM
tide_v_cdom <- make_site_df(site_tide_v_cdom_stats, "Tidal Height", "CDOM absorbance at 412 nm")

# Tide v Phosphate
tide_v_phosphate <- make_site_df(site_tide_v_phosphate_stats, "Tidal Height", "Phosphate")

# Tide v Nitrate
tide_v_nitrate <- make_site_df(site_tide_v_nitrate_stats, "Tidal Height", "Nitrate")

# Tide v Salinity
tide_v_salinity <- make_site_df(site_tide_v_salinity_stats, "Tidal Height", "Salinity")

# Alkalinity vs Chlorophyll
alk_v_chla <- make_site_df(site_alk_v_chlorophyll_stats, "Alkalinity", "Chlorophyll Concentration")

# Alkalinity vs Phycocyanin
alk_v_pc <- make_site_df(site_alk_v_pc_stats, "Alkalinity", "Phycocyanin Concentration")

# Alkalinity vs Phycoerytherin
alk_v_pe <- make_site_df(site_alk_v_pe_stats, "Alkalinity", "Phycoerytherin Concentration")

# Alkalinity vs Phosphate
alk_v_phos <- make_site_df(site_alk_v_phosphate_stats, "Phosphate", "Alkalinity")

# Alkalinity vs Nitrate
alk_v_nit <- make_site_df(site_alk_v_nitrate_stats, "Nitrate", "Alkalinity")

# Alkalinity vs Temperature
alk_v_temp <- make_site_df(site_alk_v_temp_stats, "Temperature", "Alkalinity")

# Alkalinity vs Tide
alk_v_tide <- make_site_df(site_alk_v_tide_stats, "Tidal Height", "Alkalinity")

# Alkalinity vs Salinity
alk_v_salinity <- make_site_df(site_alk_v_salinity_stats, "Salinity", "Alkalinity")

# Alkalinity vs CDOM
alk_v_cdom <- make_site_df(site_alk_v_cdom_stats, "CDOM Absorbance at 412 nm", "Alkalinity")

# pH vs Chlorophyll
ph_v_chla <- make_site_df(site_ph_v_chlorophyll_stats, "pH", "Chlorophyll Concentration")

# pH vs Phycocyanin
ph_v_pc <- make_site_df(site_ph_v_pc_stats, "pH", "Phycocyanin Concentration")

# pH vs Phycoerytherin
ph_v_pe <- make_site_df(site_ph_v_pe_stats, "pH", "Phycoerytherin Concentration")

# pH vs Phosphate
ph_v_phos <- make_site_df(site_ph_v_phosphate_stats, "Phosphate", "pH")

# pH vs Nitrate
ph_v_nit <- make_site_df(site_ph_v_nitrate_stats, "Nitrate", "pH")

# pH vs Temperature
ph_v_temp <- make_site_df(site_ph_v_temp_stats, "Temperature", "pH")

# pH vs Tide
ph_v_tide <- make_site_df(site_ph_v_tide_stats, "Tidal Height", "pH")

# pH vs Salinity
ph_v_salinity <- make_site_df(site_ph_v_salinity_stats, "Salinity", "pH")

# pH vs CDOM
ph_v_cdom <- make_site_df(site_ph_v_cdom_stats, "CDOM Absorbance at 412 nm", "pH")

# Alkalinity vs pH
alk_v_ph <- make_site_df(site_alk_v_ph_stats, "Alkalinity", "pH")

# chemtax information

# Temp V DinoA
temp_v_DinoA <- make_site_df(site_temp_v_dinoa_stats, "Temperature", "DinoA")

# CDOM V DinoA
cdom_v_DinoA <- make_site_df(site_cdom_v_dinoa_stats, "CDOM", "DinoA")

# Salinity V DinoA
salinity_v_DinoA <- make_site_df(site_salinity_v_dinoa_stats, "Salinity", "DinoA")

# Tide V DinoA
tide_v_DinoA <- make_site_df(site_tide_v_dinoa_stats, "Tidal Height", "DinoA")

# Phosphate V DinoA
phosphate_v_DinoA <- make_site_df(site_phosphate_v_dinoa_stats, "Phosphate", "DinoA")

# Nitrate V DinoA
nitrate_v_DinoA <- make_site_df(site_nitrate_v_dinoa_stats, "Nitrate", "DinoA")

# Temp V DinoB
temp_v_DinoB <- make_site_df(site_temp_v_dinob_stats, "Temperature", "DinoB")

# CDOM V DinoB
cdom_v_DinoB <- make_site_df(site_cdom_v_dinob_stats, "CDOM", "DinoB")

# Salinity V DinoB
salinity_v_DinoB <- make_site_df(site_salinity_v_dinob_stats, "Salinity", "DinoB")

# Tide V DinoB
tide_v_DinoB <- make_site_df(site_tide_v_dinob_stats, "Tidal Height", "DinoB")

# Phosphate V DinoB
phosphate_v_DinoB <- make_site_df(site_phosphate_v_dinob_stats, "Phosphate", "DinoB")

# Nitrate V DinoB
nitrate_v_DinoB <- make_site_df(site_nitrate_v_dinob_stats, "Nitrate", "DinoB")

# Temp V Cyanos
temp_v_Cyanos <- make_site_df(site_temp_v_cyanos_stats, "Temperature", "Cyanos")

# CDOM V Cyanos
cdom_v_Cyanos <- make_site_df(site_cdom_v_cyanos_stats, "CDOM", "Cyanos")

# Salinity V Cyanos
salinity_v_Cyanos <- make_site_df(site_salinity_v_cyanos_stats, "Salinity", "Cyanos")

# Tide V Cyanos
tide_v_Cyanos <- make_site_df(site_tide_v_cyanos_stats, "Tidal Height", "Cyanos")

# Phosphate V Cyanos
phosphate_v_Cyanos <- make_site_df(site_phosphate_v_cyanos_stats, "Phosphate", "Cyanos")

# Nitrate V Cyanos
nitrate_v_Cyanos <- make_site_df(site_nitrate_v_cyanos_stats, "Nitrate", "Cyanos")

# Temp V PrasinoA
temp_v_PrasinoA <- make_site_df(site_temp_v_prasinoa_stats, "Temperature", "PrasinoA")

# CDOM V PrasinoA
cdom_v_PrasinoA <- make_site_df(site_cdom_v_prasinoa_stats, "CDOM", "PrasinoA")

# Salinity V PrasinoA
salinity_v_PrasinoA <- make_site_df(site_salinity_v_prasinoa_stats, "Salinity", "PrasinoA")

# Tide V PrasinoA
tide_v_PrasinoA <- make_site_df(site_tide_v_prasinoa_stats, "Tidal Height", "PrasinoA")

# Phosphate V PrasinoA
phosphate_v_PrasinoA <- make_site_df(site_phosphate_v_prasinoa_stats, "Phosphate", "PrasinoA")

# Nitrate V PrasinoA
nitrate_v_PrasinoA <- make_site_df(site_nitrate_v_prasinoa_stats, "Nitrate", "PrasinoA")

# Temp V PrasinoB
temp_v_PrasinoB <- make_site_df(site_temp_v_prasinob_stats, "Temperature", "PrasinoB")

# CDOM V PrasinoB
cdom_v_PrasinoB <- make_site_df(site_cdom_v_prasinob_stats, "CDOM", "PrasinoB")

# Salinity V PrasinoB
salinity_v_PrasinoB <- make_site_df(site_salinity_v_prasinob_stats, "Salinity", "PrasinoB")

# Tide V PrasinoB
tide_v_PrasinoB <- make_site_df(site_tide_v_prasinob_stats, "Tidal Height", "PrasinoB")

# Phosphate V PrasinoB
phosphate_v_PrasinoB <- make_site_df(site_phosphate_v_prasinob_stats, "Phosphate", "PrasinoB")

# Nitrate V PrasinoB
nitrate_v_PrasinoB <- make_site_df(site_nitrate_v_prasinob_stats, "Nitrate", "PrasinoB")

# Temp V Chloro
temp_v_Chlor <- make_site_df(site_temp_v_chloro_stats, "Temperature", "Chloro")

# CDOM V Chloro
cdom_v_Chlor <- make_site_df(site_cdom_v_chloro_stats, "CDOM", "Chloro")

# Salinity V Chloro
salinity_v_Chlor <- make_site_df(site_salinity_v_chloro_stats, "Salinity", "Chloro")

# Tide V Chloro
tide_v_Chlor <- make_site_df(site_tide_v_chloro_stats, "Tidal Height", "Chloro")

# Phosphate V Chloro
phosphate_v_Chlor <- make_site_df(site_phosphate_v_chloro_stats, "Phosphate", "Chloro")

# Nitrate V Chloro
nitrate_v_Chlor <- make_site_df(site_nitrate_v_chloro_stats, "Nitrate", "Chloro")

# Temp V RaphidoA
temp_v_RaphidoA <- make_site_df(site_temp_v_raphidoa_stats, "Temperature", "RaphidoA")

# CDOM V RaphidoA
cdom_v_RaphidoA <- make_site_df(site_cdom_v_raphidoa_stats, "CDOM", "RaphidoA")

# Salinity V RaphidoA
salinity_v_RaphidoA <- make_site_df(site_salinity_v_raphidoa_stats, "Salinity", "RaphidoA")

# Tide V RaphidoA
tide_v_RaphidoA <- make_site_df(site_tide_v_raphidoa_stats, "Tidal Height", "RaphidoA")

# Phosphate V RaphidoA
phosphate_v_RaphidoA <- make_site_df(site_phosphate_v_raphidoa_stats, "Phosphate", "RaphidoA")

# Nitrate V RaphidoA
nitrate_v_RaphidoA <- make_site_df(site_nitrate_v_raphidoa_stats, "Nitrate", "RaphidoA")

# Temp V HaptoA
temp_v_HaptoA <- make_site_df(site_temp_v_haptoa_stats, "Temperature", "HaptoA.ChrysoA.DinoC")

# CDOM V HaptoA
cdom_v_HaptoA <- make_site_df(site_cdom_v_haptoa_stats, "CDOM", "HaptoA.ChrysoA.DinoC")

# Salinity V HaptoA
salinity_v_HaptoA <- make_site_df(site_salinity_v_haptoa_stats, "Salinity", "HaptoA.ChrysoA.DinoC")

# Tide V HaptoA
tide_v_HaptoA <- make_site_df(site_tide_v_haptoa_stats, "Tidal Height", "HaptoA.ChrysoA.DinoC")

# Phosphate V HaptoA
phosphate_v_HaptoA <- make_site_df(site_phosphate_v_haptoa_stats, "Phosphate", "HaptoA.ChrysoA.DinoC")

# Nitrate V HaptoA
nitrate_v_HaptoA <- make_site_df(site_nitrate_v_haptoa_stats, "Nitrate", "HaptoA.ChrysoA.DinoC")

# Temp V Eugleno
temp_v_Eugleno <- make_site_df(site_temp_v_eugleno_stats, "Temperature", "Eugleno")

# CDOM V Eugleno
cdom_v_Eugleno <- make_site_df(site_cdom_v_eugleno_stats, "CDOM", "Eugleno")

# Salinity V Eugleno
salinity_v_Eugleno <- make_site_df(site_salinity_v_eugleno_stats, "Salinity", "Eugleno")

# Tide V Eugleno
tide_v_Eugleno <- make_site_df(site_tide_v_eugleno_stats, "Tidal Height", "Eugleno")

# Phosphate V Eugleno
phosphate_v_Eugleno <- make_site_df(site_phosphate_v_eugleno_stats, "Phosphate", "Eugleno")

# Nitrate V Eugleno
nitrate_v_Eugleno <- make_site_df(site_nitrate_v_eugleno_stats, "Nitrate", "Eugleno")

# combine all data frames into a master df
stats_by_site <- rbind(temp_v_chloro, temp_v_pc, temp_v_pe, salinity_v_chloro, salinity_v_pc, salinity_v_pe,
                       phosphate_v_chloro, phosphate_v_pc, phosphate_v_pe, nitrate_v_chloro, nitrate_v_pc,
                       nitrate_v_pe, tide_v_cdom, tide_v_chloro, tide_v_nitrate, tide_v_pc, tide_v_pe,
                       tide_v_phosphate, tide_v_salinity, ph_v_cdom, ph_v_chla, ph_v_nit, ph_v_pc,
                       ph_v_pe, ph_v_phos, ph_v_salinity, ph_v_temp, ph_v_tide, 
                       alk_v_cdom, alk_v_chla, alk_v_nit, alk_v_pc, alk_v_pe, alk_v_ph, alk_v_phos,
                        alk_v_salinity, alk_v_temp, alk_v_tide, pc_v_cyanos, pe_v_cyanos, temp_v_DinoA, 
                       cdom_v_DinoA, salinity_v_DinoA, tide_v_DinoA, phosphate_v_DinoA, nitrate_v_DinoA, 
                       temp_v_DinoB, cdom_v_DinoB, salinity_v_DinoB, tide_v_DinoB, phosphate_v_DinoB, 
                       nitrate_v_DinoB, temp_v_Cyanos, cdom_v_Cyanos, salinity_v_Cyanos, tide_v_Cyanos, 
                       phosphate_v_Cyanos, nitrate_v_Cyanos, temp_v_PrasinoA, cdom_v_PrasinoA, salinity_v_PrasinoA,
                       tide_v_PrasinoA, phosphate_v_PrasinoA, nitrate_v_PrasinoA, temp_v_PrasinoB, cdom_v_PrasinoB,
                       salinity_v_PrasinoB, tide_v_PrasinoB, phosphate_v_PrasinoB, nitrate_v_PrasinoB,
                       temp_v_Chlor, cdom_v_Chlor, salinity_v_Chlor, tide_v_Chlor, phosphate_v_Chlor, 
                       nitrate_v_Chlor, temp_v_RaphidoA, cdom_v_RaphidoA, salinity_v_RaphidoA, tide_v_RaphidoA, 
                       phosphate_v_RaphidoA, nitrate_v_RaphidoA, temp_v_HaptoA, cdom_v_HaptoA, salinity_v_HaptoA, 
                       tide_v_HaptoA, phosphate_v_HaptoA, nitrate_v_HaptoA, temp_v_Eugleno, cdom_v_Eugleno, 
                       salinity_v_Eugleno, tide_v_Eugleno, phosphate_v_Eugleno, nitrate_v_Eugleno)
head(stats_by_site)
colnames(stats_by_site) <- c("Site", "X Variable", "Y Variable", "R Squared", "P Value")
write.csv(stats_by_site, './data/stats_by_site.csv')
