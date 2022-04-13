library(ggplot2)
library(dplyr)

source('./scripts/run_first_functions.R')
load('./data/wk_master.Rdata')

# for chlorophyll temp and tide
date_list <- unique(wk_master$Date_Extracted)

# for chlorophyll and PE phosphate and nitrate
date_list2 <- date_list[10:26]

# for chlorophyll salinity
date_list3 <- date_list[7:26]

# for PE tide and temp
date_list4 <- date_list[-8]

# for PE salinity
date_list5 <- date_list3[-2]

# for PC temp and tidal height
date_list6 <- date_list[-16]
date_list6 <- date_list6[-16]
date_list6 <- date_list6[-8]

# for pc salinity
date_list7 <- date_list6[7:23]

# for pc phosphate and nitrate
date_list8 <- date_list6[9:23]

# creating scatter plots
# cdom vs tide
pdf('./figures/CDOM_vs_tide.pdf')
cdom_vs_tide_412_stats <- vector("list", length = length(date_list2))
for (i in seq_along(date_list2)) {
  p <- cor_ggplot(wk_master, date_list2[i], "Tidal_height", "cdom_abs_d", "Tidal height (ft)",
                  "CDOM (Absorbance at 412 nm)", date_list2[i])
  cdom_vs_tide_412_stats[[i]] <- p
}
dev.off()
save(cdom_vs_tide_412_stats, file = './stats/cdom_vs_tide_412_stats.Rdata')

# salinity vs tide
pdf('./figures/salinity_vs_tide.pdf')
salinity_vs_tide_stats <- vector("list", length = length(date_list3))
for (i in seq_along(date_list3)) {
  p <- cor_ggplot(wk_master, date_list3[i], "Salinity", "Tidal_height", "Salinity (ppt)",
                  "Tidal height (ft)", date_list3[i])
  salinity_vs_tide_stats[[i]] <- p
}
dev.off()
save(salinity_vs_tide_stats, file = './stats/salinity_vs_tide_stats.Rdata')

# tidal height vs chlorophyll
pdf('./figures/chlorophyll_vs_tide.pdf')
chlorophyll_vs_tide_stats <- vector("list", length = length(date_list))
for (i in seq_along(date_list)) {
  p <- cor_ggplot(wk_master, date_list[i], "Tidal_height", "Chlorophyll_conc", "Tidal Height (ft)",
                  "Chlorophyll Concentration (μg/L)", date_list[i])
  chlorophyll_vs_tide_stats[[i]] <- p
}
dev.off()
save(chlorophyll_vs_tide_stats, file = './stats/chlorophyll_vs_tide_stats.Rdata')

# salinty vs chlorophyll
pdf('./figures/chlorophyll_vs_salinity.pdf')
chlorophyll_vs_salinity_stats <- vector("list", length = length(date_list))
for (i in seq_along(date_list3)) {
  p <- cor_ggplot(wk_master, date_list3[i], "Salinity", "Chlorophyll_conc", "Salinity (ppt)",
                  "Chlorophyll Concentration (μg/L)", date_list3[i])
  chlorophyll_vs_salinity_stats[[i]] <- p
}
dev.off()
save(chlorophyll_vs_salinity_stats, file = './stats/chlorophyll_vs_salinity_stats.Rdata')

# temperature vs chlorophyll
pdf('./figures/chlorophyll_vs_temperature.pdf')
chlorophyll_vs_temperature_stats <- vector("list", length = length(date_list))
for (i in seq_along(date_list)) {
  p <- cor_ggplot(wk_master, date_list[i], "Temp", "Chlorophyll_conc", "Temperature (°C)",
                  "Chlorophyll Concentration (μg/L)", date_list[i])
  chlorophyll_vs_temperature_stats[[i]] <- p
}
dev.off()
save(chlorophyll_vs_temperature_stats, file = './stats/chlorophyll_vs_temperature_stats.Rdata')

# nitrate vs chlorophyll
pdf('./figures/chlorophyll_vs_nitrate.pdf')
chlorophyll_vs_nitrate_stats <- vector("list", length = length(date_list2))
for (i in seq_along(date_list2)) {
  p <- cor_ggplot(wk_master, date_list2[i], "Nitrate", "Chlorophyll_conc", "Nitrate (uM)",
                  "Chlorophyll Concentration (μg/L)", date_list2[i])
  chlorophyll_vs_nitrate_stats[[i]] <- p
}
dev.off()
save(chlorophyll_vs_nitrate_stats, file = './stats/chlorophyll_vs_nitrate_stats.Rdata')

# phosphate vs chlorophyll
pdf('./figures/chlorophyll_vs_phosphate.pdf')
chlorophyll_vs_phosphate_stats <- vector("list", length = length(date_list2))
for (i in seq_along(date_list2)) {
  p <- cor_ggplot(wk_master, date_list2[i], "Phosphate", "Chlorophyll_conc", "Phosphate (uM)",
                  "Chlorophyll Concentration (μg/L)", date_list2[i])
  chlorophyll_vs_phosphate_stats[[i]] <- p
}
dev.off()
save(chlorophyll_vs_phosphate_stats, file = './stats/chlorophyll_vs_phosphate_stats.Rdata')

# tidal height vs pc
pdf('./figures/phycocyanin_vs_tide.pdf')
pc_vs_tide_stats <- vector("list", length = length(date_list6))
for (i in seq_along(date_list6)) {
  p <- cor_ggplot(wk_master, date_list6[i], "Tidal_height", "Phycocyanin_conc", "Tidal Height (ft)",
                  "Phycocyanin Concentration (μg/L)", date_list6[i])
  pc_vs_tide_stats[[i]] <- p
}
dev.off()
save(pc_vs_tide_stats, file = './stats/pc_vs_tide_stats.Rdata')

# salinty vs pc
pdf('./figures/phycocyanin_vs_salinity.pdf')
pc_vs_salinity_stats <- vector("list", length = length(date_list7))
for (i in seq_along(date_list7)) {
  p <- cor_ggplot(wk_master, date_list7[i], "Salinity", "Phycocyanin_conc", "Salinity (ppt)",
                  "Phycocyanin Concentration (μg/L)", date_list7[i])
  pc_vs_salinity_stats[[i]] <- p
}
dev.off()
save(pc_vs_salinity_stats, file = './stats/pc_vs_salinity_stats.Rdata')

# temperature vs pc
pdf('./figures/phycocyanin_vs_temperature.pdf')
pc_vs_temperature_stats <- vector("list", length = length(date_list6))
for (i in seq_along(date_list6)) {
  p <- cor_ggplot(wk_master, date_list6[i], "Temp", "Phycocyanin_conc", "Temperature (°C)",
                  "Phycocyanin Concentration (μg/L)", date_list6[i])
  pc_vs_temperature_stats[[i]] <- p
}
dev.off()
save(pc_vs_temperature_stats, file = './stats/pc_vs_temperature_stats.Rdata')

# nitrate vs pc
pdf('./figures/phycocyanin_vs_nitrate.pdf')
pc_vs_nitrate_stats <- vector("list", length = length(date_list8))
for (i in seq_along(date_list8)) {
  p <- cor_ggplot(wk_master, date_list8[i], "Nitrate", "Phycocyanin_conc", "Nitrate (uM)",
                  "Phycocyanin Concentration (μg/L)", date_list8[i])
  pc_vs_nitrate_stats[[i]] <- p
}
dev.off()
save(pc_vs_nitrate_stats, file = './stats/pc_vs_nitrate_stats.Rdata')

# phosphate vs pc
pdf('./figures/Phycocyanin_vs_phosphate.pdf')
pc_vs_phosphate_stats <- vector("list", length = length(date_list8))
for (i in seq_along(date_list8)) {
  p <- cor_ggplot(wk_master, date_list8[i], "Phosphate", "Phycocyanin_conc", "Phosphate (uM)",
                  "Phycocyanin Concentration (μg/L)", date_list8[i])
  pc_vs_phosphate_stats[[i]] <- p
}
dev.off()
save(pc_vs_phosphate_stats, file = './stats/pc_vs_phosphate_stats.Rdata')

# tidal height vs pe
pdf('./figures/phycoerytherin_vs_tide.pdf')
pe_vs_tide_stats <- vector("list", length = length(date_list4))
for (i in seq_along(date_list4)) {
  p <- cor_ggplot(wk_master, date_list4[i], "Tidal_height", "Phycoerytherin_conc", "Tidal Height (ft)",
                  "Phycoeryhterin Concentration (μg/L)", date_list4[i])
  pe_vs_tide_stats[[i]] <- p
}
dev.off()
save(pe_vs_tide_stats, file = './stats/pe_vs_tide_stats.Rdata')

# salinty vs pe
pdf('./figures/phycoerytherin_vs_salinity.pdf')
pe_vs_salinity_stats <- vector("list", length = length(date_list5))
for (i in seq_along(date_list5)) {
  p <- cor_ggplot(wk_master, date_list5[i], "Salinity", "Phycoerytherin_conc", "Salinity (ppt)",
                  "Phycoerytherin Concentration (μg/L)", date_list5[i])
  pe_vs_salinity_stats[[i]] <- p
}
dev.off()
save(pe_vs_salinity_stats, file = './stats/pe_vs_salinity_stats.Rdata')

# temperature vs pe
pdf('./figures/phycoerytherin_vs_temperature.pdf')
pe_vs_temperature_stats <- vector("list", length = length(date_list4))
for (i in seq_along(date_list4)) {
  p <- cor_ggplot(wk_master, date_list4[i], "Temp", "Phycoerytherin_conc", "Temperature (°C)",
                  "Phycoerytherin Concentration (μg/L)", date_list4[i])
  pe_vs_temperature_stats[[i]] <- p
}
dev.off()
save(pe_vs_temperature_stats, file = './stats/pe_vs_temperature_stats.Rdata')

# nitrate vs pe
pdf('./figures/phycoerytherin_vs_nitrate.pdf')
pe_vs_nitrate_stats <- vector("list", length = length(date_list2))
for (i in seq_along(date_list2)) {
  p <- cor_ggplot(wk_master, date_list2[i], "Nitrate", "Phycoerytherin_conc", "Nitrate (uM)",
                  "Phycoerytherin Concentration (μg/L)", date_list2[i])
  pe_vs_nitrate_stats[[i]] <- p
}
dev.off()
save(pe_vs_nitrate_stats, file = './stats/pe_vs_nitrate_stats.Rdata')

# phosphate vs pe
pdf('./figures/Phycoerytherin_vs_phosphate.pdf')
pe_vs_phosphate_stats <- vector("list", length = length(date_list2))
for (i in seq_along(date_list2)) {
  p <- cor_ggplot(wk_master, date_list2[i], "Phosphate", "Phycoerytherin_conc", "Phosphate (uM)",
                  "Phycoerytherin Concentration (μg/L)", date_list2[i])
  pe_vs_phosphate_stats[[i]] <- p
}
dev.off()
save(pe_vs_phosphate_stats, file = './stats/pe_vs_phosphate_stats.Rdata')

# pc vs cyanos
pdf('./figures/Phycocyanin_vs_cyanos.pdf')
pc_vs_cyanos_stats <- vector("list", length = length(date_list6))
for (i in seq_along(date_list6)) {
  p <- cor_ggplot(wk_master, date_list6[i], "Phycocyanin_conc", "Cyanos", "Phycocyanin Concentration (μg/L)",
                  "Percent of total community composition attributible to Cyanos", date_list6[i])
  pc_vs_cyanos_stats[[i]] <- p
}
dev.off()
save(pc_vs_cyanos_stats, file = './stats/pc_vs_cyanos_stats.Rdata')

# pe vs cyanos
pdf('./figures/Phycoerytherin_vs_cyanos.pdf')
pe_vs_cyanos_stats <- vector("list", length = length(date_list4))
for (i in seq_along(date_list4)) {
  p <- cor_ggplot(wk_master, date_list4[i], "Phycoerytherin_conc", "Cyanos", "Phycoerytherin Concentration (μg/L)",
                  "Percent of total community composition attributible to Cyanos", date_list4[i])
  pe_vs_cyanos_stats[[i]] <- p
}
dev.off()
save(pe_vs_cyanos_stats, file = './stats/pe_vs_cyanos_stats.Rdata')

# creating plots by site
site_list <- unique(wk_master$Sample_ID)

# tidal height vs salinity
pdf('./figures/site_tide_vs_salinity.pdf')
site_tide_v_salinity_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Salinity", "Tidal Height (ft)", "Salinity (ppt)",
                   site_list[i])
  site_tide_v_salinity_stats[[i]] <- p
}
dev.off()
save(site_tide_v_salinity_stats, file = './stats/site_tide_vs_salinity_stats.Rdata')

# tidal height vs phosphate
pdf('./figures/site_tide_vs_phosphate.pdf')
site_tide_v_phosphate_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Phosphate", "Tidal Height (ft)", "Phosphate (uM)",
                   site_list[i])
  site_tide_v_phosphate_stats[[i]] <- p
}
dev.off()
save(site_tide_v_phosphate_stats, file = './stats/site_tide_vs_phosphate_stats.Rdata')

# tidal height vs nitrate
pdf('./figures/site_tide_vs_nitrate.pdf')
site_tide_v_nitrate_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Nitrate", "Tidal Height (ft)", "Nitrate (uM)",
                   site_list[i])
  site_tide_v_nitrate_stats[[i]] <- p
}
dev.off()
save(site_tide_v_nitrate_stats, file = './stats/site_tide_vs_nitrate_stats.Rdata')

# tidal height vs CDOM
pdf('./figures/site_tide_vs_cdom.pdf')
site_tide_v_cdom_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "cdom_abs_d", "Tidal Height (ft)", 
                   "CDOM (Absorbance at 412 nm)", site_list[i])
  site_tide_v_cdom_stats[[i]] <- p
}
dev.off()
save(site_tide_v_cdom_stats, file = './stats/site_tide_vs_cdom_stats.Rdata')

# tidal height vs chlorophyll
pdf('./figures/site_tide_vs_chlorophyll.pdf')
site_tide_v_chlorophyll_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Chlorophyll_conc", "Tidal Height (ft)", 
                   "Chlorophyll Concentration (μg/L)", site_list[i])
  site_tide_v_chlorophyll_stats[[i]] <- p
}
dev.off()
save(site_tide_v_chlorophyll_stats, file = './stats/site_tide_vs_chorophyll_stats.Rdata')

# Temperature vs chlorophyll
pdf('./figures/site_temperature_vs_chlorophyll.pdf')
site_temp_v_chlorophyll_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Chlorophyll_conc", "Temperature (°C)", 
                   "Chlorophyll Concentration (μg/L)", site_list[i])
  site_temp_v_chlorophyll_stats[[i]] <- p
}
dev.off()
save(site_temp_v_chlorophyll_stats, file = './stats/site_temp_vs_chorophyll_stats.Rdata')

# salinity vs chlorophyll
pdf('./figures/site_salinity_vs_chlorophyll.pdf')
site_salinity_v_chlorophyll_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Chlorophyll_conc", "Salinity (ppt)", 
                   "Chlorophyll Concentration (μg/L)", site_list[i])
  site_salinity_v_chlorophyll_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_chlorophyll_stats, file = './stats/site_salinity_vs_chorophyll_stats.Rdata')

# phosphate vs chlorophyll
pdf('./figures/site_phosphate_vs_chlorophyll.pdf')
site_phosphate_v_chlorophyll_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Chlorophyll_conc", "Phosphate (uM)", 
                   "Chlorophyll Concentration (μg/L)", site_list[i])
  site_phosphate_v_chlorophyll_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_chlorophyll_stats, file = './stats/site_phosphate_vs_chorophyll_stats.Rdata')

# Nitrate vs chlorophyll
pdf('./figures/site_nitrate_vs_chlorophyll.pdf')
site_nitrate_v_chlorophyll_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Chlorophyll_conc", "Nitrate (uM)", 
                   "Chlorophyll Concentration (μg/L)", site_list[i])
  site_nitrate_v_chlorophyll_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_chlorophyll_stats, file = './stats/site_nitrate_vs_chorophyll_stats.Rdata')

# tidal height vs phycocyanin
pdf('./figures/site_tide_vs_phycocyanin.pdf')
site_tide_v_phycocyanin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Phycocyanin_conc", "Tidal Height (ft)", 
                   "Phycocyanin Concentration (μg/L)", site_list[i])
  site_tide_v_phycocyanin_stats[[i]] <- p
}
dev.off()
save(site_tide_v_phycocyanin_stats, file = './stats/site_tide_vs_phycocyanin_stats.Rdata')

# Temperature vs phycocyanin
pdf('./figures/site_temperature_vs_phycocyanin.pdf')
site_temp_v_phycocyanin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Phycocyanin_conc", "Temperature (°C)", 
                   "Phycocyanin Concentration (μg/L)", site_list[i])
  site_temp_v_phycocyanin_stats[[i]] <- p
}
dev.off()
save(site_temp_v_phycocyanin_stats, file = './stats/site_temp_vs_phycocyanin_stats.Rdata')

# salinity vs phycocyanin
pdf('./figures/site_salinity_vs_phycocyanin.pdf')
site_salinity_v_phycocyanin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Phycocyanin_conc", "Salinity (ppt)", 
                   "Phycocyanin Concentration (μg/L)", site_list[i])
  site_salinity_v_phycocyanin_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_phycocyanin_stats, file = './stats/site_salinity_vs_phycocyanin_stats.Rdata')

# phosphate vs phycocyanin
pdf('./figures/site_phosphate_vs_phycocyanin.pdf')
site_phosphate_v_phycocyanin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Phycocyanin_conc", "Phosphate (uM)", 
                   "Phycocyanin Concentration (μg/L)", site_list[i])
  site_phosphate_v_phycocyanin_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_phycocyanin_stats, file = './stats/site_phosphate_vs_phycocyanin_stats.Rdata')

# Nitrate vs phycocyanin
pdf('./figures/site_nitrate_vs_phycocyanin.pdf')
site_nitrate_v_phycocyanin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Phycocyanin_conc", "Nitrate (uM)", 
                   "Phycocyanin Concentration (μg/L)", site_list[i])
  site_nitrate_v_phycocyanin_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_phycocyanin_stats, file = './stats/site_nitrate_vs_phycocyanin_stats.Rdata')

# tidal height vs phycoerytherin
pdf('./figures/site_tide_vs_phycoerytherin.pdf')
site_tide_v_phycoerytherin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Phycoerytherin_conc", "Tidal Height (ft)", 
                   "Phycoerytherin Concentration (μg/L)", site_list[i])
  site_tide_v_phycoerytherin_stats[[i]] <- p
}
dev.off()
save(site_tide_v_phycoerytherin_stats, file = './stats/site_tide_vs_phycoerytherin_stats.Rdata')

# Temperature vs phycoerytherin
pdf('./figures/site_temperature_vs_phycoerytherin.pdf')
site_temp_v_phycoerytherin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Phycoerytherin_conc", "Temperature (°C)", 
                   "Phycoerytherin Concentration (μg/L)", site_list[i])
  site_temp_v_phycoerytherin_stats[[i]] <- p
}
dev.off()
save(site_temp_v_phycoerytherin_stats, file = './stats/site_temp_vs_phycoerytherin_stats.Rdata')

# salinity vs phycoerytherin
pdf('./figures/site_salinity_vs_phycoerytherin.pdf')
site_salinity_v_phycoerytherin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Phycoerytherin_conc", "Salinity (ppt)", 
                   "Phycoerytherin Concentration (μg/L)", site_list[i])
  site_salinity_v_phycoerytherin_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_phycoerytherin_stats, file = './stats/site_salinity_vs_phycoerytherin_stats.Rdata')

# phosphate vs phycoerytherin
pdf('./figures/site_phosphate_vs_phycoerytherin.pdf')
site_phosphate_v_phycoerytherin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Phycoerytherin_conc", "Phosphate (uM)", 
                   "Phycoerytherin Concentration (μg/L)", site_list[i])
  site_phosphate_v_phycoerytherin_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_phycoerytherin_stats, file = './stats/site_phosphate_vs_phycoerytherin_stats.Rdata')

# Nitrate vs phycoerytherin
pdf('./figures/site_nitrate_vs_phycoerytherin.pdf')
site_nitrate_v_phycoerytherin_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Phycoerytherin_conc", "Nitrate (uM)", 
                   "Phycoerytherin Concentration (μg/L)", site_list[i])
  site_nitrate_v_phycoerytherin_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_phycoerytherin_stats, file = './stats/site_nitrate_vs_phycoerytherin_stats.Rdata')

# phycocyanin vs cyanos
pdf('./figures/site_phycocyanin_vs_cyanos.pdf')
site_phycocyanin_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phycocyanin_conc", "Cyanos", "Phycocyanin Concentration (μg/L)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_phycocyanin_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_phycocyanin_v_cyanos_stats, file = './stats/site_phycocyanin_vs_cyanos_stats.Rdata')

# phycoerytherin vs cyanos
pdf('./figures/site_phycoerytherin_vs_cyanos.pdf')
site_phycoerytherin_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phycoerytherin_conc", "Cyanos", "Phycoerytherin Concentration (μg/L)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_phycoerytherin_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_phycoerytherin_v_cyanos_stats, file = './stats/site_phycoerytherin_vs_cyanos_stats.Rdata')