library(ggplot2)
library(dplyr)

source('./scripts/run_first_functions.R')
load('./data/wk_master.Rdata')

site_list <- unique(wk_master$Sample_ID)

# temp vs dinoA
pdf('./figures/site_temp_vs_dinoA.pdf')
site_temp_v_dinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Diatoms.DinoA", "Temperature (°C)", 
                   "Percent of total community composition attributible to Diatoms DinoA", site_list[i])
  site_temp_v_dinoa_stats[[i]] <- p
}
dev.off()
save(site_temp_v_dinoa_stats, file = './stats/site_temp_vs_dinoa_stats.Rdata')

# cdom vs dinoA
pdf('./figures/site_cdom_vs_dinoA.pdf')
site_cdom_v_dinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "Diatoms.DinoA", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to Diatoms DinoA", site_list[i])
  site_cdom_v_dinoa_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_dinoa_stats, file = './stats/site_cdom_vs_dinoa_stats.Rdata')

# salinity vs dinoA
pdf('./figures/site_salinity_vs_dinoA.pdf')
site_salinity_v_dinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Diatoms.DinoA", "Salinity (ppt)", 
                   "Percent of total community composition attributible to Diatoms DinoA", site_list[i])
  site_salinity_v_dinoa_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_dinoa_stats, file = './stats/site_salinity_vs_dinoa_stats.Rdata')

# tide vs dinoA
pdf('./figures/site_tide_vs_dinoA.pdf')
site_tide_v_dinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Diatoms.DinoA", "Tidal Height (in)", 
                   "Percent of total community composition attributible to Diatoms DinoA", site_list[i])
  site_tide_v_dinoa_stats[[i]] <- p
}
dev.off()
save(site_tide_v_dinoa_stats, file = './stats/site_tide_vs_dinoa_stats.Rdata')

# phosphate vs dinoA
pdf('./figures/site_phosphate_vs_dinoA.pdf')
site_phosphate_v_dinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Diatoms.DinoA", "Phosphate (uM)", 
                   "Percent of total community composition attributible to Diatoms DinoA", site_list[i])
  site_phosphate_v_dinoa_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_dinoa_stats, file = './stats/site_phosphate_vs_dinoa_stats.Rdata')

# nitrate vs dinoA
pdf('./figures/site_nitrate_vs_dinoA.pdf')
site_nitrate_v_dinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Diatoms.DinoA", "Nitrate (uM)", 
                   "Percent of total community composition attributible to Diatoms DinoA", site_list[i])
  site_nitrate_v_dinoa_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_dinoa_stats, file = './stats/site_nitrate_vs_dinoa_stats.Rdata')

# temp vs dinoB
pdf('./figures/site_temp_vs_dinoB.pdf')
site_temp_v_dinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "DinoB", "Temperature (°C)", 
                   "Percent of total community composition attributible to Diatoms DinoB", site_list[i])
  site_temp_v_dinob_stats[[i]] <- p
}
dev.off()
save(site_temp_v_dinob_stats, file = './stats/site_temp_vs_dinob_stats.Rdata')

# cdom vs dinoB
pdf('./figures/site_cdom_vs_dinoB.pdf')
site_cdom_v_dinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "DinoB", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to Diatoms DinoB", site_list[i])
  site_cdom_v_dinob_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_dinob_stats, file = './stats/site_cdom_vs_dinob_stats.Rdata')

# salinity vs dinoB
pdf('./figures/site_salinity_vs_dinoB.pdf')
site_salinity_v_dinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "DinoB", "Salinity (ppt)", 
                   "Percent of total community composition attributible to Diatoms DinoB", site_list[i])
  site_salinity_v_dinob_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_dinob_stats, file = './stats/site_salinity_vs_dinob_stats.Rdata')

# tide vs dinoB
pdf('./figures/site_tide_vs_dinoB.pdf')
site_tide_v_dinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "DinoB", "Tidal Height (in)", 
                   "Percent of total community composition attributible to Diatoms DinoB", site_list[i])
  site_tide_v_dinob_stats[[i]] <- p
}
dev.off()
save(site_tide_v_dinob_stats, file = './stats/site_tide_vs_dinob_stats.Rdata')

# phosphate vs dinoB
pdf('./figures/site_phosphate_vs_dinoB.pdf')
site_phosphate_v_dinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "DinoB", "Phosphate (uM)", 
                   "Percent of total community composition attributible to Diatoms DinoB", site_list[i])
  site_phosphate_v_dinob_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_dinob_stats, file = './stats/site_phosphate_vs_dinob_stats.Rdata')

# nitrate vs dinoB
pdf('./figures/site_nitrate_vs_dinoB.pdf')
site_nitrate_v_dinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "DinoB", "Nitrate (uM)", 
                   "Percent of total community composition attributible to Diatoms DinoB", site_list[i])
  site_nitrate_v_dinob_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_dinob_stats, file = './stats/site_nitrate_vs_dinob_stats.Rdata')

# temp vs cyanos
pdf('./figures/site_temp_vs_cyanos.pdf')
site_temp_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Cyanos", "Temperature (°C)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_temp_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_temp_v_cyanos_stats, file = './stats/site_temp_vs_cyanos_stats.Rdata')

# cdom vs cyanos
pdf('./figures/site_cdom_vs_cyanos.pdf')
site_cdom_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "Cyanos", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_cdom_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_cyanos_stats, file = './stats/site_cdom_vs_cyanos_stats.Rdata')

# salinity vs cyanos
pdf('./figures/site_salinity_vs_cyanos.pdf')
site_salinity_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Cyanos", "Salinity (ppt)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_salinity_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_cyanos_stats, file = './stats/site_salinity_vs_cyanos_stats.Rdata')

# tide vs cyanos
pdf('./figures/site_tide_vs_cyanos.pdf')
site_tide_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Cyanos", "Tidal Height (in)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_tide_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_tide_v_cyanos_stats, file = './stats/site_tide_vs_cyanos_stats.Rdata')

# phosphate vs cyanos
pdf('./figures/site_phosphate_vs_cyanos.pdf')
site_phosphate_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Cyanos", "Phosphate (uM)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_phosphate_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_cyanos_stats, file = './stats/site_phosphate_vs_cyanos_stats.Rdata')

# nitrate vs cyanos
pdf('./figures/site_nitrate_vs_cyanos.pdf')
site_nitrate_v_cyanos_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Cyanos", "Nitrate (uM)", 
                   "Percent of total community composition attributible to Cyanos", site_list[i])
  site_nitrate_v_cyanos_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_cyanos_stats, file = './stats/site_nitrate_vs_cyanos_stats.Rdata')

# temp vs prasinoA
pdf('./figures/site_temp_vs_prasinoa.pdf')
site_temp_v_prasinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "PrasinoA", "Temperature (°C)", 
                   "Percent of total community composition attributible to PrasinoA", site_list[i])
  site_temp_v_prasinoa_stats[[i]] <- p
}
dev.off()
save(site_temp_v_prasinoa_stats, file = './stats/site_temp_vs_prasinoa_stats.Rdata')

# cdom vs prasinoA
pdf('./figures/site_cdom_vs_prasinoa.pdf')
site_cdom_v_prasinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "PrasinoA", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to PrasinoA", site_list[i])
  site_cdom_v_prasinoa_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_prasinoa_stats, file = './stats/site_cdom_vs_prasinoa_stats.Rdata')

# salinity vs prasinoA
pdf('./figures/site_salinity_vs_prasinoa.pdf')
site_salinity_v_prasinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "PrasinoA", "Salinity (ppt)", 
                   "Percent of total community composition attributible to PrasinoA", site_list[i])
  site_salinity_v_prasinoa_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_prasinoa_stats, file = './stats/site_salinity_vs_prasinoa_stats.Rdata')

# tide vs prasinoA
pdf('./figures/site_tide_vs_prasinoa.pdf')
site_tide_v_prasinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "PrasinoA", "Tidal Height (in)", 
                   "Percent of total community composition attributible to PrasinoA", site_list[i])
  site_tide_v_prasinoa_stats[[i]] <- p
}
dev.off()
save(site_tide_v_prasinoa_stats, file = './stats/site_tide_vs_prasinoa_stats.Rdata')

# phosphate vs prasinoA
pdf('./figures/site_phosphate_vs_prasinoa.pdf')
site_phosphate_v_prasinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "PrasinoA", "Phosphate (uM)", 
                   "Percent of total community composition attributible to PrasinoA", site_list[i])
  site_phosphate_v_prasinoa_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_prasinoa_stats, file = './stats/site_phosphate_vs_prasinoa_stats.Rdata')

# nitrate vs prasinoa
pdf('./figures/site_nitrate_vs_prasinoa.pdf')
site_nitrate_v_prasinoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "PrasinoA", "Nitrate (uM)", 
                   "Percent of total community composition attributible to PrasinoA", site_list[i])
  site_nitrate_v_prasinoa_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_prasinoa_stats, file = './stats/site_nitrate_vs_prasinoa_stats.Rdata')

# temp vs prasinoB
pdf('./figures/site_temp_vs_prasinob.pdf')
site_temp_v_prasinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "PrasinoB", "Temperature (°C)", 
                   "Percent of total community composition attributible to PrasinoB", site_list[i])
  site_temp_v_prasinob_stats[[i]] <- p
}
dev.off()
save(site_temp_v_prasinob_stats, file = './stats/site_temp_vs_prasinob_stats.Rdata')

# cdom vs prasinoB
pdf('./figures/site_cdom_vs_prasinob.pdf')
site_cdom_v_prasinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "PrasinoB", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to PrasinoB", site_list[i])
  site_cdom_v_prasinob_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_prasinob_stats, file = './stats/site_cdom_vs_prasinob_stats.Rdata')

# salinity vs prasinoB
pdf('./figures/site_salinity_vs_prasinob.pdf')
site_salinity_v_prasinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "PrasinoB", "Salinity (ppt)", 
                   "Percent of total community composition attributible to PrasinoB", site_list[i])
  site_salinity_v_prasinob_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_prasinob_stats, file = './stats/site_salinity_vs_prasinob_stats.Rdata')

# tide vs prasinoB
pdf('./figures/site_tide_vs_prasinob.pdf')
site_tide_v_prasinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "PrasinoB", "Tidal Height (in)", 
                   "Percent of total community composition attributible to PrasinoB", site_list[i])
  site_tide_v_prasinob_stats[[i]] <- p
}
dev.off()
save(site_tide_v_prasinob_stats, file = './stats/site_tide_vs_prasinob_stats.Rdata')

# phosphate vs prasinoB
pdf('./figures/site_phosphate_vs_prasinob.pdf')
site_phosphate_v_prasinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "PrasinoB", "Phosphate (uM)", 
                   "Percent of total community composition attributible to PrasinoB", site_list[i])
  site_phosphate_v_prasinob_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_prasinob_stats, file = './stats/site_phosphate_vs_prasinob_stats.Rdata')

# nitrate vs prasinoB
pdf('./figures/site_nitrate_vs_prasinob.pdf')
site_nitrate_v_prasinob_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "PrasinoB", "Nitrate (uM)", 
                   "Percent of total community composition attributible to PrasinoB", site_list[i])
  site_nitrate_v_prasinob_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_prasinob_stats, file = './stats/site_nitrate_vs_prasinob_stats.Rdata')

# temp vs chloro
pdf('./figures/site_temp_vs_chloro.pdf')
site_temp_v_chloro_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Chloro", "Temperature (°C)", 
                   "Percent of total community composition attributible to Chloro", site_list[i])
  site_temp_v_chloro_stats[[i]] <- p
}
dev.off()
save(site_temp_v_chloro_stats, file = './stats/site_temp_vs_chloro_stats.Rdata')

# cdom vs chloro
pdf('./figures/site_cdom_vs_chloro.pdf')
site_cdom_v_chloro_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "Chloro", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to Chloro", site_list[i])
  site_cdom_v_chloro_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_chloro_stats, file = './stats/site_cdom_vs_chloro_stats.Rdata')

# salinity vs chloro
pdf('./figures/site_salinity_vs_chloro.pdf')
site_salinity_v_chloro_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Chloro", "Salinity (ppt)", 
                   "Percent of total community composition attributible to Chloro", site_list[i])
  site_salinity_v_chloro_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_chloro_stats, file = './stats/site_salinity_vs_chloro_stats.Rdata')

# tide vs chloro
pdf('./figures/site_tide_vs_chloro.pdf')
site_tide_v_chloro_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Chloro", "Tidal Height (in)", 
                   "Percent of total community composition attributible to Chloro", site_list[i])
  site_tide_v_chloro_stats[[i]] <- p
}
dev.off()
save(site_tide_v_chloro_stats, file = './stats/site_tide_vs_chloro_stats.Rdata')

# phosphate vs chloro
pdf('./figures/site_phosphate_vs_chloro.pdf')
site_phosphate_v_chloro_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Chloro", "Phosphate (uM)", 
                   "Percent of total community composition attributible to Chloro", site_list[i])
  site_phosphate_v_chloro_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_chloro_stats, file = './stats/site_phosphate_vs_chloro_stats.Rdata')


# nitrate vs chloro
pdf('./figures/site_nitrate_vs_chloro.pdf')
site_nitrate_v_chloro_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Chloro", "Nitrate (uM)", 
                   "Percent of total community composition attributible to Chloro", site_list[i])
  site_nitrate_v_chloro_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_chloro_stats, file = './stats/site_nitrate_vs_chloro_stats.Rdata')

# temp vs RaphidoA
pdf('./figures/site_temp_vs_raphidoa.pdf')
site_temp_v_raphidoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "RaphidoA", "Temperature (°C)", 
                   "Percent of total community composition attributible to RaphidoA", site_list[i])
  site_temp_v_raphidoa_stats[[i]] <- p
}
dev.off()
save(site_temp_v_raphidoa_stats, file = './stats/site_temp_vs_raphidoa_stats.Rdata')

# cdom vs RaphidoA
pdf('./figures/site_cdom_vs_raphidoa.pdf')
site_cdom_v_raphidoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "RaphidoA", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to RaphidoA", site_list[i])
  site_cdom_v_raphidoa_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_raphidoa_stats, file = './stats/site_cdom_vs_raphidoa_stats.Rdata')

# salinity vs RaphidoA
pdf('./figures/site_salinity_vs_raphidoa.pdf')
site_salinity_v_raphidoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "RaphidoA", "Salinity (ppt)", 
                   "Percent of total community composition attributible to RaphidoA", site_list[i])
  site_salinity_v_raphidoa_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_raphidoa_stats, file = './stats/site_salinity_vs_raphidoa_stats.Rdata')

# tide vs RaphidoA
pdf('./figures/site_tide_vs_raphidoa.pdf')
site_tide_v_raphidoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "RaphidoA", "Tidal Height (in)", 
                   "Percent of total community composition attributible to RaphidoA", site_list[i])
  site_tide_v_raphidoa_stats[[i]] <- p
}
dev.off()
save(site_tide_v_raphidoa_stats, file = './stats/site_tide_vs_raphidoa_stats.Rdata')

# phosphate vs RaphidoA
pdf('./figures/site_phosphate_vs_raphidoa.pdf')
site_phosphate_v_raphidoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "RaphidoA", "Phosphate (uM)", 
                   "Percent of total community composition attributible to RaphidoA", site_list[i])
  site_phosphate_v_raphidoa_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_raphidoa_stats, file = './stats/site_phosphate_vs_raphidoa_stats.Rdata')


# nitrate vs RaphidoA
pdf('./figures/site_nitrate_vs_raphidoa.pdf')
site_nitrate_v_raphidoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "RaphidoA", "Nitrate (uM)", 
                   "Percent of total community composition attributible to RaphidoA", site_list[i])
  site_nitrate_v_raphidoa_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_raphidoa_stats, file = './stats/site_nitrate_vs_raphidoa_stats.Rdata')

# temp vs HaptoA
pdf('./figures/site_temp_vs_haptoa.pdf')
site_temp_v_haptoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "HaptoA.ChrysoA.DinoC", "Temperature (°C)", 
                   "Percent of total community composition attributible to HaptoA.ChrysoA.DinoC", site_list[i])
  site_temp_v_haptoa_stats[[i]] <- p
}
dev.off()
save(site_temp_v_haptoa_stats, file = './stats/site_temp_vs_haptoa_stats.Rdata')

# cdom vs HaptoA
pdf('./figures/site_cdom_vs_haptoa.pdf')
site_cdom_v_haptoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "HaptoA.ChrysoA.DinoC", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to HaptoA.ChrysoA.DinoC", site_list[i])
  site_cdom_v_haptoa_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_haptoa_stats, file = './stats/site_cdom_vs_haptoa_stats.Rdata')

# salinity vs HaptoA
pdf('./figures/site_salinity_vs_haptoa.pdf')
site_salinity_v_haptoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "HaptoA.ChrysoA.DinoC", "Salinity (ppt)", 
                   "Percent of total community composition attributible to HaptoA.ChrysoA.DinoC", site_list[i])
  site_salinity_v_haptoa_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_haptoa_stats, file = './stats/site_salinity_vs_haptoa_stats.Rdata')

# tide vs HaptoA
pdf('./figures/site_tide_vs_haptoa.pdf')
site_tide_v_haptoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "HaptoA.ChrysoA.DinoC", "Tidal Height (in)", 
                   "Percent of total community composition attributible to HaptoA.ChrysoA.DinoC", site_list[i])
  site_tide_v_haptoa_stats[[i]] <- p
}
dev.off()
save(site_tide_v_haptoa_stats, file = './stats/site_tide_vs_haptoa_stats.Rdata')

# phosphate vs HaptoA
pdf('./figures/site_phosphate_vs_haptoa.pdf')
site_phosphate_v_haptoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "HaptoA.ChrysoA.DinoC", "Phosphate (uM)", 
                   "Percent of total community composition attributible to HaptoA.ChrysoA.DinoC", site_list[i])
  site_phosphate_v_haptoa_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_haptoa_stats, file = './stats/site_phosphate_vs_haptoa_stats.Rdata')


# nitrate vs HaptoA
pdf('./figures/site_nitrate_vs_haptoa.pdf')
site_nitrate_v_haptoa_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "HaptoA.ChrysoA.DinoC", "Nitrate (uM)", 
                   "Percent of total community composition attributible to HaptoA.ChrysoA.DinoC", site_list[i])
  site_nitrate_v_haptoa_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_haptoa_stats, file = './stats/site_nitrate_vs_haptoa_stats.Rdata')

# temp vs Eugleno
pdf('./figures/site_temp_vs_eugleno.pdf')
site_temp_v_eugleno_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Temp", "Eugleno", "Temperature (°C)", 
                   "Percent of total community composition attributible to Eugleno", site_list[i])
  site_temp_v_eugleno_stats[[i]] <- p
}
dev.off()
save(site_temp_v_eugleno_stats, file = './stats/site_temp_vs_eugleno_stats.Rdata')

# cdom vs Eugleno
pdf('./figures/site_cdom_vs_eugleno.pdf')
site_cdom_v_eugleno_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "cdom_abs_d", "Eugleno", "CDOM (Absorbance at 412 nm)", 
                   "Percent of total community composition attributible to Eugleno", site_list[i])
  site_cdom_v_eugleno_stats[[i]] <- p
}
dev.off()
save(site_cdom_v_eugleno_stats, file = './stats/site_cdom_vs_eugleno_stats.Rdata')

# salinity vs Eugleno
pdf('./figures/site_salinity_vs_eugleno.pdf')
site_salinity_v_eugleno_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Salinity", "Eugleno", "Salinity (ppt)", 
                   "Percent of total community composition attributible to Eugleno", site_list[i])
  site_salinity_v_eugleno_stats[[i]] <- p
}
dev.off()
save(site_salinity_v_eugleno_stats, file = './stats/site_salinity_vs_eugleno_stats.Rdata')

# tide vs Eugleno
pdf('./figures/site_tide_vs_eugleno.pdf')
site_tide_v_eugleno_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Tidal_height", "Eugleno", "Tidal Height (in)", 
                   "Percent of total community composition attributible to Eugleno", site_list[i])
  site_tide_v_eugleno_stats[[i]] <- p
}
dev.off()
save(site_tide_v_eugleno_stats, file = './stats/site_tide_vs_eugleno_stats.Rdata')

# phosphate vs Eugleno
pdf('./figures/site_phosphate_vs_eugleno.pdf')
site_phosphate_v_eugleno_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Phosphate", "Eugleno", "Phosphate (uM)", 
                   "Percent of total community composition attributible to Eugleno", site_list[i])
  site_phosphate_v_eugleno_stats[[i]] <- p
}
dev.off()
save(site_phosphate_v_eugleno_stats, file = './stats/site_phosphate_vs_eugleno_stats.Rdata')


# nitrate vs Eugleno
pdf('./figures/site_nitrate_vs_eugleno.pdf')
site_nitrate_v_eugleno_stats <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
  p <- site_ggplot(wk_master, site_list[i], "Nitrate", "Eugleno", "Nitrate (uM)", 
                   "Percent of total community composition attributible to Eugleno", site_list[i])
  site_nitrate_v_eugleno_stats[[i]] <- p
}
dev.off()
save(site_nitrate_v_eugleno_stats, file = './stats/site_nitrate_vs_eugleno_stats.Rdata')