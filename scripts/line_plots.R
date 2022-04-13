library(ggplot2)
library(dplyr)
library(reshape2)

source('./scripts/run_first_functions.R')
load('./data/wk_master.Rdata')

site_list <- unique(wk_master$Sample_ID)

# making line plots for chlorophyll
pdf('./figures/chlorophyll_through_time.pdf')
for (i in seq_along(site_list)) {
  line_ggplot(wk_master, site_list[i], "Chlorophyll_conc", "Chlorophyll Concentration (μg/L)", 
              paste0("Chlorophyll Concentration through Time at Site ", site_list[i]))
}
dev.off()

# making line plots for phycocyanin
pdf('./figures/phycocyanin_through_time.pdf')
for (i in seq_along(site_list)) {
  line_ggplot(wk_master, site_list[i], "Phycocyanin_conc", "Phycocyanin Concentration (μg/L)", 
              paste0("Phycocyanin Concentration through Time at Site ", site_list[i]))
}
dev.off()

# making line plots for phycoerytherin
pdf('./figures/phycoerytherin_through_time.pdf')
for (i in seq_along(site_list)) {
  line_ggplot(wk_master, site_list[i], "Phycoerytherin_conc", "Phycoerytherin Concentration (μg/L)", 
              paste0("Phycoerytherin Concentration through Time at Site ", site_list[i]))
}
dev.off()

# line plots with chemtax pigment data
pdf('./figures/pigments_site_through_time.pdf')
for (i in seq_along(site_list)) {
temp_dat <- wk_master %>% filter(Sample_ID == site_list[i])
temp_dat <- temp_dat[order(as.Date(temp_dat$Date_Extracted, format="%m/%d/%Y")),]
temp_dat$Date_Extracted <- factor(temp_dat$Date_Extracted, levels = unique(temp_dat$Date_Extracted))
temp_dat <- data.frame(temp_dat$Date_Extracted, temp_dat$Sample_ID, temp_dat[,33:41])
temp_dat <- melt(data = temp_dat, id.vars = c("temp_dat.Date_Extracted", "temp_dat.Sample_ID"),
                            value.name = "value")
p <- ggplot(data = temp_dat, aes(x = temp_dat.Date_Extracted, y = value, group = variable)) +
  geom_line(aes(color = variable)) +
  xlab("Date") +
  ylab("Percent Community Composition") +
  ggtitle(paste0("Community Composition through Time at Site ", site_list[i])) +
  theme(axis.text.x = element_text(angle = 90))
print(p)
}
dev.off()

