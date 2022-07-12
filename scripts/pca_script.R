# PCA analysis
library(dplyr)
library(ggplot2)
library(ggfortify)

load('./data/wk_master.Rdata')

# create matrix of wanted variables only
new_matrix <- cbind(wk_master$Sample_ID, wk_master$Date_Extracted, wk_master$Tide_Class,
                    wk_master$Tidal_height, 
                    wk_master$Chlorophyll_conc,
                    wk_master$Phycocyanin_conc, wk_master$Temp, wk_master$Nitrate, wk_master$Phosphate)
View(new_matrix)
class(new_matrix)
new_matrix <- data.frame(new_matrix)
colnames(new_matrix) <- c("Sample_ID", "Date_Extracted", "Tide_Class", "Tide", "Chlorophyll", "Phycocyanin",
                          "Temperature", "Nitrate", "Phosphate")

site_list <- unique(new_matrix$Sample_ID)
pca_list <- vector("list", length = 20)

# pca by site
for (i in seq_along(site_list)) {
site_df <- new_matrix %>% filter(Sample_ID == site_list[i])
site_mat <- site_df[, 4:9]
site_mat$Tide <- as.numeric(site_mat$Tide)
site_mat$Chlorophyll <- as.numeric(site_mat$Chlorophyll)
site_mat$Phycocyanin <- as.numeric(site_mat$Phycocyanin)
site_mat$Temperature <- as.numeric(site_mat$Temperature)
site_mat$Nitrate <- as.numeric(site_mat$Nitrate)
site_mat$Phosphate <- as.numeric(site_mat$Phosphate)
site_mat <- na.omit(site_mat)
pca <- prcomp(site_mat, scale = T, center = T)
pca_list[[i]] <- pca
}
save(pca_list, file = './stats/pca_list.Rdata')

summary(pca_list[[1]])
cov(pca_list[[1]]$x)
pca_list[[1]]$sdev
biplot(pca_list[[1]])

pdf('./figures/pca_biplots.pdf')
for (i in seq_along(site_list)) {
  biplot(pca_list[[i]], scale = 0,
         xlim = c(-3,3), ylim = c(-3,3))
  title(paste0("Biplot of Principal Components for Site ", site_list[i]), line = 2.8)
}
dev.off()

# pca by date
date_pca <- na.omit(new_matrix)
date_list <- unique(date_pca$Date_Extracted)
pca_by_date <- vector("list", length = length(date_list))

for (i in seq_along(date_list)) {
  site_df <- date_pca %>% filter(Date_Extracted == date_list[i])
  site_mat <- site_df[, 4:9]
  site_mat$Tide <- as.numeric(site_mat$Tide)
  site_mat$Chlorophyll <- as.numeric(site_mat$Chlorophyll)
  site_mat$Phycocyanin <- as.numeric(site_mat$Phycocyanin)
  site_mat$Temperature <- as.numeric(site_mat$Temperature)
  site_mat$Nitrate <- as.numeric(site_mat$Nitrate)
  site_mat$Phosphate <- as.numeric(site_mat$Phosphate)
  pca <- prcomp(site_mat, scale = T, center = T)
  pca_by_date[[i]] <- pca
}
save(pca_by_date, file = './stats/pca_by_date.Rdata')

summary(pca_by_date[[1]])

pdf('./figures/pca_by_date_biplots.pdf')
for (i in seq_along(date_list)) {
  biplot(pca_by_date[[i]], scale = 0,
         xlim = c(-3,3), ylim = c(-3,3))
  title(paste0("Biplot of Principal Components for ", date_list[i]), line = 2.8)
}
dev.off()
  
# function to visualize pca
pcaCharts <- function(x, plot_title) {
  x.var <- x$sdev ^ 2
  x.pvar <- x.var/sum(x.var)
  print("Proportions of Variance:")
  print(x.pvar)
  
  par(mfrow=c(2,2))
  plot(x.pvar,xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1), type='b', 
       main = plot_title)
  plot(cumsum(x.pvar),xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1), 
       type='b', main = plot_title)
  screeplot(x, main = plot_title)
  screeplot(x,type="l", main = plot_title)
  par(mfrow=c(1,1))
}

# function test
pcaCharts(pca_list[[1]], plot_title = "AR1")

pdf('./figures/pca_visualization.pdf')
for (i in seq_along(site_list)) {
  pcaCharts(pca_list[[i]], site_list[i])
}
dev.off()

# dataframe for loadings
rot_list <- vector("list", length = length(site_list))
for (i in seq_along(site_list)) {
site_col <- rep(site_list[i], times = 6)
rot_mat <- pca_list[[i]]$rotation
rot_df <- data.frame(rot_mat)
rot_df <- cbind(rot_df, site_col)
colnames(rot_df) <- c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "Site")
rot_list[[i]] <- rot_df
}

rot_list[[1]]
rotation_dataframe <- rbind(rot_list[[1]], rot_list[[2]], rot_list[[3]], rot_list[[4]],
                            rot_list[[5]], rot_list[[6]], rot_list[[7]], rot_list[[8]], 
                            rot_list[[9]], rot_list[[10]], rot_list[[11]], rot_list[[12]],
                            rot_list[[13]], rot_list[[14]], rot_list[[15]], rot_list[[16]],
                            rot_list[[17]], rot_list[[18]], rot_list[[19]], rot_list[[20]])
rownames(rotation_dataframe) <- NULL
vari <- rep(c("Tidal Height", "Chlorophyll", "Phycocyanin", "Temperature", "Nitrate", "Phosphate"), times = 20)
rotation_dataframe <- cbind(vari, rotation_dataframe)
colnames(rotation_dataframe) <- c("Variable", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "Site")
write.csv(rotation_dataframe, file = './data/pca_rotations.csv')

# dataframe for contributions of variance
var_list <- vector("list", length = 20)
for (i in seq_along(site_list)) {
t <- summary(pca_list[[i]])
t2 <- data.frame(t$importance)
Site <- rep(site_list[i], times = 3)
t3 <- cbind(Site, t2)
var_list[[i]] <- t3
}

variance_dataframe <- rbind(var_list[[1]], var_list[[2]], var_list[[3]], var_list[[4]],
                            var_list[[5]], var_list[[6]], var_list[[7]], var_list[[8]], 
                            var_list[[9]], var_list[[10]], var_list[[11]], var_list[[12]],
                            var_list[[13]], var_list[[14]], var_list[[15]], var_list[[16]],
                            var_list[[17]], var_list[[18]], var_list[[19]], var_list[[20]])
write.csv(variance_dataframe, file = './data/pca_variance_df.csv')


# total pca
regions <- c("Ashley River", "Ashley River", "Ashley River", "Barrier Islands", "Charleston Harbor", 
             "Charleston Harbor", "Charleston Harbor", "Barrier Islands", "Barrier Islands", "FC1", "Wando River",
             "Wando River", "James Island Creeks", "James Island Creeks", "Shem Creek", "Shem Creek", "Shem Creek",
             "Barrier Islands", "James Island Creeks", "Wando River")
dim(new_matrix)
Regions <- rep(regions, times = 26)
new_matrix <- cbind(Regions, new_matrix)
pca_matrix <- new_matrix[,5:10]
pca_matrix$Tide <- as.numeric(pca_matrix$Tide)
pca_matrix$Chlorophyll <- as.numeric(pca_matrix$Chlorophyll)
pca_matrix$Phycocyanin <- as.numeric(pca_matrix$Phycocyanin)
pca_matrix$Temperature <- as.numeric(pca_matrix$Temperature)
pca_matrix$Nitrate <- as.numeric(pca_matrix$Nitrate)
pca_matrix$Phosphate <- as.numeric(pca_matrix$Phosphate)
pca_matrix <- na.omit(pca_matrix)
pca_all <- prcomp(pca_matrix, scale = T, center = T)

# pca plot
new_matrix_omit <- na.omit(new_matrix)
colnames(new_matrix_omit) <- c("Regions", "Site", "Date", "Tide_Class", "Tide", "Chlorophyll", 
                               "Phycocyanin", "Temperature", 
                               "Nitrate", "Phosphate")
pdf('./figures/pca_plot_all.pdf')
autoplot(pca_all, data = new_matrix_omit, colour = "Regions", size = 4) 
dev.off()

pdf('./figures/pca_plot_all_framed.pdf')
autoplot(pca_all, data = new_matrix_omit, colour = "Regions", size = 4, frame = T, frame.type = "norm") 
dev.off()

# total pca by impervious surface
load('./data/imperv_df.Rdata')

Imperviousness <- rep(imperv_df$Impervious_Index, times = 26)
new_matrix2 <- cbind(new_matrix, Imperviousness)
new_matrix_omit2 <- na.omit(new_matrix2)
colnames(new_matrix_omit2) <- c("Regions", "Site", "Date", "Tide_Class", "Tide", "Chlorophyll", "Phycocyanin", 
                                "Temperature", 
                               "Nitrate", "Phosphate", "Imperviousness")

pdf('./figures/pca_plot_impervious.pdf')
autoplot(pca_all, data = new_matrix_omit2, colour = "Imperviousness", size = 4) 
dev.off()

pdf('./figures/pca_plot_impervious_framed.pdf')
autoplot(pca_all, data = new_matrix_omit2, colour = "Imperviousness", size = 4, frame = T, frame.type = "norm") 
dev.off()

# pca all grouped by tide class
pdf('./figures/pca_plot_tide_class.pdf')
autoplot(pca_all, data = new_matrix_omit2, colour = "Tide_Class", size = 4) 
dev.off()

pdf('./figures/pca_plot_tide_class_framed.pdf')
autoplot(pca_all, data = new_matrix_omit2, colour = "Tide_Class", size = 4, frame = T, frame.type = "norm") 
dev.off()

# pca grouped by impervious surface, nutrients and pigments only
pca_nuts_mat <- cbind(new_matrix$Sample_ID, new_matrix$Tide_Class, Imperviousness, new_matrix$Chlorophyll, 
                      new_matrix$Phycocyanin, new_matrix$Nitrate,
                      new_matrix$Phosphate)
pca_nuts_mat <- data.frame(pca_nuts_mat)
colnames(pca_nuts_mat) <- c("Site", "Tide_Class", "Imperviousness", 
                            "Chlorophyll", "Phycocyanin", "Nitrate", "Phosphate")
pca_nuts_mat$Chlorophyll <- as.numeric(pca_nuts_mat$Chlorophyll)
pca_nuts_mat$Phycocyanin <- as.numeric(pca_nuts_mat$Phycocyanin)
pca_nuts_mat$Nitrate <- as.numeric(pca_nuts_mat$Nitrate)
pca_nuts_mat$Phosphate <- as.numeric(pca_nuts_mat$Phosphate)
pca_nuts_mat <- na.omit(pca_nuts_mat)
pca_nuts_only <- pca_nuts_mat[,4:7]
pca_nuts_all <- prcomp(pca_nuts_only, scale = T, center = T)

pdf('./figures/pca_plot_impervious_nuts.pdf')
autoplot(pca_nuts_all, data = pca_nuts_mat, colour = "Imperviousness", size = 4) 
dev.off()

pdf('./figures/pca_plot_impervious_nuts_framed.pdf')
autoplot(pca_nuts_all, data = pca_nuts_mat, colour = "Imperviousness", size = 4, frame = T, 
         frame.type = "norm") 
dev.off()

# pca nutrients only by tide class
pdf('./figures/pca_plot_tide_nuts.pdf')
autoplot(pca_nuts_all, data = pca_nuts_mat, colour = "Tide_Class", size = 4) 
dev.off()

pdf('./figures/pca_plot_tide_nuts_framed.pdf')
autoplot(pca_nuts_all, data = pca_nuts_mat, colour = "Tide_Class", size = 4, frame = T, 
         frame.type = "norm") 
dev.off()

# pca nutrients and pigments, 5 highest and five lowest urbanization only
hi_lo_urban <- cbind(new_matrix$Sample_ID, new_matrix$Chlorophyll, 
                      new_matrix$Phycocyanin, new_matrix$Nitrate,
                      new_matrix$Phosphate)
hi_lo_urban <- data.frame(hi_lo_urban)
colnames(hi_lo_urban) <- c("Site", "Chlorophyll", "Phycocyanin", "Nitrate", "Phosphate")
wanted_sites <- c("AR1", "AR2", "AR3", "CC1", "CH3", "CS1", "FC1", "JIC1", "SR1", "WR1")
needed_mat_list <- vector("list", length = length(wanted_sites))
for (i in seq_along(wanted_sites)) {
    t <- hi_lo_urban %>% filter(Site == wanted_sites[i])
    needed_mat_list[[i]] <- t
}

needed_mat <- rbind(needed_mat_list[[1]], needed_mat_list[[2]], needed_mat_list[[3]], needed_mat_list[[4]],
                    needed_mat_list[[5]], needed_mat_list[[6]], needed_mat_list[[7]], needed_mat_list[[8]],
                    needed_mat_list[[9]], needed_mat_list[[10]])
head(needed_mat)
impervious <- c(rep("High", times = 52), rep("Low", times = 52), rep("High", times = 26), rep("Low", times = 26),
                rep("High", times = 26), rep("Low", times = 52), rep("High", times = 26))
needed_mat2 <- cbind(needed_mat, impervious)
colnames(needed_mat2) <- c("Site", "Chlorophyll", "Phycocyanin", "Nitrate", "Phosphate", "Imperviousness")
needed_mat3 <- na.omit(needed_mat2)
needed_mat_pca <- needed_mat3[,2:5]
needed_mat_pca$Chlorophyll <- as.numeric(needed_mat_pca$Chlorophyll)
needed_mat_pca$Phycocyanin <- as.numeric(needed_mat_pca$Phycocyanin)
needed_mat_pca$Nitrate <- as.numeric(needed_mat_pca$Nitrate)
needed_mat_pca$Phosphate <- as.numeric(needed_mat_pca$Phosphate)
hi_lo_pca <- prcomp(needed_mat_pca, scale = T, center = T)

pdf('./figures/pca_plot_highest_and_lowest_impervious_nuts.pdf')
autoplot(hi_lo_pca, data = needed_mat3, colour = "Imperviousness", size = 4) 
dev.off()

pdf('./figures/pca_plot_highest_and_lowest_impervious_nuts_framed.pdf')
autoplot(hi_lo_pca, data = needed_mat3, colour = "Imperviousness", size = 4, frame = T, 
         frame.type = "norm") 
dev.off()

# investigating 
high_urban <- needed_mat3 %>% filter(Imperviousness == "High")
low_urban <- needed_mat3 %>% filter(Imperviousness == "Low")

plot(high_urban$Nitrate, high_urban$Chlorophyll)
plot(high_urban$Phosphate, high_urban$Chlorophyll)
plot(high_urban$Phosphate, high_urban$Phycocyanin)
plot(high_urban$Nitrate, high_urban$Phycocyanin)
