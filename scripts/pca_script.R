# PCA analysis
library(dplyr)
library(ggplot2)
library(ggfortify)

load('./data/wk_master.Rdata')

# create matrix of wanted variables only
new_matrix <- cbind(wk_master$Sample_ID, wk_master$Tidal_height, wk_master$Chlorophyll_conc,
                    wk_master$Phycocyanin_conc, wk_master$Temp, wk_master$Nitrate, wk_master$Phosphate)
View(new_matrix)
class(new_matrix)
new_matrix <- data.frame(new_matrix)
colnames(new_matrix) <- c("Sample_ID", "Tide", "Chlorophyll", "Phycocyanin",
                          "Temperature", "Nitrate", "Phosphate")

site_list <- unique(new_matrix$Sample_ID)
pca_list <- vector("list", length = 20)

# pca by site
for (i in seq_along(site_list)) {
site_df <- new_matrix %>% filter(Sample_ID == site_list[i])
site_mat <- site_df[, 2:7]
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
pca_matrix <- new_matrix[,3:8]
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
colnames(new_matrix_omit) <- c("Regions", "Site", "Tide", "Chlorophyll", "Phycocyanin", "Temperature", 
                               "Nitrate", "Phosphate")
pdf('./figures/pca_plot_all.pdf')
autoplot(pca_all, data = new_matrix_omit, colour = "Regions", size = 4) 
dev.off()

pdf('./figures/pca_plot_all_framed.pdf')
autoplot(pca_all, data = new_matrix_omit, colour = "Regions", size = 4, frame = T, frame.type = "norm") 
dev.off()


