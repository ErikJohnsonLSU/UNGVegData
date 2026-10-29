# Use this file to build a PCA from the Urban Native Greens vegetation data

library(dplyr)

# need to still subtract invasive herb (and shrub) from total herb (and shrub) to get native herb (and shrub)

box_veg <- shaffer_data_filtered %>%
  distinct(BoxID, .keep_all = TRUE)
  
box_pca <- box_veg %>%
  select(avg_BA_m2, avg_canopy_cover, shrub_cover, herb_cover, herb_invasive_cover, shrub_invasive_cover, n_trees, avg_BA_m2_pertree)

box_pca[is.na(box_pca)] <- 0

pca <- prcomp(box_pca, scale. = TRUE)
pca$rotation[ ,1:8] #loadings
pca$x
autoplot(pca, data = new2)
screeplot(pca, type = "lines")
biplot(pca)
s <- summary(pca)
s

# Placeholder
box_veg$M_band2 <- paste(box_veg$M_Band, "_tree")
