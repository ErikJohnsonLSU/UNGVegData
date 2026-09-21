install.packages(“ellipse”)

library(ellipse)

citation("ellipse")



setwd("C:/Users/ErikJohnson/OneDrive - LSU AgCenter/Documents/Research/Marshbirds - Diversions/-.Data Entry")



Veg <- read.xlsx("DataEntry_DiversionVegetation.xlsx", sheetName = "Sheet1", header = TRUE)



# Remove unwanted variables and rare plant species

Plants <- subset(Veg, select = -c(Year, Date, Time, Observer, Low.Marsh, High.Marsh, Marsh.Terrestrial.Border, Invasives, Pannes..Pools..Creeks, Open.Water, Upland, Wrack, Dead.Snags..count., UNKgrass3, UNKgrass4, Unk_grass_photo, Unk_emerging_grass, Unk_dead_grass, Unk_Grass_maidencane, Unk_Tall_Grass, Unk_Grass_DSC_7061, Unk_Tall_Grass_notsampled, Tall_Grass_DSC_7072, Unk_Red_Woody_DSC_7082, JELA22_unk2, JELA22_unk1, JELA22_unk6, JELA22_unk5, Unk_Low, Unk_Shrub, DAPO22_unk1, DAPO22_unk2, DAPO22_unk3, DAPO22_unk4))

Plants <- subset(Plants, select = -c(Andropogon, Smartweed, Succulent_DSC_7060, RedMaple, Spartina_patens, Tallow, Iris, Arrowhead_Sagittarias, Frogfruit, Lilypad, ElephantEar, Vigna, Carex, Lotus))

Plants[is.na(Plants)] <- 0



which(colnames(Plants)=="Forbs")



new <- aggregate(Plants[, 5:31], list(Plants$Unique), mean)

new2 <- subset(new, select = -c(Group.1))

new2 <- new2[-1, ] # remove row names



# PCA

Plants_PCA <- prcomp(new2, scale. = TRUE)

Plants_PCA$rotation[ ,1:27] #loadings

Plants_PCA$x

autoplot(Plants_PCA, data = new2)

screeplot(Plants_PCA, type = "lines")

biplot(Plants_PCA)

s <- summary(Plants_PCA)

s



Plants_PCA$pch.group <- c(rep("Big Mar", times=8), rep("Davis Pond", times=16), rep("Jean Lafitte", times=24))





# Calculate total contribution (e.g., sum of contributions to PC1 and PC2)

var_contrib <- get_pca_var(Plants_PCA)$contrib

total_contrib <- rowSums(var_contrib[, 1:2])

top_n <- 10  # Change this number as needed

top_vars <- names(sort(total_contrib, decreasing = TRUE))[1:top_n]

top_vars





PCA <- fviz_pca_biplot(Plants_PCA,
                       
                       label = "var",
                       
                       mean.point = FALSE,
                       
                       habillage = Plants_PCA$pch.group,  # Color individuals by group
                       
                       addEllipses = TRUE,
                       
                       select.var = list(name = top_vars),
                       
                       palette = c("darkgray", "gray", "black"),
                       
                       col.var = "black",
                       
                       repel = TRUE
                       
) + ggtitle(NULL)

PCA



ggsave("PCA.png", plot = PCA, width = 10, height = 6, dpi = 300)



# PERMANOVA

new2$Site <- c(rep("Big Mar", times=8), rep("Davis Pond", times=16), rep("Jean Lafitte", times=24))

new2$Diversion[new2$Site == "Big Mar"] <- "Yes"

new2$Diversion[new2$Site == "Davis Pond"] <- "Yes"

new2$Diversion[new2$Site == "Jean Lafitte"] <- "No"