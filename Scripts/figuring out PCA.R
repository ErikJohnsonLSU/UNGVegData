#### PCA personal attempt ----

install.packages("readxl")   
library(readxl)
library(tidyverse)
df <- read_excel("X2026_BAKA_MAP")

library(dplyr)
library(ggplot2)




#three hashtags as a main heading
# 4 hashtags is a subheading


X2026_LDWF_ASF_WMA_MAPS_Excel_Data_Entry_Template_2025 |>
ggplot(pca_df, aes(PC1 = Weight, PC2 = WNG)) +
  geom_point() +
  labs(title = "PCA: Weight and Wing Length",
       x = "PC1",
       y = "PC2")
df
names(df)

df <- read_excel("C:/Users/Alex/Documents/X2026 BAKA MAP.xlsx")

library(readxl)
library(tidyverse)


df <- read_excel("X2026 BAKA MAP")
pca_data <- df %>% select(WEIGHT, WNG)
pca_model <- prcomp(pca_data, scale. = TRUE)
pca_df <- as.data.frame(pca_model$x)
ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(
    title = "PCA: Weight and Wing Length",
    x = "PC1",
    y = "PC2"
  )


####PCA website ----

# Import data
install.packages("readxl")   
library(readxl)
library(tidyverse)
df <- read_excel("X2026_LDWF_ASF_WMA_MAPS_Excel_Data_Entry_Template_2025.xlsx")

# PCA using base function - prcomp()
p <- prcomp(ma.pollen1, scale=TRUE)

# Summary
s <- summary(p)

# Screeplot
layout(matrix(1:2, ncol=2))
screeplot(p)
screeplot(p, type="lines")
biplot(p)