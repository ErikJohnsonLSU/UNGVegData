# Use this file to build a PCA from the Urban Native Greens vegetation data


library(gitcreds)
gitcreds_set()
library(usethis)

usethis::use_git()      # initializes Git + first commit
usethis::use_git_ignore()  # adds .Rhistory, .RData, etc.

######FAKE DATA PCA ----------------------------------------------------------------
install.packages("RODBC")
library(RODBC)

x <- 1:5
y <- LETTERS [1:5]
data.frame(x,y)

fake <- data.frame(
  Col1 = c(12,4,5,19,205),
  Col2 = c(1,2,412,66,34),
  Col3 = c(16,15,22,56,535),
  Col4 = c(1,43,55,1,10),
  Col5 = c(4,65,56,22,5)
)

summary(fake)


install.packages("factoextra")
library(factoextra)

Run PCA

pca <- prcomp(fake, center = TRUE, scale. = TRUE)
pca
summary(pca)

# PCA biplot
fviz_pca_biplot(
  pca,
  repel = TRUE,
  col.var = "red",
  col.ind = "blue"
)

# Scree plot
fviz_eig(pca)



#first two PCs explain 85 percent of the variance
#Col1 and Col3 increase together.
#Col5 varies in the opposite direction.
#PC1 represents a gradient between those variables.
#Which variables are related.
#Which variables are driving patterns.
#Whether observations form distinct groups.
#How much of the total variation can be summarized by a few components.
#The underlying gradients or trends in the data.


####UNG PCA PLAYING AROUND -----'

z <- 1:5
w <- LETTERS [1:5]
data.frame(z,w)

fake2 <-data.frame(
  Col1 = c(1,2,3,4,5),
  Col2 = c(1,2,3,6,7),
  Col3 = c(8,10,65,56,535),
  Col4 = c(5,43,55,190,5),
  Col5 = c(4,65,5,5,5)
)

pca <- prcomp(fake2, center = TRUE, scale. = TRUE)
pca
summary(pca)

# PCA biplot
fviz_pca_biplot(
  pca,
  repel = TRUE,
  col.var = "red",
  col.ind = "blue"
)
