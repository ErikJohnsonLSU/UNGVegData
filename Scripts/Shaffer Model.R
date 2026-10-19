setwd("~/Research/Urban Native Greens/Data/UNGVegData/") # For Erik
setwd("C:/Users/alexb/Desktop/R Projects/UNG/Urban_Native_Greens") #For Alex
library(RODBC)

# To connect to Access driver, it seems the full directory path is needed

# Only Erik
con <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
   DBQ=C:/Users/ErikJohnson/OneDrive - LSU AgCenter/Documents/Research/Urban Native Greens/Data/UNGVegData/Data/UNG Veg Surveys.accdb"
)
#For A-Dawg
con <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
   DBQ=C:/Users/alexb/LSU AgCenter/Johnson, Erik I. - Urban Native Greens/Data/UNGVegData/Data/UNG Veg Surveys.accdb"
)


box <- sqlFetch(con, "Survey-Box") #read table from Access database file
herb <- sqlFetch(con, "Survey-Herbaceous") 
sapshrub <- sqlFetch(con, "Survey-SaplingShrub")
canopy <- sqlFetch(con, "Survey-Subplot")
tree <- sqlFetch(con, "Survey-Tree")

odbcCloseAll() # disconnect from Access driver

library(dplyr)
library(tidyr)

###### Transform pct cover data to wide format --------------------------------------

invasive <- subset(data, Invasive == 1)
native <- subset(data, Invasive == 0)


herb_wide <- herb %>%
  select(SubplotSurveyID, SpeciesID, PctCover) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = PctCover,
    values_fill = list(PctCover = 0)
  )

herb_long <- herb_wide %>%
  pivot_longer(
    cols = -SubplotSurveyID,
    names_to = "SpeciesID",
    values_to = "PctCover"
  )

box2 <- box %>%
  left_join(canopy, by = "SurveyID")

box3 <- box2 %>%
  select(BoxID, SubplotSurveyID, Canopy_cover)

herb2 <- herb_long %>%
  right_join(box3, by = "SubplotSurveyID")

# We still filter out the subplots that were surveyed a 2nd time

#######IV------------------------------------------------------------------------

# Step 1 - Calculate the frequency of occurrence and average cover in each plot

freq <- herb2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    herb_cover_sum = sum(PctCover > 0, na.rm = TRUE),
    herb_cover_mean = mean(PctCover, na.rm = TRUE)
  )


# Step 2 - Relative frequency for each species / sum of frequencies among species

total_freq <- freq %>%
  group_by(BoxID) %>%
  summarise(
    herb_freq_total = sum(herb_cover_sum),
    mean_cover_total = sum(herb_cover_mean)
  )

# Step 3 - Relative cover for each species / sum of covers among species
rel_cover <- freq %>%
  left_join(total_freq, by = "BoxID") %>%
  mutate(
    rel_cover = (herb_cover_mean / mean_cover_total) * 100
  )

# Step 4 - IV = sum of relative values for each species within each plot
herb_IV <- freq %>%
  left_join(total_freq, by = "BoxID") %>%
  mutate(
    rel_freq = (herb_cover_sum / herb_freq_total) * 100,
    rel_cover = (herb_cover_mean / mean_cover_total) * 100,
    IV = (rel_freq + rel_cover) / 2
  )
#IV out of 100? to make easier to comprehend?
#Will have to divide by 3 for other shrubs species but ration should stay the same

#Shrubs IV

shrub_metrics <- shrub2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    shrub_freq = sum(PctCover > 0, na.rm = TRUE),   # frequency
    shrub_density = n(),                        # density (# individuals)
    shrub_cover_mean = mean(PctCover, na.rm = TRUE),
    .groups = "drop"
  )

# Step 2 - Calculate totals within each box

shrub_totals <- shrub_metrics %>%
  group_by(BoxID) %>%
  summarise(
    shrub_freq_total = sum(shrub_freq),
    shrub_density_total = sum(shrub_density),
    shrub_cover_total = sum(shrub_cover_mean),
    .groups = "drop"
  )

# Step 3 - Calculate relative frequency, density, and cover

shrub_IV <- shrub_metrics %>%
  left_join(shrub_totals, by = "BoxID") %>%
  mutate(
    rel_freq = (shrub_freq / shrub_freq_total) * 100,
    rel_density = (shrub_density / shrub_density_total) * 100,
    rel_cover = (shrub_cover_mean / shrub_cover_total) * 100,
    
    # Importance Value
    IV = (rel_freq + rel_density + rel_cover) / 3
  )
# Tree Values

tree_metrics <- tree2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    tree_freq = n_distinct(SubplotSurveyID),
    tree_density = n(),
    basal_area = sum(pi * (DBH / 2)^2, na.rm = TRUE),
    .groups = "drop"
  )

tree_IV <- tree_metrics %>%
  left_join(tree_totals, by = "BoxID") %>%
  mutate(
    rel_freq = (tree_freq / tree_freq_total) * 100,
    rel_density = (tree_density / tree_density_total) * 100,
    rel_dominance = (basal_area / basal_area_total) * 100,
    IV = (rel_freq + rel_density + rel_dominance) / 3
  )

#####Basal Area------------------------------------------------------------------

library(dplyr)

DBh_cm <- sqlFetch(con, "DBH_cm")
trees <- trees %>%
  mutate(
    BA_tree = 0.00007854 * DBH_cm ^2
  )

subplot_ba <- trees %>%
  group_by(PlotID, SubplotID) %>%
  summarize(
    BA_subplot = sum(BA_tree, na.rm = TRUE),
    .groups = "drop"
  )

subplot_area <- 0.00785

plot_ba <- subplot_ba %>%
  mutate(
    BA_ha = BA_subplot / subplot_area
  ) %>%
  group_by(PlotID) %>%
  summarize(
    Mean_BA_ha = mean(BA_ha, na.rm = TRUE),
    .groups = "drop"
  )
#####Shaffer Framework -------------------------------------------------
#no canopy cover in yet
library(lme4)

#All objects
# nest_intervals is a place holder for nest data and "input here" is the same
shaffer.all <- glm(
  Fate ~ tree_IV +
    shrub_IV +
    herb_IV +
    plot_ba,
  family = binomial,
  weights = ExposureDays,
  data = "input here"
)

#Shaffer BA and tree
shaffer.batree <- glm(
  Fate ~ tree_IV +
    plot_ba,
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
)

#Shaffer tree
shaffer.tree <- glm(
  Fate ~ tree_IV, 
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
)

#Shaffer BA
shaffer.ba <- glm(
  Fate ~ plot_ba,
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
)

#Shaffer herb + shrub
shaffer.herbshrub <- glm(
  Fate ~ shrub_IV + herb_IV,
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
)

#Shaffer Herb
shaffer.herb <- glm(
  Fate ~ herb_IV,
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
)

#shaffer herb and tree
shaffer.herbtree <- glm(
  Fate ~ tree_IV +
   herb_IV,
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
 )

#Shaffer Tree and Shrub
shaffer.treeshrub <- glm(
  Fate ~ tree_IV +
    shrub_IV,
  family = binomial,
  weights = ExposureDays,
  data = nest_intervals
)

#Shaffer Ba and herb
shaffer.baherb <- glm(
  Fate ~ herb_IV +
    plot_ba,
  family = binomial,
  weights = ExposureDays,
  data = "input"
)  
#Shaffer shrubba
shaffer.shrubba <- glm(
  Fate ~ shrub_IV +
    plot_ba,
  family = binomial,
  weights = ExposureDays,
  data = "input here"
)
nest_intervals$DSR <- predict(
  shaffer.mod,
  type = "response"
)

nest_intervals$NestSuccess <-
  nest_intervals$DSR^25

AIC(shaffer.all, shaffer.baandtree, shaffer.tree, shaffer.ba, shaffer.herbshrub, shaffer.herb, shaffer.herbtree, shaffer.treeshrub, shaffer.baherb, shaffer.shrubba)

library(MASS)

#nest_success_data is a placeholder
brood <- glm.nb(
  Fledglings ~ tree_IV +
    shrub_IV +
    herb_IV +
    plot_ba,
  data = "nest_success_data"[nest_data$Fledglings > 0,]
)

nest_success_data$ExpectedFecundity <-
  nest_success_data$NestSuccess *
  predict(brood,
          type = "response")





