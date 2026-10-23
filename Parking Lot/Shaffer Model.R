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

library(readxl)
NestFates_2026 <- read_excel("C:/Users/alexb/Desktop/R Projects/UNG/Urban_Native_Greens/UNGVegData/Data/NestFates_2026.xlsx")
View(NestFates_2026)

box <- sqlFetch(con, "Survey-Box") #read table from Access database file
herb <- sqlFetch(con, "Survey-Herbaceous") 
sapshrub <- sqlFetch(con, "Survey-SaplingShrub")
canopy <- sqlFetch(con, "Survey-Subplot")
tree <- sqlFetch(con, "Survey-Tree")
invasive <- sqlFetch(con, "Invasive")
odbcCloseAll() # disconnect from Access driver

library(dplyr)
library(tidyr)

###### Transform pct cover data to wide format --------------------------------------


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

freq_herb <- herb2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    herb_cover_sum = sum(PctCover > 0, na.rm = TRUE),
    herb_cover_mean = mean(PctCover, na.rm = TRUE)
  )


# Step 2 - Relative frequency for each species / sum of frequencies among species

total_freq_herb <- freq_herb %>%
  group_by(BoxID) %>%
  summarise(
    herb_freq_total = sum(herb_cover_sum),
    mean_cover_total = sum(herb_cover_mean)
  )

# Step 3 - Relative cover for each species / sum of covers among species
rel_cover_herb <- freq_herb %>%
  left_join(total_freq_herb, by = "BoxID") %>%
  mutate(
    rel_cover_herb = (herb_cover_mean / mean_cover_total) * 100
  )

# Step 4 - IV = sum of relative values for each species within each plot
herb_IV <- freq_herb %>%
  left_join(total_freq_herb, by = "BoxID") %>%
  mutate(
    rel_freq_herb = (herb_cover_sum / herb_freq_total) * 100,
    rel_cover_herb = (herb_cover_mean / mean_cover_total) * 100,
    IV = (rel_freq_herb + rel_cover_herb) / 2
  )
#IV out of 100? to make easier to comprehend?
#Will have to divide by 3 for other shrubs species but ration should stay the same

#Shrubs IV
shrub_wide <- sapshrub %>%
  select(SubplotSurveyID, SpeciesID, PctCover, StemCount) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = c(PctCover, StemCount),
    values_fill = list(PctCover = 0, StemCount = 0)
  )
#something is wrong here
shrub_long <- shrub_wide %>%
  pivot_longer(
    cols = -SubplotSurveyID,
    names_to = c(".value", "SpeciesID"),
    names_sep = "="
  )

shrub2 <- shrub_long %>%
  right_join(box3, by = "SubplotSurveyID")


shrub_metrics <- shrub2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    shrub_freq = sum(PctCover > 0, na.rm = TRUE),
    shrub_density = sum(StemCount, na.rm = TRUE),
    shrub_cover_mean = mean(PctCover, na.rm = TRUE),
    .groups = "drop"
  )
shrub_totals <- shrub_metrics %>%
  group_by(BoxID) %>%
  summarise(
    shrub_freq_total = sum(shrub_freq),
    shrub_density_total = sum(shrub_density),
    shrub_cover_total = sum(shrub_cover_mean),
    .groups = "drop"
  )
shrub_IV <- shrub_metrics %>%
  left_join(shrub_totals, by = "BoxID") %>%
  mutate(
    rel_freq_shrub = 100 * shrub_freq / shrub_freq_total,
    rel_density_shrub = 100 * shrub_density / shrub_density_total,
    rel_cover_shrub = 100 * shrub_cover_mean / shrub_cover_total,
    IV_shrub = (rel_freq_shrub + rel_density_shrub + rel_cover_shrub) / 3
  )

# Tree Values


tree_wide <- tree %>%
  group_by(SubplotSurveyID, SpeciesID) %>%
  summarise(
    DBH_cm = sum(DBH_cm, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = DBH_cm,
    values_fill = 0
  )

tree_long <- tree_wide %>%
  pivot_longer(
    cols = -SubplotSurveyID,
    names_to = "SpeciesID",
    values_to = "DBH_cm"
  )

tree2 <- tree_long %>%
  right_join(box3, by = "SubplotSurveyID")

tree_metrics <- tree2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    tree_freq = n_distinct(SubplotSurveyID),
    tree_density = n(),
    tree_dbh = sum(DBH_cm, na.rm = TRUE),
    .groups = "drop"
  )

tree_totals <- tree_metrics %>%
  group_by(BoxID) %>%
  summarise(
    tree_freq_total = sum(tree_freq),
    tree_density_total = sum(tree_density),
    tree_dbh_total = sum(tree_dbh),
    .groups = "drop"
  )

tree_IV <- tree_metrics %>%
  left_join(tree_totals, by = "BoxID") %>%
  mutate(
    rel_freq_tree = (tree_freq / tree_freq_total) * 100,
    rel_density_tree = (tree_density / tree_density_total) * 100,
    rel_dbh_tree = (tree_dbh / tree_dbh_total) * 100,
    
    IV = (rel_freq_tree + rel_density_tree + rel_dbh_tree) / 3
  )
tree_IV %>%
  arrange(BoxID, desc(IV))

#####Basal Area------------------------------------------------------------------



tree2 <- tree %>%
  right_join(box3, by = "SubplotSurveyID") %>%
  mutate(
    DBH_cm_forreal = DBH_cm * 2.54,
    BA_m2 = pi * (DBH_cm / 200)^2
  )

subplot_BA <- tree2 %>%
  group_by(BoxID, SubplotSurveyID) %>%
  summarise(
    BA_subplot = sum(BA_m2, na.rm = TRUE),
    .groups = "drop"
  )
plot_BA <- subplot_BA %>%
  group_by(BoxID) %>%
  summarise(
    avg_BA_m2 = mean(BA_subplot, na.rm = TRUE),
    .groups = "drop"
  )

#####canopy -------------
plot_canopy_cover <- box3 %>%
  group_by(BoxID) %>%
  summarise(
    avg_canopy_cover = mean(Canopy_cover, na.rm = TRUE),
    .groups = "drop"
  )

plot_canopy_ba_metrics <- plot_BA %>%
  left_join(plot_cover, by = "BoxID")




######Data Frame--------------------------------------------------------------------------------------
# Clean up and isolate the values for everything
# Clean and widen IV


#Major road block
herb_wide_final <- herb_IV %>%
  ungroup() %>%                          
  dplyr::select(BoxID, SpeciesID, PctCover) %>% 
  pivot_wider(
    names_from = SpeciesID,
    values_from = IV,
    names_prefix = "IV_herb_",
    values_fill = 0
  )


shrub_wide_final <- shrub_IV %>%
  ungroup() %>%
  dplyr::select(BoxID, SpeciesID, ??????????? ) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = IV_shrub,
    names_prefix = "IV_shrub_",
    values_fill = 0
  )

# Tree IV Fix
tree_wide_final <- tree_IV %>%
  ungroup() %>%
  dplyr::select(BoxID, SpeciesID,???????????) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = IV,
    names_prefix = "IV_tree_",
    values_fill = 0
  )

#combines and isolates BA and CC
plot_physical_metrics <- plot_metrics %>%
  select(BoxID, avg_BA_m2, avg_canopy_cover)

#Joins everything
The_One_Data_Frame_To_Rule_Them_All <- NestFates_2026 %>%
  left_join(plot_physical_metrics, by = "BoxID") %>%
  left_join(herb_wide_final, by = "BoxID") %>%
  left_join(shrub_wide_final, by = "BoxID") %>%
  left_join(tree_wide_final, by = "BoxID")

# Check the results
glimpse(master_df)

#####Shaffer Framework -------------------------------------------------------------
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
  data = "NestFates_2026"[nest_data$Fledglings > 0,]
)

nest_success_data$ExpectedFecundity <-
  nest_success_data$NestSuccess *
  predict(brood,
          type = "response")

