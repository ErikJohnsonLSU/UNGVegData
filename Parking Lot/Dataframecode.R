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

###### Transform pct cover data to wide format -------------------------------------------------------------------------------------


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

# We still need filter out the subplots that were surveyed a 2nd time

#######IV--------------------------------------------------------------------------------------------------------------

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

shrub_metrics <- sapshrub %>%
  left_join(box3, by = "SubplotSurveyID") %>%
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
tree2 <- tree %>%
  left_join(box3, by = "SubplotSurveyID") %>%
  mutate(
    DBH_m = DBH_cm * 0.0254,  # convert inches to meters
    BA_m2 = pi * (DBH_m / 2)^2
  )
tree_metrics <- tree2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    tree_freq = n_distinct(SubplotSurveyID),
    tree_density = n(),
    tree_ba = sum(BA_m2, na.rm = TRUE),
    .groups = "drop"
  )
                                          
tree_totals <- tree_metrics %>%
  group_by(BoxID) %>%
  summarise(
    tree_freq_total = sum(tree_freq),
    tree_density_total = sum(tree_density),
    tree_ba_total = sum(tree_ba),
    .groups = "drop"
  )

tree_IV <- tree_metrics %>%
  left_join(tree_totals, by = "BoxID") %>%
  mutate(
    rel_freq_tree = 100 * tree_freq / tree_freq_total,
    rel_density_tree = 100 * tree_density / tree_density_total,
    rel_dominance_tree = 100 * tree_ba / tree_ba_total,
    IV = (rel_freq_tree + rel_density_tree + rel_dominance_tree) / 3
  )



tree2 <- tree %>%
  left_join(box3, by = "SubplotSurveyID")

tree_metrics <- tree2 %>%
  group_by(BoxID, SpeciesID) %>%
  summarise(
    tree_freq = n_distinct(SubplotSurveyID[DBH_cm > 0]),
    tree_density = n(),
    tree_dbh = sum(DBH_cm, na.rm = TRUE),
    .groups = "drop"
  )

tree_IV %>%
  arrange(BoxID, desc(IV))

#####Basal Area-------------------------------------------------------------------------------------------

tree2 <- tree %>%
  right_join(box3, by = "SubplotSurveyID") %>%
  mutate(
    DBH_cm_forreal = DBH_cm * 2.54,
    BA_m2 = pi * (DBH_cm_forreal / 200)^2
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
  left_join(plot_canopy_cover, by = "BoxID")

######Data Frame---------------------------------------------------------------------------------------------------------------
# Clean up and isolate the values for everything
# Clean and widen IV

top10_tree <- tree_IV %>%
  group_by(BoxID) %>%
  arrange(desc(IV), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  select(BoxID, rank, SpeciesID, IV) %>%
  pivot_wider(
    names_from = rank,
    values_from = c(SpeciesID, IV),
    names_glue = "Tree_{rank}_{.value}"
  )
top10_shrub <- shrub_IV %>%
  group_by(BoxID) %>%
  arrange(desc(IV_shrub), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  select(BoxID, rank, SpeciesID, IV_shrub) %>%
  pivot_wider(
    names_from = rank,
    values_from = c(SpeciesID, IV_shrub),
    names_glue = "Shrub_{rank}_{.value}"
  )
top10_herb <- herb_IV %>%
  group_by(BoxID) %>%
  arrange(desc(IV), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 10) %>%
  select(BoxID, rank, SpeciesID, IV) %>%
  pivot_wider(
    names_from = rank,
    values_from = c(SpeciesID, IV),
    names_glue = "Herb_{rank}_{.value}"
  )

avg_tree_ba <- tree %>%
  left_join(box3 %>% select(SubplotSurveyID, BoxID),
    by = "SubplotSurveyID"
  ) %>%
  mutate(
    DBH_cm_forreal = DBH_cm * 2.54,
    BA_m2 = pi * (DBH_cm_forreal / 200)^2 / 4
  ) %>%
  group_by(BoxID) %>%
  summarise(
    n_trees = n(),
    avg_BA_m2_pertree = mean(BA_m2, na.rm = TRUE)
  ) %>%
  ungroup()


shrub_cover <- sapshrub %>%
  left_join(box3 %>% select(BoxID, SubplotSurveyID),
            by = "SubplotSurveyID") %>%
  group_by(BoxID, SubplotSurveyID) %>%
  summarise(
    subplot_cover = sum(PctCover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(BoxID) %>%
  summarise(
    shrub_cover = mean(subplot_cover, na.rm = TRUE),
    .groups = "drop"
  )

herb_cover <- herb %>%
  left_join(box3 %>% select(BoxID, SubplotSurveyID),
            by = "SubplotSurveyID") %>%
  group_by(BoxID, SubplotSurveyID) %>%
  summarise(
    subplot_cover = sum(PctCover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(BoxID) %>%
  summarise(
    herb_cover = mean(subplot_cover, na.rm = TRUE),
    .groups = "drop"
  )

herb_invasive_cover <- herb %>%
  left_join(
    box3 %>% select(BoxID, SubplotSurveyID),
    by = "SubplotSurveyID"
  ) %>%
  filter(Invasive == 1) %>%
  group_by(BoxID, SubplotSurveyID) %>%
  summarise(
    subplot_invasive_cover = sum(PctCover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(BoxID) %>%
  summarise(
    herb_invasive_cover = mean(subplot_invasive_cover, na.rm = TRUE),
    .groups = "drop"
  )

shrub_invasive_cover <- sapshrub %>%
  left_join(
    box3 %>% select(BoxID, SubplotSurveyID),
    by = "SubplotSurveyID"
  ) %>%
  filter(Invasive == 1) %>%
  group_by(BoxID, SubplotSurveyID) %>%
  summarise(
    subplot_invasive_cover = sum(PctCover, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(BoxID) %>%
  summarise(
    shrub_invasive_cover = mean(subplot_invasive_cover, na.rm = TRUE),
    .groups = "drop"
  )

veg_metrics <- plot_canopy_ba_metrics %>%
  left_join(top10_tree, by = "BoxID") %>%
  left_join(top10_shrub, by = "BoxID") %>%
  left_join(top10_herb, by = "BoxID") %>%
  left_join(shrub_cover, by = "BoxID") %>%
  left_join(herb_cover, by = "BoxID") %>%
  left_join(herb_invasive_cover, by = "BoxID") %>%
  left_join(shrub_invasive_cover, by = "BoxID") %>%
  left_join(avg_tree_ba, by = "BoxID")
  


NestFates_2026 <- NestFates_2026 %>%
  rename(BoxID = "Station Name")

#to sort data that doesn't have data in the other 
anti_join(NestFates_2026, veg_metrics, by = "BoxID")#missing from veg
anti_join(veg_metrics, NestFates_2026, by = "BoxID")#missing from nest data

shaffer_data_filtered <- NestFates_2026 %>%
  inner_join(veg_metrics, by = "BoxID")



nrow(NestFates_2026)
nrow(shaffer_data_filtered)
#HELL YAH! Banana Bread!








