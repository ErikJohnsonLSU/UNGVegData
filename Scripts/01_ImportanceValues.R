# Importance values

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
   DBQ=C:/Users/alexb/LSU AgCenter/Johnson, Erik I. - Urban Native Greens/Data/UNG Veg Surveys.accdb")

box <- sqlFetch(con, "Survey-Box") #read table from Access database file
herb <- sqlFetch(con, "Survey-Herbaceous") 
sapshrub <- sqlFetch(con, "Survey-SaplingShrub")
canopy <- sqlFetch(con, "Survey-Subplot")
tree <- sqlFetch(con, "Survey-Tree")

odbcCloseAll() # disconnect from Access driver

library(dplyr)
library(tidyr)

# Transform pct cover data to wide format --------------------------------------

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
rel_metrics <- freq %>%
  left_join(total_freq, by = "BoxID") %>%
  mutate(
    rel_freq = (herb_cover_sum / herb_freq_total) * 100,
    rel_cover = (herb_cover_mean / mean_cover_total) * 100,
    IV = (rel_freq + rel_cover) / 2
  )
#IV out of 100? to make easier to comprehend?
#Will have to divide by 3 for other shrubs species but ration should stay the same


