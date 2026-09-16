setwd("~/Research/Urban Native Greens/Data/UNGVegData/")

rm(list = ls()) # clear the Environment

# new script

# Load the data ----------------------------------------------------------------

library(RODBC)

# To connect to Access driver, it seems the full directory path is needed

con <- odbcDriverConnect(
  "Driver={Microsoft Access Driver (*.mdb, *.accdb)};
   DBQ=C:/Users/ErikJohnson/OneDrive - LSU AgCenter/Documents/Research/Urban Native Greens/Data/UNGVegData/Data/UNGVegSurveys.accdb"
)

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
    values_fill = 0
  )

# Diagnostic to look for duplicate species in a subplot
herb %>%
  count(SubplotSurveyID, SpeciesID) %>%
  filter(n > 1)


sapshrub_wide <- sapshrub %>%
  select(SubplotSurveyID, SpeciesID, PctCover) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = PctCover,
    values_fill = 0
  )

# Diagnostic to look for duplicate species in a subplot
sapshrub %>%
  count(SubplotSurveyID, SpeciesID) %>%
  filter(n > 1)

# Summarize tree data to wide format -------------------------------------------

tree$SpeciesID <- paste0(tree$SpeciesID, "_tree")

tree_summary <- tree %>%
  filter(Status == "Live") %>%
  group_by(SubplotSurveyID, SpeciesID) %>%
  summarise(
    trees_n = n(),
    trees_dbh_sum = sum(DBH_cm, na.rm = TRUE),
    trees_dbh_mean = mean(DBH_cm, na.rm = TRUE),
    trees_dbh_sd = sd(DBH_cm, na.rm = TRUE),
    .groups = "drop"
  )

tree_wide <- tree_summary %>%
  select(SubplotSurveyID, SpeciesID, trees_n, trees_dbh_sum, trees_dbh_mean, trees_dbh_sd) %>%
  pivot_wider(
    names_from = SpeciesID,
    values_from = c(trees_n, trees_dbh_sum, trees_dbh_mean, trees_dbh_sd),
    values_fill = 0
  )

# Join the data tables together into a single data frame -----------------------

a <- box %>%
  left_join(canopy, by = "SurveyID")

veg <- a %>%
  left_join(herb_wide, by = "SubplotSurveyID") 
  left_join(sapshrub_wide, by = "SubplotSurveyID") 

herbs <- SurveyHerbaceous %>%
  left_join(SurveySubplot, by = "SubplotSurveyID") %>%
  left_join(SurveyBox, by = "SurveyID") %>%
  mutate(Stratum = "Herbaceous")
