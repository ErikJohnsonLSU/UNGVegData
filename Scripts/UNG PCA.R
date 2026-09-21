#### Allowing R to access Microsoft Access ----
library(DBI)
install.packages("odbc")
library(odbc)

con <- dbConnect(
  odbc::odbc(),
  .connection_string = "Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=C:/Users/alexb/Desktop/R Projects/UNG/UNG Veg Surveys.accdb")
Reading Tables
dbListTables(con)          # shows all tables
df <- dbReadTable(con, "ExactTableName")   # read a table
dbDisconnect(con


