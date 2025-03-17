library(dplyr)
library(lubridate)
library(hms)
library(tidyr)
library(ggplot2)

# Next steps
# 1) Merge in quiet zon e data
# 2) Estimate Nation-wide how many people likely lose sleep due to the train horns
# 3) What percent of affected population (within 1k feet of crossing) live within a quiet zone
# 4)


crossings <- read.csv("~/Crossing_Inventory_Data__Form_71__-_Current_20241203.csv")

# Filter crossings to exclude
#   Not at-grade
#   Not closed
#   Not private
#   Does not run down the street
#   Is not a local transit train (e.g. light rail) or tourist train
#   Is paved

crossings_filtered <- crossings %>%
  filter(Crossing.Position=="At Grade", 
         Crossing.Closed=="No",
         !grepl("PVT|PRIV", Highway.Name, ignore.case = TRUE),
         Track.Run.Down.Street != "Yes",
         !grepl("\\b14\\b|\\b16\\b", Type.Of.Train.Service.IDs),
         Highway.Paved != "No") %>%
  select(Crossing.ID,
         Annual.Average.Daily.Traffic.Count,
         Revision.Date,
         Reason.Description,
         Reporting.Agency.Name,
         Railroad.Name,
         City.Name,
         State.Name,
         Type.Of.Train.Service.IDs,
         Type.Of.Train.Service.1,
         Type.Of.Train.Service.2,
         Type.Of.Train.Service.3,
         Type.Of.Train.Service.4,
         Total.Switching.Trains,
         Total.Nighttime.Thru.Trains,
         Total.Daylight.Thru.Trains,
         Total.Transit.Trains,
         Development.Type,
         Crossing.Purpose,
         Track.Run.Down.Street,
         Number.Of.Yard.Tracks,
         Number.Of.Industry.Tracks,
         Number.Of.Main.Tracks,
         Number.Of.Transit.Tracks,
         Number.Of.Siding.Tracks,
         Highway.Paved,
         Road.At.Crossing,
         Road.At.Crossing.Type,
         Street,
         Highway.Name,
         Crossing.Type,
         Crossing.Position,
         Whistle.Ban,
         Whistle.Date,
         Quiet.Zone.ID,
         THR.Request.No,
         Latitude,
         Longitude,
         Channelization.Devices,
         Crossing.Closed,
         Url)

crossings_gis <- crossings_filtered |>
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude),
         Revision.Date = as.Date(Revision.Date, format = "%m/%d/%Y"),
         Total.Non.Transit = Total.Daylight.Thru.Trains + Total.Nighttime.Thru.Trains - Total.Transit.Trains) |>
  filter(!is.na(Latitude) & !is.na(Longitude)) |>
  select(Revision.Date,
         Crossing.ID,
         State.Name,
         City.Name,
         Street,
         Highway.Name,
         THR.Request.No,
         Type.Of.Train.Service.IDs,
         Type.Of.Train.Service.1,
         Type.Of.Train.Service.2,
         Type.Of.Train.Service.3,
         Type.Of.Train.Service.4,
         Total.Switching.Trains,
         Total.Nighttime.Thru.Trains,
         Total.Daylight.Thru.Trains,
         Total.Transit.Trains,
         Total.Non.Transit,
         Development.Type,
         Crossing.Purpose,
         Track.Run.Down.Street,
         Number.Of.Yard.Tracks,
         Number.Of.Industry.Tracks,
         Number.Of.Main.Tracks,
         Number.Of.Transit.Tracks,
         Number.Of.Siding.Tracks,
         Highway.Paved,
         Road.At.Crossing,
         Road.At.Crossing.Type,
         Latitude,
         Longitude)

duplicates <- crossings_gis %>%
  group_by(Crossing.ID) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  arrange(Crossing.ID) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result

crossings_gis1 <- crossings_gis %>%
  distinct(Crossing.ID, .keep_all = TRUE)

duplicates <- crossings_gis1 %>%
  group_by(Latitude, Longitude) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  arrange(Latitude) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result

crossings_gis2 <- crossings_gis1 %>%
  group_by(Latitude, Longitude) %>% # Group by all other columns
  slice_max(order_by = Revision.Date, n = 1) %>% # Keep the row with the most recent date
  distinct(Latitude, Longitude, .keep_all = TRUE) %>% # Removes duplicates if multiple rows have the same date
  ungroup() # Remove grouping for a clean result

# Check for final duplicates
duplicates <- crossings_gis2 %>%
  group_by(Latitude, Longitude) %>%
  filter(n() > 1) %>% # Keeps rows where the group size is greater than 1
  arrange(Latitude) %>% # Sort by Latitude
  ungroup() # Remove grouping for a clean result

write.csv(crossings_gis2, "~/crossings_usa_clean1.csv", row.names = FALSE)