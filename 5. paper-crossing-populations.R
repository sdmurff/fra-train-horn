library("dplyr")
library("tidyr")


#### USA Analysis ####
# Load in census data which is already linked to crossings
census_crossings <- read.csv("~/crossings_census_join_US1.csv")

# Load in US crossings that have gates, bells, and lights (comparable to Florida crossings in the 1980s)
crossings_gates_bells_lights <- read.csv("~/crossings_gates_bells_lights.csv")

# Join census data with crossings that have gates, bells, and lights
census_crossings_gates_bells_lights1 <- crossings_gates_bells_lights %>%
  inner_join(census_crossings, by = "Crossing.ID")

# Sum up population from census blocks near train crossings
crossings_gates_bells_lights_adj_pop <- census_crossings_gates_bells_lights1 %>%
  group_by(GEOID20) %>%
  filter(any(!is.na(adjusted_population) & adjusted_population > 0)) %>%  # Ensure at least one valid value
  filter(adjusted_population == max(adjusted_population, na.rm = TRUE)) %>%
  slice(1) %>%  # Keep only one row per GEOID20 to avoid double counting
  ungroup() %>%
  group_by(Crossing.ID) %>%
  summarise(TOTAL_ADJ_POP20 = sum(adjusted_population)) %>%
  ungroup()

# Merge sum back into base crossing file
crossings_gates_bells_lights1 <- crossings_gates_bells_lights %>%
  inner_join(crossings_gates_bells_lights_adj_pop, by = "Crossing.ID")

# Sum up the population of residents living within 500 feet of the train crossings
sum(crossings_gates_bells_lights1$TOTAL_ADJ_POP20[is.finite(crossings_gates_bells_lights1$TOTAL_ADJ_POP20)])

# Output to CSV for later use
write.csv(crossings_gates_bells_lights1, "~/crossings_gates_bells_lights1.csv")





utah_quiet_zones <- read.csv("~/utah_quiet_zones.csv")
utah_quiet_zones1 <- utah_quiet_zones |>
  filter(nchar(Crossing.ID) == nchar("859670T"))

# Merge sum back into base crossing file
utah_census_crossings <- utah_quiet_zones1 %>%
  left_join(census_crossings, by = "Crossing.ID")

# Sum up population from census blocks near train crossings
crossing_adjusted_pop <- utah_census_crossings %>%
  group_by(GEOID20) %>%
  filter(any(!is.na(adjusted_population) & adjusted_population > 0)) %>%  # Ensure at least one valid value
  filter(adjusted_population == max(adjusted_population, na.rm = TRUE)) %>%
  slice(1) %>%  # Keep only one row per GEOID20 to avoid double counting
  ungroup() %>%
  group_by(Crossing.ID) %>%
  summarise(TOTAL_ADJ_POP20 = sum(adjusted_population)) %>%
  ungroup()
 
# Select subset of columns
usa_crossings <- census_crossings %>%
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
         Longitude) %>%
  distinct()
  
# Merge sum back into base crossing file
usa_crossings1 <- utah_quiet_zones1 %>%
  left_join(crossing_adjusted_pop, by = "Crossing.ID") |>
  group_by(THR.Request.No) |>
  summarise(total_pop = sum(TOTAL_ADJ_POP20, na.rm = TRUE))

sum(usa_crossings1$TOTAL_ADJ_POP20[is.finite(usa_crossings1$TOTAL_ADJ_POP20)])

write.csv(usa_crossings1, "~/usa_crossings_adj_population.csv")

# Sum up population not in a quiet zone.
TotalInUS <- usa_crossings1 %>%
  filter(THR.Request.No =="")

sum(TotalInUS$TOTAL_ADJ_POP20[is.finite(TotalInUS$TOTAL_ADJ_POP20)])

