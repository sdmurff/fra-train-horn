library("dplyr")
library("tidyr")
library("lubridate")
library("plotrix")

# Read in the CSV files using read.csv
crossings <- read.csv("~/crossings_gates_bells_lights1.csv")
accidents <- read.csv("~/Highway-Rail_Grade_Crossing_Accident_Data__Form_57__20241212.csv")
night_accidents <- accidents |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"),
         Time1 = hms::as_hms(parse_date_time(Time, orders = "I:M p")),
         HourOfDay = hour(Time1), # Extract the hour
         TimeOfDay = ifelse(HourOfDay >= 7 & HourOfDay < 23, "Waking Hours", "Sleeping Hours"),
         Suicide = ifelse(Highway.User.Action=="Suicide/attempted suicide", "Suicide", "Other"),
         Crossing.ID = Grade.Crossing.ID) |>
  filter(Total.Killed.Form.57 + Total.Injured.Form.57 > 0,
         Report.Year >= 2020 & Report.Year <= 2024,
         !(HourOfDay >= 6 & HourOfDay < 22)) |>
  mutate(LifeExpectancy = 77.5,
         DALY = pmax(Crossing.Users.Killed*77.5 - Crossing.Users.Killed*User.Age,Crossing.Users.Killed*7) + 
           pmax(Employees.Killed*77.5 - Employees.Killed*42,0) + 
           pmax(Passengers.Killed*77.5 - Passengers.Killed*35,0) + 
           pmax(Crossing.Users.Injured*77.5 - Crossing.Users.Injured*User.Age,7)*0.6 +
           pmax(Employees.Injured*77.5 - Employees.Injured*42,0)*0.6 +
           pmax(Passengers.Injured*77.5 - Passengers.Injured*35,0)*0.6,
         DWAnnoyance = .02,
         DWDisturbance = .07) |>
  select(Crossing.ID,
         Grade.Crossing.ID,
       Whistle.Ban,
       Date,
       Report.Year,
       Time,
       Time1,
       HourOfDay,
       TimeOfDay,
       County.Name,
       State.Name,
       City.Name, 
       Highway.Name,
       Public.Private,
       Highway.User,
       Estimated.Vehicle.Speed,
       Highway.User.Position,
       Visibility,
       Equipment.Type,
       Train.Speed,
       Crossing.Warning.Expanded.1,
       Crossing.Warning.Expanded.2,
       Crossing.Warning.Expanded.3,
       Crossing.Warning.Expanded.4,
       Signaled.Crossing.Warning,
       User.Gender,
       Highway.User.Action,
       Suicide,
       Crossing.Users.Killed,
       Crossing.Users.Injured,
       Employees.Killed,
       Employees.Injured,
       Passengers.Killed,
       Passengers.Injured,
       User.Age,
       LifeExpectancy,
       DALY,
       DWAnnoyance,
       DWDisturbance,
       Narrative,
       Total.Killed.Form.57,
       Total.Injured.Form.57,
       Total.Killed.Form.55A,
       Total.Injured.Form.55A,
       Whistle.Ban,
       Reporting.Parent.Railroad.Name,
       Reporting.Railroad.Holding.Company,
       Url
)

night_accidents1 <- inner_join(night_accidents, crossings, by = "Crossing.ID")

#### Nationwide DALY Calculations ####
# Total DALYs lost from train accidents
total_DALY <- sum(night_accidents1$DALY)
total_DALY

# Total DALYs saved under assumption of (195%-23%) increase of night night accidents from FRA study
total_DALY*(1 + 1.95-0.23) - total_DALY

# DALYs lost from public health and safety costs of train horn rule
# Lower bound with DW of 0.02 (annoyance)
(918251*0.02*5)
# Decimal percent
(918251*0.02*5)/(total_DALY*(1 + 1.95-0.23) - total_DALY)-1
(918251*0.02*5*0.25)/(total_DALY*(1 + 1.95-0.23) - total_DALY)-1
(918251*0.02*5*0.50)/(total_DALY*(1 + 1.95-0.23) - total_DALY)-1

# Upper bound with DW of 0.07 (disturbance)
(918251*0.07*5)
# Decimal percent
(918251*0.07*5)/(total_DALY*(1 + 1.95-0.23) - total_DALY)-1

# Net zero cost of policy
(total_DALY*(1 + 1.95-0.23) - total_DALY)/(0.02*5)


#### Chart ####
# Create a sequence of disability weights (DW) from 0.02 to 0.07 in increments of 0.01.
disability_weights <- seq(0.02, 0.07, by = 0.01)

# Create a sequence for the proportion from 0.25 to 1 in increments of 0.25.
proportions <- seq(0.25, 1, by = 0.25)

# Initialize a matrix to store the computed ratios.
# Rows: disability weights; Columns: proportions.
ratios <- matrix(NA, nrow = length(disability_weights), ncol = length(proportions))

# Compute the ratio for each combination of disability weight and proportion.
for (j in seq_along(proportions)) {
  for (i in seq_along(disability_weights)) {
    ratios[i, j] <- (918251 * proportions[j] * disability_weights[i] * 5) / (total_DALY*(1 + 1.95-0.23) - total_DALY) - 1
  }
}

# Multiply ratios by 100 to express them as percentages.
ratios_percent <- ratios * 100

# Plot the results without the default y-axis.
# Define a vector of line types for each proportion scenario.
line_types <- 1:length(proportions)
plot(disability_weights, ratios_percent[, 1], type = "l", lwd = 2, yaxt = "n", lty = line_types[1], col = "black",
     xlim = c(0.02, 0.07),
     ylim = c(0, max(ratios_percent)),
     xlab = "Disability Weight (DW)",
     ylab = "% Increase",
     main = "% Increase in DALYs lost")

# Add lines for each proportion scenario.
for (j in seq_along(proportions)) {
  lines(disability_weights, ratios_percent[, j], col = "black", lwd = 2, lty = line_types[j])
}


# Generate custom y-axis ticks (e.g., roughly 10 tick marks)
y_ticks <- pretty(c(0, max(ratios_percent)), n = 10)

# Add the y-axis with custom tick marks formatted as percentages.
axis(2, at = y_ticks, labels = paste0(y_ticks, "%"))

# Place the legend in the top left corner.
legend("topleft", legend = proportions, title = "Proportion of affected residents", col ="black", lty = line_types, lwd = 2)

# Add a red point at (0.2, 0%)
points(0.02, 0, pch = 8, col = "black", cex = 1)

# Add the text label slightly above the point.
text(0.0264, 0, "DALY Cost = DALY Benefit", pos = 3, offset = -.16, col ="black", cex = .6)


#### Utah DALY Calculations ####
# Total DALYs lost from train accidents
utah_crossings <- crossings |>
  filter(THR.Request.No %in% c("THR_Request_000000111217", "THR_Request_000000111670"))
night_accidents1_utah <- inner_join(night_accidents, utah_crossings, by = "Crossing.ID")

accident_DALYs <- sum(night_accidents1_utah$DALY)*((6/12)/5)
sum(night_accidents1_utah$DALY)
accident_DALYs

accident_DALYs*(1 + 0.25)
# Total DALYs saved under assumption of (195% - 23%) increase of night night accidents from FRA study
accident_DALYs*(1 + 0.25) - accident_DALYs

# DALYs lost from public health and safety costs of train horn rule
# Lower bound with DW of 0.02 (annoyance)
(2454*0.02*(6/12)) + (2371*0.02*(4/12))
# Multiple after standardizing time period to be comparable
((2454*0.02*(6/12)) + (2371*0.02*(4/12)))/(accident_DALYs*(1 + 0.25))-1
((2454*0.02*(6/12)*.25) + (2371*0.02*(4/12)*.25))
((2454*0.02*(6/12)*.25) + (2371*0.02*(4/12)*.25))/(accident_DALYs*(1 + 0.25))-1

# Upper bound with DW of 0.07 (disturbance)
(2454*0.07*(6/12)) + (2371*0.07*(4/12))
# Multiple
((2454*0.07*(6/12)) + (2371*0.07*(4/12)))/(accident_DALYs*(1 + 0.25))-1

# Net zero cost of policy
(accident_DALYs*(1 + 0.25))/(0.02*(6/12))

#### Utah Chart ####

# Create a sequence of disability weights (DW) from 0.02 to 0.07 in increments of 0.01.
disability_weights <- seq(0.02, 0.07, by = 0.01)

# Create a sequence for the proportion from 0.25 to 1 in increments of 0.25.
proportions <- seq(0.25, 1, by = 0.25)

# Initialize a matrix to store the computed ratios.
# Rows: disability weights; Columns: proportions.
ratios <- matrix(NA, nrow = length(disability_weights), ncol = length(proportions))

# Compute the ratio for each combination of disability weight and proportion.
for (j in seq_along(proportions)) {
  for (i in seq_along(disability_weights)) {
    ratios[i, j] <- ((2454 * proportions[j] * disability_weights[i] * (6/12)) + (2371 * proportions[j] * disability_weights[i] * (4/12))) / (accident_DALYs*(1 + 0.25)) -1
  }
}

# Multiply ratios by 100 to express them as percentages.
ratios_percent <- ratios * 100

gap_interval <- c(0.0, 0.02)

# Plot the results without the default y-axis.
# Define a vector of line types for each proportion scenario.
line_types <- 1:length(proportions)
plot(disability_weights, ratios_percent[, 1], type = "l", lwd = 2, yaxt = "n", lty = line_types[1], col = "black",
     gap = gap_interval,
     xlim = c(0., 0.07),
     ylim = c(-.1, max(ratios_percent)),
     xlab = "Disability Weight (DW)",
     ylab = "% Increase",
     main = "% Increase in DALYs lost in Utah")

# Add custom x-axis ticks. Here, for example, we show ticks at several values.
axis(1, at = c(0.02, 0.03, 0.04, 0.05, 0.06, 0.07),
     labels = c("0.02", "0.03", "0.04", "0.05", "0.06", "0.07"))

# Insert an axis break.
# Here, breakpos is set at 0.08 and brw (the break width) is set to 0.01.
axis.break(1, breakpos = 0.01, style = "slash", brw = 0.01)

# Add lines for each proportion scenario.
for (j in seq_along(proportions)) {
  lines(disability_weights, ratios_percent[, j], col = "black", lwd = 2, lty = line_types[j])
}

# Generate custom y-axis ticks (e.g., roughly 10 tick marks)
y_ticks <- pretty(c(-.1, max(ratios_percent)), n = 10)

# Add the y-axis with custom tick marks formatted as percentages.
axis(2, at = y_ticks, labels = paste0(y_ticks, "%"))

# Place the legend in the top left corner.
legend("topleft", legend = proportions, title = "Proportion of affected residents", col ="black", lty = line_types, lwd = 2)

# Add a red point at (0.2, 0%)
points(0, 6.6, pch = 8, col = "black", cex = 1)

# Add the text label "net zero" slightly above the point.
text(0.009, 0, "6.6 DALYs w/no suspension", pos = 3, offset = -0.1, col ="black", cex = .6)







