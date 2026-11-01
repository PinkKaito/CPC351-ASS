
# ==========================================
# QUESTION 2(a)
# Data Preprocessing and Cost Matrix
# ==========================================

# PART A: Define the 10 delivery points

points <- matrix(c(
  60, 200,
  180, 200,
  80, 180,
  140, 180,
  20, 160,
  100, 160,
  200, 160,
  140, 140,
  40, 120,
  100, 120
), ncol = 2, byrow = TRUE)

point_names <- c(
  "P1", "P2", "P3", "P4", "P5",
  "P6", "P7", "P8", "P9", "P10"
)


# PART B: Import historical travel data

travel_data <- read.csv("ahmad_travel_logs.csv")

cat("\n--- Original Data Summary ---\n")
print(summary(travel_data))


# PART C: Filter and clean the data

# Only Rainy morning records with valid travel
# times and coordinates will be retained.

clean_data <- travel_data[
  !is.na(travel_data$Weather) &
  !is.na(travel_data$Time_of_Day) &
  travel_data$Weather == "Rainy" &
  travel_data$Time_of_Day == "Morning" &
  !is.na(travel_data$Travel_Time_Minutes) &
  travel_data$Travel_Time_Minutes > 0 &
  !is.na(travel_data$Origin_X) &
  !is.na(travel_data$Origin_Y) &
  !is.na(travel_data$Dest_X) &
  !is.na(travel_data$Dest_Y),
]

cat("\n--- Cleaned Data Summary ---\n")
print(summary(clean_data))

cat("\nValid Rainy morning records:",
    length(clean_data$Travel_Time_Minutes), "\n")


# PART D: Create the 10 x 10 cost matrix

cost_matrix <- matrix(
  NA,
  nrow = 10,
  ncol = 10,
  dimnames = list(point_names, point_names)
)

# Calculate 75th percentile for each route

for (i in 1:10) {

  for (j in 1:10) {

    # No travelling from a point to itself
    if (i == j) {
      cost_matrix[i, j] <- 0

    } else {

      route_times <- c()

      # Check each historical record
      if (length(clean_data$Travel_Time_Minutes) > 0) {

        for (k in 1:length(clean_data$Travel_Time_Minutes)) {

          # Match origin and destination coordinates
          if (
            clean_data$Origin_X[k] == points[i, 1] &&
            clean_data$Origin_Y[k] == points[i, 2] &&
            clean_data$Dest_X[k] == points[j, 1] &&
            clean_data$Dest_Y[k] == points[j, 2]
          ) {

            route_times <- c(
              route_times,
              clean_data$Travel_Time_Minutes[k]
            )
          }
        }
      }

      # Calculate the third quartile (75th percentile)
      if (length(route_times) > 0) {

        route_summary <- summary(route_times)

        # The fifth value is the third quartile
        cost_matrix[i, j] <- route_summary[5]
      }

      # Routes without valid records remain NA
      # until penalty calculation.
    }
  }
}


# PART E: Calculate severe penalty

# Find the largest valid historical travel time
largest_time <- 0

if (length(clean_data$Travel_Time_Minutes) > 0) {

  for (k in 1:length(clean_data$Travel_Time_Minutes)) {

    if (clean_data$Travel_Time_Minutes[k] > largest_time) {
      largest_time <- clean_data$Travel_Time_Minutes[k]
    }
  }
}

# Dynamic penalty larger than any normal route cost
if (largest_time > 0) {
  penalty <- largest_time * 10 + 1
} else {
  # Fallback when no valid travel records exist
  penalty <- 999999
}

# Replace missing route costs with penalty
blocked_routes <- 0

for (i in 1:10) {
  for (j in 1:10) {

    if (i != j && is.na(cost_matrix[i, j])) {

      cost_matrix[i, j] <- penalty
      blocked_routes <- blocked_routes + 1
    }
  }
}


# PART F: Display final results

cat("\n--- Question 2(a): Final Cost Matrix ---\n")
print(cost_matrix)

cat("\nPenalty cost:", penalty, "minutes\n")

cat("Routes without valid travel history:",
    blocked_routes, "\n")