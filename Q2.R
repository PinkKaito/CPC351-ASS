
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

# First keep all Rainy Morning records
# Closure records are still kept at this stage

rainy_morning_data <- travel_data[
  !is.na(travel_data$Weather) &
  !is.na(travel_data$Time_of_Day) &
  tolower(trimws(travel_data$Weather)) == "rainy" &
  tolower(trimws(travel_data$Time_of_Day)) == "morning" &
  !is.na(travel_data$Origin_X) &
  !is.na(travel_data$Origin_Y) &
  !is.na(travel_data$Dest_X) &
  !is.na(travel_data$Dest_Y),
]

# Remove invalid travel times such as NA and -1.
# Travel times must be positive.

clean_data <- rainy_morning_data[
  !is.na(rainy_morning_data$Travel_Time_Minutes) &
  rainy_morning_data$Travel_Time_Minutes > 0,
]

cat("\n--- Cleaned Data Summary ---\n")
print(summary(clean_data))

cat("\nRainy Morning records before cleaning:", 
    nrow(rainy_morning_data),"\n")

cat("Valid Rainy Morning records:",
    nrow(clean_data),"\n")

cat("Invalid/closure records removed:",
    nrow(rainy_morning_data) - nrow(clean_data), "\n")


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

        cost_matrix[i,j] <- quantile(
          route_times,
          probs = 0.75,
          names = FALSE
        )
      }

      # Routes without valid records remain NA
      # until penalty calculation.
    }
  }
}


# PART E: Calculate severe penalty

# Find the largest valid historical travel time
if(nrow(clean_data) > 0){
  largest_time <- max(clean_data$Travel_Time_Minutes)
  
  # Dynamic penalty larger than any normal route cost
  penalty <- largest_time * 10 + 1
}else{
  
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

# ==========================================
# QUESTION 2(b)
# Nearest Neighbour Algorithm
# ==========================================

# PART A: Set the starting point

start_point <- 1 # P1
current_point <- start_point

n_points <- nrow(cost_matrix)

# Keep track of visited points
visited <- rep(FALSE, n_points)
visited[start_point] <- TRUE

# Store the route
route <- c(start_point)

# Store the total expected travel time
total_time <- 0

# PART B: Find the nearest unvisited point
for( step in 1:(n_points - 1)){
  
  # Get travel costs from the current point
  available_costs <- cost_matrix[current_point,]
  
  # Prevent already visited points from being selected by using Inf (Infinity)
  available_costs[visited] <- Inf
  
  # Select the unvisited point with the lowest cost
  next_point <- which.min(available_costs)
  
  # Add the travel cost
  total_time <- total_time + cost_matrix[current_point, next_point]
  
  # Add the selected point to the route
  route <- c(route, next_point)
  
  # Mark the selected point as visited
  visited[next_point] <- TRUE
  
  # Move to the selected point
  current_point <- next_point
}

# PART C: Return to the starting point
total_time <- total_time + cost_matrix[current_point, start_point]

route <- c(route,start_point)

# PART D: Display the final route and total travel time
route_names <- point_names[route]

cat("\n--- Question 2(b): Nearest Neighbour Result ---\n")

cat("Final route:", paste(route_names, collapse = " -> "), "\n")

cat("Total expected travel time:", total_time, "minutes\n")