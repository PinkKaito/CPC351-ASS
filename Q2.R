
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

required_columns <- c(
  "Origin_X", "Origin_Y", "Dest_X", "Dest_Y",
  "Weather", "Time_of_Day", "Travel_Time_Minutes"
)
missing_columns <- setdiff(required_columns, names(travel_data))
if (length(missing_columns) > 0) {
  stop("Missing required CSV columns: ",
       paste(missing_columns, collapse = ", "))
}

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

# Remove NA, NaN, infinity, and nonpositive travel times (including -1).
# Only finite, positive times can contribute to a route's percentile.

clean_data <- rainy_morning_data[
  is.finite(rainy_morning_data$Travel_Time_Minutes) &
  rainy_morning_data$Travel_Time_Minutes > 0,
]

unit_warning_threshold <- 300
if (nrow(clean_data) > 0 &&
    any(clean_data$Travel_Time_Minutes > unit_warning_threshold)) {
  warning(
    "Some travel times exceed ", unit_warning_threshold,
    " minutes. Verify source units; values remain included."
  )
}

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

sample_count_matrix <- matrix(
  0,
  nrow = 10,
  ncol = 10,
  dimnames = list(point_names, point_names)
)

route_status_matrix <- matrix(
  "Pending",
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
      route_status_matrix[i, j] <- "Same point"

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

        sample_count_matrix[i, j] <- length(route_times)
        route_status_matrix[i, j] <- "Valid"

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
      route_status_matrix[i, j] <- "Penalized"
      blocked_routes <- blocked_routes + 1
    }
  }
}


# PART F: Display final results

cat("\n--- Data Reliability: Valid Sample Counts ---\n")
print(sample_count_matrix)

cat("\n--- Data Reliability: Route Status ---\n")
print(route_status_matrix)

cat("\n\n\n================================================================\n\n\n")

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

n_points <- nrow(cost_matrix)

# Check the matrix before selecting destinations. Finite penalties remain
# eligible so a tour can still be produced when usable connections run out.
if (n_points != ncol(cost_matrix) || n_points < 2 ||
    any(!is.finite(cost_matrix)) || any(cost_matrix < 0)) {
  stop("Routing requires a square matrix of finite, nonnegative costs.")
}
if (length(start_point) != 1 || !is.finite(start_point) ||
    start_point != floor(start_point) || start_point < 1 || start_point > n_points) {
  stop("Starting point must be an integer within the cost matrix.")
}
current_point <- start_point

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

# Verify the closed tour and independently sum every leg, including the return.
if (length(route) != n_points + 1 || route[1] != start_point ||
    tail(route, 1) != start_point ||
    !identical(sort(as.integer(route[seq_len(n_points)])), seq_len(n_points))) {
  stop("Route validation FAILED: Visit each point once and return to the start.")
}
leg_indices <- cbind(head(route, -1), tail(route, -1))
leg_costs <- cost_matrix[leg_indices]
if (!isTRUE(all.equal(total_time, sum(leg_costs)))) {
  stop("Route validation FAILED: Total does not match the selected legs.")
}

penalized_legs <- route_status_matrix[leg_indices] == "Penalized"
route_legs <- data.frame(
  From = point_names[head(route, -1)],
  To = point_names[tail(route, -1)],
  Cost = as.numeric(leg_costs),
  Penalized = as.logical(penalized_legs)
)

# PART D: Display the final route and total travel time
route_names <- point_names[route]

cat("\n--- Question 2(b): Nearest Neighbour Result ---\n")

cat("Final route:", paste(route_names, collapse = " -> "), "\n")

cat("\n--- Selected Route Legs (including return) ---\n")
print(route_legs, row.names = FALSE)
cat("\nRoute validation PASSED: Each point visited once; return and total checked.\n")
cat("Penalized legs used:", sum(penalized_legs), "\n")
if (any(penalized_legs)) {
  cat("Total routing cost (including artificial penalties):", total_time, "\n")
  cat("This tour uses connections without valid history; the total is not a realistic travel-time estimate.\n")
} else {
  cat("Total expected travel time:", total_time, "minutes\n")
}
