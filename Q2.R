
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

# Preserve raw values so the audit can distinguish missing and malformed times.
raw_times <- trimws(as.character(travel_data$Travel_Time_Minutes))
travel_data$Travel_Time_Minutes <- suppressWarnings(as.numeric(raw_times))

cat("\n--- Original Data Summary ---\n")
print(summary(travel_data))

# PART C: Filter conditions, validate delivery points, and classify each row.
target <- !is.na(travel_data$Weather) & !is.na(travel_data$Time_of_Day) &
  tolower(trimws(travel_data$Weather)) == "rainy" &
  tolower(trimws(travel_data$Time_of_Day)) == "morning"
rainy_morning_data <- travel_data[target, , drop = FALSE]
raw_target_times <- raw_times[target]
point_keys <- paste(points[, 1], points[, 2], sep = ",")
origin_id <- match(paste(rainy_morning_data$Origin_X,
                         rainy_morning_data$Origin_Y, sep = ","), point_keys)
dest_id <- match(paste(rainy_morning_data$Dest_X,
                       rainy_morning_data$Dest_Y, sep = ","), point_keys)
valid_coordinates <- !is.na(origin_id) & !is.na(dest_id)
self_journey <- valid_coordinates & origin_id == dest_id
self_journey[is.na(self_journey)] <- FALSE

# Each target-condition record receives exactly one reason, in this priority order.
times <- rainy_morning_data$Travel_Time_Minutes
cleaning_reason <- rep("Valid", nrow(rainy_morning_data))
classify <- function(mask, label) {
  mask[is.na(mask)] <- FALSE
  cleaning_reason[mask & cleaning_reason == "Valid"] <<- label
}
classify(!valid_coordinates, "Invalid coordinates")
classify(self_journey, "Same-point journey")
classify(is.na(raw_target_times) | raw_target_times %in% c("", "NA"), "Missing time")
classify(is.nan(times) | is.infinite(times), "Non-finite time")
classify(is.na(times), "Malformed time")
classify(times == -1, "Closure (-1)")
classify(times == 0, "Zero time")
classify(times < 0, "Other negative time")
cleaning_categories <- c("Invalid coordinates", "Same-point journey", "Missing time",
                         "Non-finite time", "Malformed time", "Closure (-1)",
                         "Zero time", "Other negative time", "Valid")
cleaning_audit <- data.frame(
  Reason = cleaning_categories,
  Records = as.integer(table(factor(cleaning_reason, levels = cleaning_categories)))
)
stopifnot(sum(cleaning_audit$Records) == nrow(rainy_morning_data))
clean_data <- rainy_morning_data[cleaning_reason == "Valid", , drop = FALSE]
cat("\n--- Rainy Morning Cleaning Audit (one reason per record) ---\n")
print(cleaning_audit, row.names = FALSE)
cat("Records outside Rainy Morning conditions:", sum(!target), "\n")

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

      # Preserve the reason for unavailable history separately from the penalty flag.
      if (length(route_times) == 0) {
        pair_rows <- which(valid_coordinates & !self_journey &
                           origin_id == i & dest_id == j)
        if (length(pair_rows) == 0) {
          route_status_matrix[i, j] <- "No observations"
        } else if (all(cleaning_reason[pair_rows] %in%
                       c("Missing time", "Closure (-1)"))) {
          route_status_matrix[i, j] <- "Closure-only history"
        } else {
          route_status_matrix[i, j] <- "Other invalid history"
        }
      }
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

if (!is.finite(penalty) || penalty <= 0) {
  stop("Penalty calculation overflowed; travel times are too large.")
}
penalty_matrix <- is.na(cost_matrix)
diag(penalty_matrix) <- FALSE

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


# Independently verify the completed matrix and the history supporting each entry.
validate_cost_matrix <- function(costs, counts, penalties, penalty_value, labels) {
  expected_names <- list(labels, labels)
  off_diagonal <- row(costs) != col(costs)
  if (!identical(dim(costs), c(length(labels), length(labels))) ||
      !identical(dimnames(costs), expected_names) ||
      !identical(dim(counts), dim(costs)) ||
      !identical(dim(penalties), dim(costs)) ||
      any(!is.finite(costs)) || any(diag(costs) != 0) ||
      any(costs[off_diagonal] <= 0) ||
      any(diag(counts) != 0) || any(counts < 0) ||
      any(diag(penalties)) ||
      any(penalties[off_diagonal] != (counts[off_diagonal] == 0)) ||
      !is.finite(penalty_value) || penalty_value <= 0 ||
      any(costs[penalties] != penalty_value) ||
      any(costs[off_diagonal & !penalties] >= penalty_value)) {
    stop("Cost matrix validation FAILED: Check labels, costs, counts and penalties.")
  }
  TRUE
}
validate_cost_matrix(cost_matrix, sample_count_matrix, penalty_matrix, penalty, point_names)
cat("\nCost matrix validation PASSED: Structure, history and penalties checked.\n")

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


# PART A: Create a reusable Nearest Neighbour function

nearest_neighbour <- function(cost_matrix, start_point = 1) {
  
  # Get the number of delivery points
  n_points <- nrow(cost_matrix)
  
  # Check that the cost matrix is valid
  if (n_points != ncol(cost_matrix) || n_points < 2 ||
      any(!is.finite(cost_matrix)) || any(cost_matrix < 0)) {
    stop("Routing requires a square matrix of finite, nonnegative costs.")
  }
  
  # Check that the starting point is valid
  if (length(start_point) != 1 || !is.numeric(start_point) ||
      !is.finite(start_point) ||
      start_point != floor(start_point) ||
      start_point < 1 || start_point > n_points) {
    stop("Starting point must be an integer within the cost matrix.")
  }
  
  # Start from the chosen point
  current_point <- start_point
  
  # Keep track of visited points
  visited <- rep(FALSE, n_points)
  visited[start_point] <- TRUE
  
  # Store the route and total travel time
  route <- c(start_point)
  total_time <- 0
  
  # Find the nearest unvisited point
  for (step in 1:(n_points - 1)) {
    
    # Get travel costs from the current point
    available_costs <- cost_matrix[current_point, ]
    
    # Prevent visiting the same point again
    available_costs[visited] <- Inf
    
    # Choose the nearest unvisited point
    next_point <- which.min(available_costs)
    
    # Add the travel cost
    total_time <- total_time + cost_matrix[current_point, next_point]
    
    # Update the route
    route <- c(route, next_point)
    
    # Mark the point as visited
    visited[next_point] <- TRUE
    
    # Move to the next point
    current_point <- next_point
  }
  
  # Return to the starting point
  total_time <- total_time + cost_matrix[current_point, start_point]
  route <- c(route, start_point)
  
  # Return both results
  return(list(route = route, total_time = total_time))
}


# PART B: Run the Nearest Neighbour function

start_point <- 1

nn_result <- nearest_neighbour(cost_matrix, start_point)

# Save the results for validation and display
route <- nn_result$route
total_time <- nn_result$total_time
n_points <- nrow(cost_matrix)


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

penalized_legs <- penalty_matrix[leg_indices]
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
  
  # Identify which routes are penalized
  penalized_routes <- paste(
    route_legs$From[penalized_legs],
    "->",
    route_legs$To[penalized_legs],
    collapse = ", "
  )
  
  # Display the artificial total
  cat("Total routing cost (including artificial penalties):",
      total_time, "minutes\n")
  
  # Display a warning showing the penalized routes
  warning(
    "Artificial routing cost! Penalized leg(s): ",
    penalized_routes,
    ". Total is not a realistic travel-time estimate.",
    call. = FALSE
  )
  
} else {
  
  # No penalties used
  cat("Total expected travel time:", total_time, "minutes\n")
}


# Plot a graph displaying tour map
plot(
  points[, 1], points[, 2],
  xlim = c(0, 220), ylim = c(105, 215),
  pch = 19, cex = 1.6, col = "steelblue",
  xlab = "X Coordinate", ylab = "Y Coordinate",
  main = paste0("Q2 Nearest Neighbour Tour (Total: ", round(total_time, 2), " mins)")
)
grid()

# Draw directed arrows for each leg in the tour
for (k in 1:(length(route) - 1)) {
  from <- route[k]
  to <- route[k + 1]
  arrows(
    points[from, 1], points[from, 2],
    points[to, 1], points[to, 2],
    length = 0.12, lwd = 1.8,
    col = ifelse(k == length(route) - 1, "darkgreen", "darkorange")
  )
  mid_x <- (points[from, 1] + points[to, 1]) / 2
  mid_y <- (points[from, 2] + points[to, 2]) / 2
  text(mid_x, mid_y, labels = round(cost_matrix[from, to], 1), cex = 0.75, col = "darkred")
}

# Highlight P1 start/end and label all points
points(points[start_point, 1], points[start_point, 2], pch = 19, cex = 2.0, col = "darkgreen")
text(points[, 1], points[, 2] + 5, labels = point_names, font = 2)
legend(
  "bottomright",
  legend = c("Start / End (P1)", "Delivery Leg (75th %ile mins)", "Return Leg to P1"),
  pch = c(19, NA, NA), lty = c(NA, 1, 1), lwd = 2,
  col = c("darkgreen", "darkorange", "darkgreen"),
  bty = "n", cex = 0.8
)