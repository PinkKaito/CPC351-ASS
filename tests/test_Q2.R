# Run from the project folder: Rscript tests/test_Q2.R
# Base R only. Each case uses a temporary CSV, never the project's CSV.
q2_script <- normalizePath("Q2.R", mustWork = TRUE)

run_case <- function(data) {
  original_folder <- getwd()
  test_folder <- tempfile("q2-test-")
  dir.create(test_folder)
  on.exit({
    setwd(original_folder)
    unlink(test_folder, recursive = TRUE)
  })
  setwd(test_folder)
  write.csv(data, "ahmad_travel_logs.csv", row.names = FALSE)
  result <- new.env(parent = globalenv())
  # Keep successful test output concise; errors still propagate.
  invisible(capture.output(sys.source(q2_script, envir = result)))
  stopifnot(all(dim(result$cost_matrix) == c(10, 10)),
            all(is.finite(result$cost_matrix)),
            all(diag(result$cost_matrix) == 0),
            length(result$route) == 11,
            all(sort(result$route[1:10]) == 1:10),
            result$route[11] == result$route[1])
  leg_total <- 0
  for (i in 1:10) {
    leg_total <- leg_total + result$cost_matrix[result$route[i],
                                               result$route[i + 1]]
  }
  stopifnot(isTRUE(all.equal(result$total_time, leg_total)))
  return(result)
}

# Full directed history, with four known observations for each route.
x <- c(60, 180, 80, 140, 20, 100, 200, 140, 40, 100)
y <- c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
history <- data.frame()
for (i in 1:10) {
  for (j in 1:10) {
    if (i != j) {
      history <- rbind(history, data.frame(
        Origin_X = x[i], Origin_Y = y[i], Dest_X = x[j], Dest_Y = y[j],
        Weather = "Rainy", Time_of_Day = "Morning",
        Travel_Time_Minutes = c(10, 20, 30, 40)
      ))
    }
  }
}

baseline <- run_case(history)
stopifnot(all(baseline$cost_matrix[row(baseline$cost_matrix) !=
                                  col(baseline$cost_matrix)] == 32.5))

# Invalid times must not change a known percentile or the penalty.
invalid <- history[rep(1, 6), ]
invalid$Travel_Time_Minutes <- c(NA, NaN, Inf, -Inf, -1, 0)
result <- run_case(rbind(history, invalid))
stopifnot(identical(result$cost_matrix, baseline$cost_matrix),
          result$penalty == baseline$penalty,
          nrow(result$clean_data) == nrow(history))
cat("PASS: non-finite and nonpositive times removed; known percentile correct\n")

for (condition in c("no matching conditions", "empty CSV", "all closures")) {
  data <- history
  if (condition == "no matching conditions") data$Weather <- "Clear"
  if (condition == "empty CSV") data <- data[FALSE, ]
  if (condition == "all closures") {
    data$Travel_Time_Minutes <- rep(c(NA, -1), length.out = nrow(data))
  }
  result <- run_case(data)
  stopifnot(nrow(result$clean_data) == 0, result$blocked_routes == 90,
            result$penalty == 999999, result$total_time == 10 * 999999)
  cat("PASS:", condition, "\n")
}

# P1 -> P2 has no history; the reverse direction must stay valid.
missing_route <- history[!(history$Origin_X == x[1] &
                           history$Origin_Y == y[1] &
                           history$Dest_X == x[2] &
                           history$Dest_Y == y[2]), ]
result <- run_case(missing_route)
stopifnot(result$blocked_routes == 1,
          result$cost_matrix[1, 2] == result$penalty,
          result$cost_matrix[2, 1] == 32.5)
cat("PASS: missing directed route penalized independently\n")

set.seed(351)
result <- run_case(history[sample(nrow(history)), ])
stopifnot(identical(result$cost_matrix, baseline$cost_matrix),
          identical(result$route, baseline$route),
          result$total_time == baseline$total_time)
cat("PASS: shuffled rows preserve matrix, route and total\n")

# All costs tie: which.min selects the first unvisited point.
stopifnot(all(baseline$route == c(1:10, 1)),
          baseline$total_time == 325)
cat("PASS: ties choose the first unvisited point consistently\n")

# Force a valid forward chain but a blocked P10 -> P1 return.
chain <- history[FALSE, ]
for (i in 1:9) {
  chain <- rbind(chain, data.frame(
    Origin_X = x[i], Origin_Y = y[i], Dest_X = x[i + 1], Dest_Y = y[i + 1],
    Weather = "Rainy", Time_of_Day = "Morning", Travel_Time_Minutes = 10
  ))
}
closure <- history[1:2, ]
closure$Origin_X <- x[10]
closure$Origin_Y <- y[10]
closure$Dest_X <- x[1]
closure$Dest_Y <- y[1]
closure$Travel_Time_Minutes <- c(NA, -1)
result <- run_case(rbind(chain, closure))
stopifnot(all(result$route == c(1:10, 1)),
          result$cost_matrix[10, 1] == result$penalty,
          result$total_time == 90 + result$penalty)
cat("PASS: blocked return leg is included using its penalty\n")

error <- tryCatch({
  run_case(history[, !names(history) %in% c("Weather", "Dest_X")])
  NULL
}, error = function(e) conditionMessage(e))
stopifnot(identical(error, "Missing required CSV columns: Dest_X, Weather"))
cat("PASS: missing required columns produce a clear error\n")
cat("All Question 2 checks passed.\n")
