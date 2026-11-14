# Run from the project root: Rscript tests/test_Q1.R
original_folder <- getwd()
invisible(capture.output(source("ass.R")))
stopifnot(identical(getwd(), original_folder),
          identical(as.integer(extended_schedule), c(1L, 2L, 3L, 6L, 8L, 9L)),
          identical(as.integer(improved_schedule), c(1L, 2L, 5L, 10L, 14L, 17L)))
cat("PASS: sample timetables unchanged; working directory preserved\n")

expect_failure <- function(assignments, students, message,
                           strict = FALSE, capacities = rep(c(40, 70), 4)) {
  error <- tryCatch({
    validate_timetable(assignments, capacities, students, strict)
    "No error"
  }, error = function(e) conditionMessage(e))
  stopifnot(grepl(message, error, fixed = TRUE))
}

shared <- list(A = "NAME1", B = "NAME1")
separate <- list(A = "NAME1", B = "NAME2")
stopifnot(validate_timetable(c(A = 1, B = 2), rep(c(40, 70), 4), separate),
          validate_timetable(c(A = 1, B = 3), rep(c(40, 70), 4), shared),
          validate_timetable(c(A = 1, B = 5), rep(c(40, 70), 4), shared, TRUE))

expect_failure(c(A = 1), shared, "every course")
expect_failure(c(A = 1, A = 5), shared, "every course")
for (bad_slot in c(NA, Inf, 0, 1.5, 17)) {
  expect_failure(c(A = 1, B = bad_slot), shared, "Slots must")
}
expect_failure(c(A = 1, B = 1), separate, "Duplicate venue-slot")
expect_failure(c(A = 1, B = 2), shared, "Concurrent examinations")
expect_failure(c(A = 1, B = 3), shared, "Back-to-back", strict = TRUE)
expect_failure(c(A = 1, B = 5), list(A = paste0("NAME", 1:41), B = "NAME99"),
               "Room capacity")
cat("PASS: missing courses, invalid slots, room collisions, capacity and student clashes rejected\n")

# Even an incorrect conflict matrix cannot hide a clash from the validator.
conflict_matrix[,] <- 0
expect_failure(c(A = 1, B = 2), shared, "Concurrent examinations")
cat("PASS: validation uses student lists independently of the scheduling matrix\n")
cat("All Question 1 checks passed.\n")
