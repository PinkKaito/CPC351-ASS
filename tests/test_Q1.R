# Run from the project root: Rscript tests/test_Q1.R
original_folder <- getwd()
invisible(capture.output(source("ass.R")))
stopifnot(identical(getwd(), original_folder),
          identical(as.integer(extended_schedule), c(1L, 2L, 3L, 6L, 8L, 9L)),
          identical(as.integer(improved_schedule), c(1L, 2L, 5L, 10L, 14L, 17L)))
cat("PASS: sample timetables unchanged; working directory preserved\n")

expected_sizes <- c(Course1 = 38L, Course2 = 54L, Course3 = 38L,
                    Course4 = 64L, Course5 = 65L, Course6 = 6L)
expected_conflicts <- matrix(c(
  0L, 0L, 1L, 1L, 14L, 1L,
  0L, 0L, 1L, 22L, 9L, 0L,
  1L, 1L, 0L, 22L, 23L, 2L,
  1L, 22L, 22L, 0L, 18L, 1L,
  14L, 9L, 23L, 18L, 0L, 1L,
  1L, 0L, 2L, 1L, 1L, 0L
), nrow = 6L, byrow = TRUE,
dimnames = list(names(expected_sizes), names(expected_sizes)))

stopifnot(identical(as.integer(course_sizes), as.integer(expected_sizes)),
          "NAME45" %in% course_students$Course1,
          "NAME9" %in% course_students$Course4,
          sum(course_students$Course5 == "NAME29") == 1L,
          isTRUE(all.equal(conflict_matrix, expected_conflicts)),
          isSymmetric(conflict_matrix),
          all(diag(conflict_matrix) == 0L),
          is.null(basic_schedule),
          max(get_day(extended_schedule)) == 3L)

clique_courses <- c("Course1", "Course3", "Course4", "Course5", "Course6")
clique_conflicts <- conflict_matrix[clique_courses, clique_courses]
stopifnot(all(clique_conflicts[upper.tri(clique_conflicts)] > 0L))
cat("PASS: cleaning, exact conflict matrix, five-course clique and two-day insufficiency verified\n")

expect_failure <- function(assignments, students, message,
                           strict = FALSE, capacities = rep(c(40, 70), 4)) {
  error <- tryCatch({
    validate_timetable(assignments, capacities, students, strict)
    "No error"
  }, error = function(e) conditionMessage(e))
  stopifnot(grepl(message, error, fixed = TRUE))
  invisible(error)
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
duplicate_error <- expect_failure(c(A = 1, B = 1), separate, "Duplicate venue-slot")
concurrent_error <- expect_failure(c(A = 1, B = 2), shared, "Concurrent examinations")
back_to_back_error <- expect_failure(c(A = 1, B = 3), shared,
                                     "Back-to-back", strict = TRUE)
capacity_error <- expect_failure(c(A = 1, B = 5),
                                 list(A = paste0("NAME", 1:41), B = "NAME99"),
                                 "Room capacity")
stopifnot(grepl("Slot 1", duplicate_error, fixed = TRUE),
          grepl("Slots 1 and 2", concurrent_error, fixed = TRUE),
          grepl("Slots 1 and 3", back_to_back_error, fixed = TRUE),
          grepl("Slot 1", capacity_error, fixed = TRUE))
cat("PASS: missing courses, invalid slots, room collisions, capacity and student clashes rejected\n")

# Even an incorrect conflict matrix cannot hide a clash from the validator.
conflict_matrix[,] <- 0
expect_failure(c(A = 1, B = 2), shared, "Concurrent examinations")
cat("PASS: validation uses student lists independently of the scheduling matrix\n")
cat("All Question 1 checks passed.\n")
