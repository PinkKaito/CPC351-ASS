
# ==== PART A: Select the folder containing the six registration files ====
folder_path <- "Q1"

if (!dir.exists(folder_path)) {
  stop("Q1 folder not found.")
}

setwd(folder_path)

cat("Working directory:", getwd(), "\n")

# Display the files in the selected folder
print(list.files())

# Read and clean student IDs from one file
clean_students_ids <- function(file_path) {
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  
  raw_data <- readLines(file_path, warn = FALSE)
  
  # Standardize how each student ID is written
  cleaned <- trimws(raw_data)
  cleaned <- toupper(cleaned)
  cleaned <- gsub("[ _-]", "", cleaned)
  
  # Treat NEMA as a typo for NAME (assumption for this dataset)
  cleaned <- sub("^NEMA", "NAME", cleaned)
  
  # Example: NAME003 and NAME03 both become NAME3
  cleaned <- sub("^NAME0+", "NAME", cleaned)
  
  # Remove missing IDs, blank lines and duplicate registrations
  cleaned <- cleaned[!is.na(cleaned) & cleaned != ""]
  return(unique(cleaned))
}

# Keep the original course names to minimize changes to teammate's code
course_files <- list(
  Course1 = "Q1_CCS592.txt",
  Course2 = "Q1_CDS501.txt",
  Course3 = "Q1_CDS506.txt",
  Course4 = "Q1_CDS512.txt",
  Course5 = "Q1_CDS521.txt",
  Course6 = "Q1_CDS523.txt"
)

course_students <- lapply(course_files, clean_students_ids)
course_sizes <- sapply(course_students, length)

#==== PART B: Generate the 6 x 6 conflict matrix ====
courses <- names(course_files)
n_courses <- length(courses)

conflict_matrix <- matrix(0,
                          nrow = n_courses,
                          ncol = n_courses,
                          dimnames = list(courses, courses))

# Count students shared by every pair of different courses
for (i in 1:n_courses) {
  for (j in 1:n_courses) {
    if (i == j) {
      conflict_matrix[i, j] <- 0
    } else {
      shared_students <- length(
        intersect(course_students[[i]], course_students[[j]])
      )
      conflict_matrix[i, j] <- shared_students
    }
  }
}

cat("\nCourse Enrolment Counts:\n")
print(course_sizes)

cat("\n--- 6x6 Conflict Matrix ---\n")
print(conflict_matrix)


#==== PART C: Timetable Generation ====

# Original we have 8 slots (2 days)
# Venue 1 = 40 students, Venue 2 = 70 students
slot_capacities <- c(40,70,40,70,40,70,40,70)

# Find the time block for each slot where
# Slots 1 and 2 happen at the same time 
get_time_block <- function(slot) { 
  return (ceiling(slot/2))
}

# Function to generate examination timetable
schedule_exams_basic <- function(conflict_matrix,
                                 course_sizes,
                                 slot_capacities) {
  n_courses <- length(course_sizes)
  
  # Initially, no course has been assigned
  assignments <- rep(NA, n_courses)
  names(assignments) <- names(course_sizes)
  
  # Go thorugh each course
  for (i in 1:n_courses) {
    
    placed <- FALSE
    
    # Try each available slot ]
    for (slot in 1:length(slot_capacities)){
      valid <- TRUE
      
      # Check Venue Capacity
      if (course_sizes[i] > slot_capacities[slot]) {
        valid <- FALSE
      }
      
      # Check previously assigned course
      for (j in 1:n_courses){
        if (!is.na(assignments[j])) {
          # Check is the slot already occupied
          if (assignments[j] == slot) {
            valid <- FALSE
          }
          
          # Check concurrent examination conflicts
          if (conflict_matrix[i,j] > 0) {
            if (get_time_block(assignments[j]) == get_time_block(slot)) {
              valid <- FALSE
            }
          }
        }
      }
      
      
      # Assign the course if all checks pass
      if (valid) {
        assignments[i] <- slot
        placed <- TRUE
        break
      }
    }
    
    # If no suitable slot is found 
    if (!placed) {
      return(NULL)
    }
  }
  
  return(assignments)
}

# Check the original 8 slots
basic_schedule <- schedule_exams_basic(
  conflict_matrix,
  course_sizes,
  slot_capacities
)

cat("\n--- Original 8 slots ---\n")

if(is.null(basic_schedule)) {
  cat("The original 8 slots are insufficient.\n")
} else {
  print(basic_schedule)
}

# Generate a workable extended timetable 
# If the original 8 slots are insufficient
# Start with 3 days and increase if necessary
if(is.null(basic_schedule)) {
  extended_schedule <- NULL
  
  for (days in 3:n_courses) {
    # Two session per day, two venues per session
    extended_capacities <- rep(c(40,70), 2 * days)
    
    extended_schedule <- schedule_exams_basic(
      conflict_matrix,
      course_sizes,
      extended_capacities
    )
    
    if (!is.null(extended_schedule)) {
      break
    }
  }
} else {
  extended_schedule <- basic_schedule
}


# Display the generated timetable
if (!is.null(extended_schedule)) {
  
  timetable <- data.frame(
    Course = names(course_sizes),
    Students = as.integer(course_sizes),
    Slot = as.integer(extended_schedule),
    
    Day = (extended_schedule - 1) %/% 4 + 1,
    
    Session = ifelse(
      (extended_schedule - 1) %% 4 < 2,
      "Morning", "Afternoon"
    ),
    
    Venue = ifelse(
      extended_schedule %% 2 == 1,
      "Venue 1", "Venue 2"
    ),
    
    row.names = NULL
  )
  
  cat("\n--- Part C: Workable Timetable ---\n")
  print(timetable)
  
} else {
  cat("No workable timetable was found.\n")
}

# ==== PART D: Back-to-Back Examination Constraint ====

# Each day has 4 slots:
# Morning: Venue 1 and Venue 2
# Afternoon: Venue 1 and Venue 2

get_day <- function(slot) {
  return((slot - 1) %/% 4 + 1)
}

get_session <- function(slot) {
  if ((slot - 1) %% 4 < 2) {
    return("Morning")
  } else {
    return("Afternoon")
  }
}

# Generate timetable using backtracking
schedule_exams_improved <- function(conflict_matrix,
                                    course_sizes,
                                    slot_capacities) {

  n_courses <- length(course_sizes)

  # NA means the course has not been assigned
  assignments <- rep(NA_integer_, n_courses)
  names(assignments) <- names(course_sizes)

  # Check if assigning a course to a slot is valid
  is_valid <- function(i, slot) {

    # Constraint 1: Venue capacity
    if (course_sizes[i] > slot_capacities[slot]) {
      return(FALSE)
    }

    for (j in seq_len(n_courses)) {

      # Skip courses that are not assigned yet
      if (is.na(assignments[j])) {
        next
      }

      # Constraint 2: Slot must be unoccupied
      if (assignments[j] == slot) {
        return(FALSE)
      }

      # Only check students shared by both courses
      if (conflict_matrix[i, j] > 0) {

        # Constraint 3: No concurrent exams
        if (get_time_block(assignments[j]) ==
            get_time_block(slot)) {
          return(FALSE)
        }

        # Constraint 4: No back-to-back exams
        same_day <- get_day(assignments[j]) ==
                    get_day(slot)

        different_session <-
          get_session(assignments[j]) !=
          get_session(slot)

        if (same_day && different_session) {
          return(FALSE)
        }
      }
    }

    return(TRUE)
  }

  # Recursive backtracking function
  backtrack <- function(i) {

    # Base case: All courses successfully assigned
    if (i > n_courses) {
      return(TRUE)
    }

    # Try every available slot
    for (slot in seq_along(slot_capacities)) {

      if (is_valid(i, slot)) {

        # Assign the course
        assignments[i] <<- slot

        # Recursively assign the next course
        if (backtrack(i + 1)) {
          return(TRUE)
        }

        # Backtrack: Undo the assignment
        assignments[i] <<- NA_integer_
      }
    }

    # No valid assignment found
    return(FALSE)
  }

  if (backtrack(1)) {
    return(assignments)
  }

  return(NULL)
}


# ==== Generate Improved Timetable ====

improved_schedule <- NULL

# Start with 2 days and extend if necessary
for (days in 2:n_courses) {

  # 4 slots per day, alternating venue capacities
  improved_capacities <- rep(c(40, 70), 2 * days)

  improved_schedule <- schedule_exams_improved(
    conflict_matrix,
    course_sizes,
    improved_capacities
  )

  if (!is.null(improved_schedule)) {
    cat("\nValid schedule found using",
        days, "days.\n")
    break
  }
}


# ==== Display Improved Timetable ====

if (!is.null(improved_schedule)) {

  improved_timetable <- data.frame(
    Course = names(course_sizes),
    Students = as.integer(course_sizes),
    Slot = as.integer(improved_schedule),

    Day = get_day(improved_schedule),

    Session = sapply(
      improved_schedule,
      get_session,
      USE.NAMES = FALSE
    ),

    Venue = ifelse(
      improved_schedule %% 2 == 1,
      "Venue 1",
      "Venue 2"
    ),

    row.names = NULL
  )

  cat("\n--- Part D: Improved Timetable ---\n")
  print(improved_timetable)

} else {

  cat("\nNo valid timetable found.\n")

}


# ==== Final Validation ====

if (!is.null(improved_schedule)) {

  valid_schedule <- TRUE

  for (i in seq_len(n_courses)) {

    # Verify room capacity
    if (course_sizes[i] >
        improved_capacities[improved_schedule[i]]) {
      valid_schedule <- FALSE
    }

    if (i < n_courses) {
      for (j in seq.int(i + 1, n_courses)) {

        # Verify no same-slot room collision
        if (improved_schedule[i] ==
            improved_schedule[j]) {
          valid_schedule <- FALSE
        }

        if (conflict_matrix[i, j] > 0) {

          # Verify no concurrent examinations
          if (get_time_block(improved_schedule[i]) ==
              get_time_block(improved_schedule[j])) {
            valid_schedule <- FALSE
          }

          # Verify no back-to-back examinations
          if (get_day(improved_schedule[i]) ==
              get_day(improved_schedule[j]) &&
              get_session(improved_schedule[i]) !=
              get_session(improved_schedule[j])) {
            valid_schedule <- FALSE
          }
        }
      }
    }
  }

  if (valid_schedule) {
    cat("\nValidation PASSED:\n")
    cat("- No concurrent examinations\n")
    cat("- No back-to-back examinations\n")
    cat("- No venue capacity violations\n")
    cat("- No duplicate venue-slot assignments\n")
  } else {
    stop("Validation FAILED: Timetable violates constraints.")
  }
}