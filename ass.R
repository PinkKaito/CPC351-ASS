
# ==== PART A: Select the folder containing the six registration files ====
# choose.dir() works in R on Windows.
folder_path <- choose.dir(caption = "Select the Q1 folder")

if (is.na(folder_path) || !dir.exists(folder_path)) {
  stop("No valid Q1 folder was selected.")
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

# Find the examination day for each slot
# Divide by 4 is because each day contains fout slots
# Two venues in the morning & two venues in the afternoon
get_day <- function(slot) {
  return(ceiling(slot/4))
}

# Generate timetable without back-to-back examinations
schedule_exams_improved <- function(conflict_matrix,
                                    course_sizes,
                                    slot_capacities) {
  n_courses <- length(course_sizes)
  
  # Initially, no course has been assigned
  assignments <- rep(NA, n_courses)
  names(assignments) <- names(course_sizes)
  
  # Go through each course
  for (i in 1:n_courses) {
    placed <- FALSE
    
    # Try each available slot
    for (slot in 1:length(slot_capacities)) {
      valid <- TRUE
      
      # Check venue availability
      if (course_sizes[i] > slot_capacities[slot]) {
        valid <- FALSE
      }
      
      # Check previously assigned courses
      for (j in 1:n_courses) {
        if (!is.na(assignments[j])) {
          
          # Check whether slot is already occupied
          if (assignments[j] == slot) {
            valid <- FALSE
          }
          
          # Check courses with overlapping students
          if (conflict_matrix[i, j] > 0) {
            # Check concurrent examinations
            if (get_time_block(assignments[j]) == get_time_block(slot)) {
              valid <- false
            }
            
            # Check Back-to-back examinations
            # Shared students cannot have exams
            # on the same day
            if (get_day(assignments[j]) == get_day(slot)) {
              valid <- FALSE
            }
          }
        }
      }
      
      # Assign course if all checks pass
      if (valid) {
        assignments[i] <- slot
        placed <- TRUE
        break
      } 
    }
    
    # Return NULL if not suitable slot found
    if (!placed) {
      return (NULL)
    }
  }
  
  return(assignments)
}

# Generate the improved timetable
improved_schedule <- NULL

# Try 2 days, then add more days if necessary
for (days in 2:n_courses) {
  # 4 slots per day
  improved_capacities <- rep(c(40, 70), 2 * days)
  
  improved_schedule <- schedule_exams_improved(
    conflict_matrix,
    course_sizes,
    improved_capacities
  )
  
  # Stop when a valid schedule is found
  if (!is.null(improved_schedule)) {
    break
  }
}

# Display improved timetable
if (!is.null(improved_schedule)) {
  
  improved_timetable <- data.frame(
    Course = names(course_sizes),
    Students = as.integer(course_sizes),
    Slot = as.integer(improved_schedule),
    
    Day = (improved_schedule - 1) %/% 4 + 1,
    
    Session = ifelse(
      (improved_schedule - 1) %% 4 < 2,
      "Morning", "Afternoon"
    ),
    
    Venue = ifelse(
      improved_schedule %% 2 == 1,
      "Venue 1", "Venue 2"
    ),
    
    row.names = NULL
  )
  
  cat("\n--- Part D: Improved Timetable ---\n")
  print(improved_timetable)
  
} else {
  cat("No workable timetable was found.\n")
}