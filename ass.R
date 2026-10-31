# Get the path of the folder
folder_path <- readline(prompt = "Enter the full folder path in your device: ")
folder_path <- gsub('["\']', '', folder_path)

if(dir.exists(folder_path)){
  setwd(folder_path)
  cat("Working directory successfull set to:", getwd(),"\n")
}else{
  stop("The folder path does not exist. Please check and run again.")
}

clean_students_ids <- function(file_path){
  if(!file.exist(folder_path)){
    warning(paste("File not found:", file.path))
    return(character(0))
  }
  
  raw_data<-readLines(file_path, warn = FALSE)
  
  cleaned <- trimsws(raw_data)
  cleaned <- toupper(cleaned)
  cleaned <- cleaned[cleaned != "" & !is.na(cleaned)]
  
  unique(cleaned)
}

course_files <- list(
  Course1 = "Q1_CCS592.txt",
  Course2 = "Q1_CDS501.txt",
  Course3 = "Q1_CDS506.txt",
  Course4 = "Q1_CDS512.txt",
  Course5 = "Q1_CDS521.txt",
  Course6 = "Q1_CDS523.txt"
)

course_students <- lapply(course_files, clean_student_ids)

course_sizes <- sapply(course_students, length)
cat("\nCourse Enrolment Counts:\n")
print(course_sizes)

# Part b
courses <-names(course_files)
n_courses<-length(courses)

conflict_matrix <- matrix(0, nrow = n_courses, ncol = n_courses, dimnames = list(course, courses))


for(i in 1:n_courses){
  for(j in 1:n_courses){
    if(i==j){
      conflict_matrix[i,j]<-0
    }else{
      shared_students <- length(intersect(course_students[[i]], course_students[[j]]))
      conflict_matrix[i,j]<-shared_students
    }
  }
}

cat("\n--- 6x6 Conflict Matrix (Shared Student Overlaps) ---\n")
print(conflict_matrix)

#part c
slot_capacities <- c(40,70,40,70,40,70,40,70)
names(slot_capacities)<-paste("Slot", 1:8)

get_time_block<-function(slot){
  ceiling(slot/2)
}

schedule_exams_basic <- function(conflict_matrix,course_sizes,slot_capacities){
  assignments <- setNames(rep(NA, n_courses), courses)
  
  course_degrees <- rowSums(conflict_matrix > 0)
  sorted_courses <- names(sort(course_degrees, decreasing =TRUE))
}