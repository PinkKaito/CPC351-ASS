# ======================================
# CREATE DUMMY DATASET FOR QUESTION 2
# =======================================

# PART A: Define the 10 delivery points

points <- data.frame(
  Point = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10"),
  
  X = c(60, 180, 80, 140, 20, 100, 200, 140, 40, 100),
  
  Y = c(200, 200, 180, 180, 160, 160, 160, 140, 120, 120)
  
)

print(points)

# PARTR B: Generate normal Rainy Morning records

set.seed(351)

dummy_data <- data.frame()

for(i in 1:10){
  
  for(j in 1:10){
    
    # Do not create travel from a point to itself
    if(i != j){
      
      # Create 4 historical travel times
      travel_times <- sample(15:60, 4, replace = TRUE)
      
      for(k in 1:4){
        
        new_record <- data.frame(
          Origin_X = points$X[i],
          Origin_Y = points$Y[i],
          Dest_X = points$X[j],
          Dest_Y = points$Y[j],
          Weather = "Rainy",
          Time_of_Day = "Morning",
          Travel_Time_Minutes = travel_times[k]
        )
        
        dummy_data <- rbind(dummy_data, new_record)
      }
    }
  }
}

# PART C: Add records that should be filtered out

extra_records <- data.frame(
  
  Origin_X = c(60, 180, 80, 140),
  Origin_Y = c(200, 200, 180, 180),
  Dest_X = c(180, 80, 140, 100),
  Dest_Y = c(200, 180, 180, 160),
  
  Weather = c("Clear", "Storm", "Rainy", "Clear"),
  Time_of_Day = c("Morning", "Morning", "Afternoon", "Evening"),
  Travel_Time_Minutes = c(20, 45, 35, 28)
)

dummy_data <- rbind(dummy_data, extra_records)

# PART D: Add NA and -1 road closure records

closure_records <- data.frame(
  
  Origin_X = c(60,60),
  Origin_Y = c(200,200),
  Dest_X = c(180,180),
  Dest_Y = c(200,200),
  
  Weather = c("Rainy", "Rainy"),
  Time_of_Day = c("Morning", "Morning"),
  Travel_Time_Minutes = c(NA,-1)
)

dummy_data <- rbind(dummy_data, closure_records)

# PART E: Make one route completely blocked
# P3 (80,180)-> P7(200,160)
# First remove the normal P3 -> P7 Rainy Morning records

dummy_data <- dummy_data[
  !(
    dummy_data$Origin_X == 80 &
      dummy_data$Origin_Y == 180 &
      dummy_data$Dest_X == 200 &
      dummy_data$Dest_Y == 160 &
      dummy_data$Weather == "Rainy" &
      dummy_data$Time_of_Day == "Morning"
  ),
]

# Add only blocked records for P3 -> P7

blocked_route <- data.frame(
  
  Origin_X = c(80, 80, 80),
  Origin_Y = c(180, 180, 180),
  Dest_X = c(200, 200, 200),
  Dest_Y = c(160, 160, 160),
  
  Weather = c("Rainy", "Rainy", "Rainy"),
  Time_of_Day = c("Morning", "Morning", "Morning"),
  Travel_Time_Minutes = c(NA, -1, NA)
)

dummy_data <- rbind(dummy_data, blocked_route)

# PART F: Check the dummy dataset

cat("\n--- Dummy Dataset Summary --- \n")

print(summary(dummy_data))

cat("\nTotal number of records:", nrow(dummy_data), "\n")

head(dummy_data)

# PART G: Save as CSV file

write.csv(
  dummy_data,
  "ahmad_travel_logs.csv",
  row.names = FALSE,
  na = "NA"
)

cat("\nDummy dataset created successfully!\n")