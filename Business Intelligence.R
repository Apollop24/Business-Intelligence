# 1. How many visits did Kimathi Street and Pipeline medical centers have from May 2022 to September 2022?
library(readxl)
# Load the data into a data frame
dataP <- read_excel("KU.xlsx")

# Convert the VisitDateTime column to a date format
dataP$VisitDateTime <- as.POSIXct(dataP$VisitDateTime, format = "%d/%m/%Y %H:%M:%S")

# Filter the data to only include visits from May 2022 to September 2022
may_to_september_visits <- subset(dataP, 
                                  VisitDateTime >= as.POSIXct("2022-05-01 00:00:00") &
                                    VisitDateTime <= as.POSIXct("2022-09-30 23:59:59"))

# Filter the data to only include visits at Kimathi Street and Pipeline medical centers
kimathi_pipeline_visits <- subset(may_to_september_visits, MedicalCenter %in% c("Kimathi Street", "Pipeline"))

# Calculate the number of visits at Kimathi Street and Pipeline medical centers
num_visits <- nrow(kimathi_pipeline_visits)

# Print the result
cat(paste0("Kimathi Street and Pipeline medical centers had ", num_visits, " visits from May 2022 to September 2022."))


# 2. What is the most common diagnosis in 2022 for Tassia and Embakasi medical centers?

# Load the data into a data frame
dataM <- read_excel("DI.xlsx")

# Filter the data to only include visits at Tassia and Embakasi medical centers
tassia_embakasi_visits <- subset(dataM, MedicalCenter %in% c("Tassia", "Embakasi"))

# Find the most common diagnosis
most_common_diagnosis <- names(which.max(table(tassia_embakasi_visits$Diagnosis)))

# Print the result
cat(paste0("The most common diagnosis in 2022 for Tassia and Embakasi branches combined is ", most_common_diagnosis, "."))


# 3. How many unique patients experienced a blended healthcare approach in 2022?

# Filter data for the year 2022
patient_data_2022 <- read_excel("KU.xlsx")

# Count unique patients with blended healthcare approach
blended_patients <- unique(patient_data_2022$PatientCode[patient_data_2022$VisitCategory %in% c("In-person Visit", "Telemedicine Visit")])

# Get the count of unique patients
num_blended_patients <- length(blended_patients)

# Print the result
cat("The number of unique patients who experienced a blended healthcare approach in 2022 is:", num_blended_patients)


library(readxl)
KU <- read_excel("KU.xlsx")
head(KU)

# 4. How many unique values are there in the 'PatientCode' column of the 'KU' data frame that are not repeated?

# Assuming you have a data frame called 'data' and 'column_name' represents the column you want to analyze
column_values <- KU$PatientCode

# Count the occurrences of each value in the column
value_counts <- table(column_values)

# Count the number of values that appear only once
unique_values_count <- sum(value_counts == 1)

# Print the total number of unique values that are not repeated
print(paste("Total number of unique values in the 'PatientCode' column that are not repeated:", unique_values_count))

# Print the dimensions of the KU data frame
print(paste("Dimensions of the KU data frame:", dim(KU)))



# 9. What percentage of visits in April 2022 happened within 30 days of the preceding visit by the same patient?

# Load the data into a data frame
dataP <- read_excel("KU.xlsx")

# Convert the VisitDateTime column to a date format
dataP$VisitDateTime <- as.POSIXct(dataP$VisitDateTime, format = "%d/%m/%Y %H:%M:%S")

# Filter the data to only include visits in April 2022
april_visits <- dataP[format(dataP$VisitDateTime, "%Y-%m") == "2022-04", ]

# Calculate the total number of visits in April 2022
total_visits <- nrow(april_visits)

# Create a new column to indicate if the visit happened within 30 days of a preceding visit by the same patient
april_visits$Within30Days <- FALSE

for (i in 1:nrow(april_visits)) {
  patient_code <- april_visits$PatientCode[i]
  visit_date <- april_visits$VisitDateTime[i]
  
  # Find the most recent preceding visit by the same patient
  preceding_visit <- subset(april_visits, PatientCode == patient_code & VisitDateTime < visit_date)
  
  if (nrow(preceding_visit) > 0) {
    most_recent_preceding_visit <- max(preceding_visit$VisitDateTime)
    
    # Check if the visit happened within 30 days of the most recent preceding visit
    if (as.numeric(difftime(visit_date, most_recent_preceding_visit, units = "days")) <= 30) {
      april_visits$Within30Days[i] <- TRUE
    }
  }
}

# Calculate the number of visits within 30 days of a preceding visit by the same patient
visits_within_30_days <- sum(april_visits$Within30Days)

# Calculate the percentage of visits within 30 days of a preceding visit by the same patient
percentage_within_30_days <- (visits_within_30_days / total_visits) * 100

# Print the result
cat(paste0("The percentage of visits in April 2022 that happened within 30 days of the preceding visit by the same patient is ", round(percentage_within_30_days, 2), "%."))
