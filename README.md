# Medical Center Analysis

This repository contains scripts for analyzing medical center visit data. The analysis is focused on several key questions related to patient visits and diagnoses at various medical centers from May 2022 to September 2022. The analyses are performed using R with the `readxl` package for reading Excel files and base R functions for data manipulation and calculation.

## Table of Contents
1. [How many visits did Kimathi Street and Pipeline medical centers have from May 2022 to September 2022?](#1-how-many-visits-did-kimathi-street-and-pipeline-medical-centers-have-from-may-2022-to-september-2022)
2. [What is the most common diagnosis in 2022 for Tassia and Embakasi medical centers?](#2-what-is-the-most-common-diagnosis-in-2022-for-tassia-and-embakasi-medical-centers)
3. [How many unique patients experienced a blended healthcare approach in 2022?](#3-how-many-unique-patients-experienced-a-blended-healthcare-approach-in-2022)
4. [How many unique values are there in the 'PatientCode' column of the 'KU' data frame that are not repeated?](#4-how-many-unique-values-are-there-in-the-patientcode-column-of-the-ku-data-frame-that-are-not-repeated)
5. [What percentage of visits in April 2022 happened within 30 days of the preceding visit by the same patient?](#5-what-percentage-of-visits-in-april-2022-happened-within-30-days-of-the-preceding-visit-by-the-same-patient)

## Requirements
- R
- `readxl` package

Install the required package by running:
```R
install.packages("readxl")
```

## 1. How many visits did Kimathi Street and Pipeline medical centers have from May 2022 to September 2022?

The following code calculates the number of visits to Kimathi Street and Pipeline medical centers between May 2022 and September 2022.

```R
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
```

## 2. What is the most common diagnosis in 2022 for Tassia and Embakasi medical centers?

This code identifies the most common diagnosis at Tassia and Embakasi medical centers in 2022.

```R
# Load the data into a data frame
dataM <- read_excel("DI.xlsx")

# Filter the data to only include visits at Tassia and Embakasi medical centers
tassia_embakasi_visits <- subset(dataM, MedicalCenter %in% c("Tassia", "Embakasi"))

# Find the most common diagnosis
most_common_diagnosis <- names(which.max(table(tassia_embakasi_visits$Diagnosis)))

# Print the result
cat(paste0("The most common diagnosis in 2022 for Tassia and Embakasi branches combined is ", most_common_diagnosis, "."))
```

## 3. How many unique patients experienced a blended healthcare approach in 2022?

This code calculates the number of unique patients who experienced a blended healthcare approach (both in-person and telemedicine visits) in 2022.

```R
# Filter data for the year 2022
patient_data_2022 <- read_excel("KU.xlsx")

# Count unique patients with blended healthcare approach
blended_patients <- unique(patient_data_2022$PatientCode[patient_data_2022$VisitCategory %in% c("In-person Visit", "Telemedicine Visit")])

# Get the count of unique patients
num_blended_patients <- length(blended_patients)

# Print the result
cat("The number of unique patients who experienced a blended healthcare approach in 2022 is:", num_blended_patients)
```

## 4. How many unique values are there in the 'PatientCode' column of the 'KU' data frame that are not repeated?

This code counts the number of unique 'PatientCode' values in the 'KU' data frame that appear only once.

```R
# Load the KU data
KU <- read_excel("KU.xlsx")

# Count the occurrences of each value in the column
value_counts <- table(KU$PatientCode)

# Count the number of values that appear only once
unique_values_count <- sum(value_counts == 1)

# Print the total number of unique values that are not repeated
print(paste("Total number of unique values in the 'PatientCode' column that are not repeated:", unique_values_count))

# Print the dimensions of the KU data frame
print(paste("Dimensions of the KU data frame:", dim(KU)))
```

## 5. What percentage of visits in April 2022 happened within 30 days of the preceding visit by the same patient?

This code calculates the percentage of visits in April 2022 that occurred within 30 days of a preceding visit by the same patient.

```R
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
```

## Conclusion
This repository provides scripts to analyze various aspects of medical center visit data. Each script addresses a specific question, ranging from visit counts and common diagnoses to unique patient analyses and visit patterns. The methodologies are designed to be clear and easily reproducible for other analysts and data scientists.
