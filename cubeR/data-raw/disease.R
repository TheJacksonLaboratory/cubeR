# Load raw data from .csv file
disease <- read.csv("data-raw/disease.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(disease)
