# Load raw data from .csv file
drug_treatment <- read.csv("data-raw/drug_treatment.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(drug_treatment)
