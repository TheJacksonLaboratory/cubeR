# Load raw data from .csv file
assay <- read.csv("data-raw/assay.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(assay)
