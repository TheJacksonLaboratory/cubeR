# Load raw data from .csv file
metadata_assay <- read.csv("data-raw/metadata_assay.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(metadata_assay)
