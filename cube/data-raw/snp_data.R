# Load raw data from .csv file
snp_data <- read.csv("data-raw/snap_grid_sample.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(snp_data)