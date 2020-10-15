# Load raw data from .csv file
mice <- read.csv("data-raw/mice.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(mice)
