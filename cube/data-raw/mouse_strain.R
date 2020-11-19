# Load raw data from .csv file
mouse_strain <- read.csv("data-raw/mouse_strain.csv")
# Apply preprocessing...
# Save the cleaned data in the required R package location
usethis::use_data(mouse_strain)
