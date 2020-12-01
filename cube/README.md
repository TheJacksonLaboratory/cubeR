# Cube Functions for R Notebook
# Cube Custom R Package
This is custom R package for Cube project

## To Set up

```
install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)
install.packages("R6")
library(R6)
install.packages("RPostgreSQL")
library('RPostgreSQL')
```

## install

```
devtools::install()
devtools::document()
usethis::use_vignette("introduction")
```

## run test

```
usethis::use_testthat()
use_test()



snp_data <- read.csv("data-raw/snap_grid_sample.csv")
usethis::use_data(snp_data, overwrite = TRUE)

assay <- read.csv("data-raw/assay.csv")
usethis::use_data(assay, overwrite = TRUE)

disease <- read.csv("data-raw/disease.csv")
usethis::use_data(disease, overwrite = TRUE)

drug_treatment <- read.csv("data-raw/drug_treatment.csv")
usethis::use_data(drug_treatment, overwrite = TRUE)

mouse_strain <- read.csv("data-raw/mouse_strain.csv")
usethis::use_data(mouse_strain, overwrite = TRUE)

mouse <- read.csv("data-raw/mouse.csv")
usethis::use_data(mouse, overwrite = TRUE)

setwd('./cubeR')
document()
usethis::use_vignette("introduction")



install("cubeR")
library(cubeR)

packageVersion("cubeR")

c <- cubeR("assay", c("JAXAS00001", "JAXAS00030", "JAXAS00033"))
c <- cubeR("drug treatment", c("JAXDR00004", "JAXDR00005"))
c$data
c$metadata

```
