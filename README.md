# Cube Functions for R Notebook

#### To Install

```
install.packages("devtools")
library("devtools")
devtools::install_github("TheJacksonLaboratory/cubeR/cube")
devtools::install_github("TheJacksonLaboratory/cubeR/qtlviewer")
library(cube)
```

#### 

```
cube_obj <- Cube$new()
testdata <- cube_obj$test()
```

### Cube API call

```
cube_api <- CubeAPI$new()
json <- cube_api$get_disclaimer_json()
```
