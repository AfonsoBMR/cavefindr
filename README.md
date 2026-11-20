# cavefindr

Tools to locate, correct, and predict cave entrance locations from airborne LiDAR (LAS/LAZ) point clouds. Provides functions to inspect point cloud coordinates, correct GPS error in a known cave database using ground-plane geometry, train a random forest classifier on curated cave regions of interest, and generate new candidate cave entrance predictions (via simple thresholds or the trained model) with optional interactive case-by-case review.

## Installation

cavefindr depends on [lidR](https://github.com/r-lidar/lidR), which is distributed through [r-universe](https://r-lidar.r-universe.dev). The following options ensure R finds it.

Run these lines in R:

```r
options(repos = c(
  rlidar = "https://r-lidar.r-universe.dev",
  CRAN   = "https://cloud.r-project.org"
))

install.packages("remotes")

# Option 1: install from GitHub
remotes::install_github("AfonsoBMR/cavefindr")

# Option 2: install from the downloaded package file
# replace the path with the location of the downloaded package
remotes::install_local("path/to/cavefindr_1.0.0.zip") 
```

## Example and usage

The package comes with a small example point cloud and cave dataset so you can try the full workflow without any data of your own. The workflow is exemplified in a vignette, available at https://afonsobmr.github.io/cavefindr/ or https://espeleopt.com/cavefindr.php

If you use your actual point cloud, know that the package expects a 
'Classification' field in the point cloud files. The functions 
only process 'Ground' and 'Noise (Low Point)' points, and assume they 
are classified as '2' and '7', respectively. 

This version of cavefindr was tested against the following package versions:
lidR 4.3.4
terra 1.9-50
sf 1.1-3
rgl 1.3.36
randomForest 4.7-1.2	
dbscan 1.3.0	
FNN 1.1.4.1
maptiles 0.13.0
ggplot2	4.0.4
tidyr 1.3.2
checkmate 2.3.4
here 1.0.2