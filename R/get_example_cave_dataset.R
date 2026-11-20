#' Load example cave dataset
#'
#' Reads the small example cave coordinate dataset into memory, so you can 
#' try out the package's workflow without any cave data of your own.
#'
#' @return A `terra::SpatVector` object.
#'
#' @details
#' The dataset has 5 points: the reported location of 3 caves that are 
#' clearly detectable in the pointcloud, one point on a shallow depression 
#' that isn't detectable, and one point that falls entirely outside the LAS 
#' tile. All with simulated GPS error.
#'
#' @examplesIf FALSE
#' my_cave_dataset <- get_example_cave_dataset()
#'
#' @export
get_example_cave_dataset <- function(){

  shp_path <- system.file("extdata", "cave_dataset_example", "cave_dataset_example.shp", package = "cavefindr")

  if (shp_path == "") {
    stop("Example cave dataset not found in the installed package. Try reinstalling cavefindr.")
  }

  terra::vect(shp_path)
}
