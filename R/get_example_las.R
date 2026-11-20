#' Load example LAS tile
#'
#' Reads the small example LAS tile into memory as, so you can try out the package's functions
#' without any LiDAR data of your own.
#'
#' @return A `lidR::LAS` object.
#'
#' @details
#' The example tile simulates a small (200 x 200 m) LiDAR tile with three
#' narrow, deep depressions that are clearly detectable in the
#' pointcloud, and one shallow depression that isn't. Pair it with
#' [get_example_cave_dataset()] for a matching cave coordinate dataset.
#'
#' @examplesIf FALSE
#' my_las <- get_example_las()
#'
#' @export
get_example_las <- function(){

  las_path <- system.file("extdata", "cave_terrain_example.laz", package = "cavefindr")

  if (las_path == "") {
    stop("Example LAS file not found in the installed package. Try reinstalling cavefindr.")
  }

  lidR::readLAS(las_path)
}
