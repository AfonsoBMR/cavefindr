#' Check pointcloud in specific coordinate
#' 
#' @param x Longitude coordinate in same CRS as the LAS file.
#' @param y Latitude coordinate in same CRS as the LAS file.
#' @param las A lidR LAS or LAScatalog object.
#' @param radius Radius to be plotted, in meters. Default value is 10.
#' 
#' @return
#' This function doesn't return any object.
#' 
#' @details
#' Takes the coordinates of any point in the point cloud and plots the terrain at that location.
#' The function is useful, for example, to visualize the terrain in any location of interest, 
#' or to verify whether a set of coordinates correctly refers to a feature. 
#' 
#' @examplesIf FALSE
#' # get data
#' my_catalog <- lidR::readLAScatalog(here::here("data", "LIDAR_tiles"))
#'
#' check_coordinate(
#'   x = -50298.67,
#'   y = -12377.10,
#'   las = my_catalog,
#'   radius = 10
#' )
#'
#' @export
check_coordinate <- function(
  x,
  y,
  las,
  radius = 10
){
  
  # CHECK ARGUMENTS ------------------------------------------------------------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  # unconditional checks
  checkmate::assert_number(x, finite = TRUE, add = coll)
  checkmate::assert_number(y, finite = TRUE, add = coll)
  checkmate::assert(
    checkmate::check_class(las, "LAS"),
    checkmate::check_class(las, "LAScatalog"), 
    add = coll
  )
  checkmate::assert_number(radius, lower = 0, finite = TRUE, add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  # PLOT ------------------------------------------------------------------------------------------------
  
  # define color scheme for rgl plots
  class_colors <- c(
    "1" = "grey", 
    "2" = "black", 
    "3" = "lightgreen",
    "4" = "green",
    "5" = "darkgreen",
    "6" = "red", 
    "7" = "purple", 
    "9" = "blue", 
    "26" = "red"
  )
  
  # get ROI of coordinate
  roi <- lidR::clip_circle( las, x, y, radius = radius) 
  
  # getting class colors
  cols <- unname(class_colors[as.character(roi$Classification)])
  cols[is.na(cols)] <- "gray"
  
  # plot based on point classification
  rgl::open3d()
  rgl::plot3d(
    roi@data$X,
    roi@data$Y,
    roi@data$Z,
    col = cols,
    size = 3,
    xlab = "X",
    ylab = "Y",
    zlab = "Z",
    aspect = "iso"
  )
}
  
