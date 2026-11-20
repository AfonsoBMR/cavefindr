#' Correct coordinates on a cave database and prepare a database for model training
#' 
#' @param cave_vect A cave database in terra point SpatVector format.
#' @param las A lidR LAS or LAScatalog object.
#' @param radius Radius to define ROI of each cave, in meters. Default value is 10.
#' @param review Logical, default FALSE. Review corrections?
#'
#' @return
#' Returns the corrected cave ROIs (a list of LAS objects, with "radius" and "id" attributes attached). Also exported to disk.
#'
#' @details
#' Has two purposes: 
#' to correct coordinates on any cave database/cadastre, including altitude; 
#' to prepare a subsetted and location-corrected cave database for model training. 
#' 
#' The function firstly prompts the user to classify each cave as ‘detectable’ or ‘not detectable’ 
#' in the point cloud (i.e., whether the entrance is clearly visible in the LiDAR data) and then 
#' automatically corrects the coordinates of ‘detectable’ caves to exactly match their point cloud 
#' depressions. Altitude is corrected using the mean heights of neighbouring ground points. This 
#' eliminates the GPS error of the coordinates, which can be considerable.
#' 
#' @examplesIf FALSE
#' # get data
#' my_catalog <- lidR::readLAScatalog(here::here("data", "LIDAR_tiles"))
#' my_cave_dataset <- terra::vect(here::here("data", "cave_dataset", "cave_dataset.shp"))
#'
#' my_cave_rois_corrected <- correct_coordinates(
#'   cave_vect = my_cave_dataset,
#'   las = my_catalog,
#'   radius = 10,
#'   review = TRUE
#' )
#'
#' @export
correct_coordinates <- function(
    cave_vect,
    las,
    radius = 10,
    review = FALSE
){
  
  # CHECK ARGUMENTS ------------------------------------------------------------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  # unconditional checks
  checkmate::assert_class(cave_vect, "SpatVector", add = coll)
  checkmate::assert(
    checkmate::check_class(las, "LAS"),
    checkmate::check_class(las, "LAScatalog"), 
    add = coll
  )
  checkmate::assert_number(radius, lower = 0, finite = TRUE, add = coll)
  checkmate::assert_flag(review, add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  # FIRST STEPS ------------------------------------------------------------------------------------------------
  
  # get original crs
  original_crs <- terra::crs(cave_vect)
  
  # reproject cave dataset to same crs as las
  cave_vect <- terra::project( cave_vect, lidR::wkt(las) )
  
  # try to find name of field with cave names
  # if not found becomes empty string
  name_field <- names(cave_vect)[grep("nome|name", names(cave_vect), ignore.case = TRUE)][1]
  if (is.na(name_field)) {name_field <- names(cave_vect)[1]}
  
  # get las footprint as spatvector
  footprints <- terra::vect( sf::st_as_sfc( sf::st_bbox(las) ) )
  
  # filter cave points that are inside LAS footprint
  is_inside_las <- terra::is.related(cave_vect, footprints, relation = "intersects")
  cave_vect_studyArea <- cave_vect[is_inside_las]
  
  message("The cave dataset has ", nrow(cave_vect), " coordinates, of which ", nrow(cave_vect_studyArea), " are inside the LAS.")
  
  # get cave coordinates
  xs <- terra::crds(cave_vect_studyArea)[,1]
  ys <- terra::crds(cave_vect_studyArea)[,2]
  
  terra::plot(footprints, main="Cave dataset") ; graphics::points(xs, ys)
  
  # get cave ROI's (regions of interest)
  cave_rois <- lidR::clip_circle( las, xs, ys, radius = radius)
  
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
  

  
  # MANUAL CLASSIFICATION OF CAVE DATA ------------------------------------------------------------------------------------------------
  # classifying each cave as detectable or non-detectable in the pointcloud

  message(
    "Case-by-case detectability labeling starting...
Minimize R to the left of the screen so you can see the plot window at the same time.
Label each point with 'y' if a cave is detectable in the pointcloud or 'n' if not."
  )
  
  # vector to store decisions
  is_detectable <- rep( NA, length(cave_rois))
  
  for ( i in seq_along(cave_rois)) {
    
    roi <- cave_rois[[i]]
    
    # getting class colors
    cols <- unname(class_colors[as.character(roi$Classification)])
    cols[is.na(cols)] <- "gray"
    
    # plot based on point classification
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

    # change window size
    rgl::par3d(windowRect = c(800, 100, 1520, 800))
    
    # side view
    rgl::view3d(theta = 180, phi = 90)
    
    # query user and register answer
    is_detectable[i] <- readline(prompt = paste("Index", i, cave_vect_studyArea[[name_field]][i,], " --- Detectable? (y/n): "))
    
    # close window
    rgl::close3d()
  }
  
  # convert decision vector to logical
  # all values other than "y" or "Y" are considered FALSE
  is_detectable <- tolower(is_detectable) == "y"
  
  message("Out of ", length(is_detectable), " caves, ", sum(is_detectable), " where classified as detectable in the pointcloud.")
  
  # subset
  cave_rois_curated <- cave_rois[ is_detectable ]
  
  # checkpoint
  if (length(cave_rois_curated) == 0) {
    stop("No caves were classified as detectable, so there's nothing to correct. Check your choices or your data.")
  }
  
  
  
  # COORDINATE AND ROI CORRECTION ------------------------------------------------------------------------------------------------
  # for each cave which is detectable on the pointcloud, get a new ROI with the entrance centered (in the first ROIs, the entrances are not centered due to GPS error)
  # entrance altitude is also estimated from the mean ROI ground points altitude
  
  # get real cave coordinates and altitude, without GPS error, for each ROI
  corrected_coords <- sapply( cave_rois_curated, function(roi){
    
    # get dataframe with point data
    pts <- roi@data[,c("X","Y","Z","Classification")]

    # subset points classified as "ground" (2) and as "noise" (7)
    pts <- pts[ pts$Classification == 2 | pts$Classification == 7, ]
    
    # subset ground points
    ground_pts <- pts[pts$Classification == 2,] 
    
    # fit plane: Z = a + bX + cY
    fit <- stats::lm(Z ~ X + Y, data = ground_pts)
    
    # predicted plane z values
    pts$Z_plane <- stats::predict(fit, newdata = pts[,c("X","Y")] )
    
    # residuals (negative means below plane)
    pts$residual <- pts$Z - pts$Z_plane
    
    # point lowest below plane
    id <- which.min(pts$residual)
    
    # return coordinates of point lowest below plane
    # the third value is corrected entrance altitude, calculated as mean ground points altitude
    c( pts$X[id], pts$Y[id], mean(ground_pts$Z) )
  })
  
  corrected_coords <- t(corrected_coords)
  
  message( nrow(corrected_coords), " corrected cave coordinates were obtained.")
  
  # find all duplicated coordinates, where correction corrected two caves to same point due to proximity
  same_coords <- duplicated(corrected_coords) | duplicated(corrected_coords, fromLast = TRUE) 
  
  # remove them from detectable/curated/coordinate objects, this ensures these corrections are ignored
  is_detectable[is_detectable] <- !same_coords
  cave_rois_curated <- cave_rois_curated[!same_coords]
  corrected_coords <- corrected_coords[!same_coords,]
  
  message( sum(same_coords), " of the above were wrongly corrected due to cave proximity. These were removed from the corrected dataset.")
  
  message("Getting corrected cave ROI's...")
  
  # get corrected cave ROI's
  cave_rois_corrected <- lidR::clip_circle( 
    las,
    corrected_coords[,1], corrected_coords[,2],
    radius = radius
  ) 
  
  # add radius as metadata 
  attr(cave_rois_corrected, "radius") <- radius
  
  # review if correction worked for each cave 
  if (review == TRUE){
    
    message(
"Case-by-case correction review starting...
Minimize R to the left of the screen so you can see the plot window at the same time.
Label each point with 'y' if the correction was sucessfull or 'n' if it was unsucessfull and should be excluded."
    )
    
    # vector to store decisions
    reviewed <- rep( NA, length(cave_rois_corrected))
    
    for ( i in seq_along(cave_rois_corrected)) {
      
      # plot original ROI
      
      roi_curated <- cave_rois_curated[[i]]
      
      # getting class colors
      cols <- unname(class_colors[as.character(roi_curated$Classification)])
      cols[is.na(cols)] <- "gray"
      
      # plot based on point classification
      rgl::open3d()
      rgl::plot3d(
        roi_curated@data$X,
        roi_curated@data$Y,
        roi_curated@data$Z,
        col = cols,
        size = 1.5,
        xlab = "X",
        ylab = "Y",
        zlab = "Z",
        aspect = "iso"
      )
      
      # change window size
      rgl::par3d(windowRect = c(800, 100, 1520, 450))
      
      # side view
      rgl::view3d(theta = 180, phi = 90)
      
      
      
      # plot corrected ROI 
      
      roi_corrected <- cave_rois_corrected[[i]]
      
      # getting class colors
      cols <- unname(class_colors[as.character(roi_corrected$Classification)])
      cols[is.na(cols)] <- "gray"
      
      # plot based on point classification
      rgl::open3d()
      rgl::plot3d(
        roi_corrected@data$X,
        roi_corrected@data$Y,
        roi_corrected@data$Z,
        col = cols,
        size = 1.5,
        xlab = "X",
        ylab = "Y",
        zlab = "Z",
        aspect = "iso"
      )

      # change window size
      rgl::par3d(windowRect = c(800, 450, 1520, 800))
      
      # side view
      rgl::view3d(theta = 180, phi = 90)
      
      # query user and register answer
      reviewed[i] <- readline(prompt = paste("Index", i, " of curated dataset. Is correction fine? (y/n): "))
      
      # close windows
      rgl::close3d()
      rgl::close3d()
    }
    
    # convert decision vector to logical
    # all values other than "y" or "Y" are considered FALSE
    reviewed <- tolower(reviewed) == "y"
    
    # remove negative reviews from detectable/curated/coordinate objects, this ensures these corrections are ignored
    is_detectable[is_detectable] <- reviewed
    cave_rois_curated <- cave_rois_curated[reviewed]
    cave_rois_corrected <- cave_rois_corrected[reviewed]
    corrected_coords <- corrected_coords[reviewed,]
    
    message( sum(!reviewed), " of the above were classified as a defective correction. These were removed from the corrected dataset.")
  }
  
  
  
  # EXPORTING CORRECTED CAVE VECTOR DATASET ------------------------------------------------------------------------------------------------
  # write corrected coordinates and altitudes to cave vector dataset, while preserving all other untouched coordinates and fields
  
  # updating coordinates, only in slots where there was correction (all others remain untouched)
  xy_corrected <- terra::crds(cave_vect)
  xy_corrected[is_inside_las,][is_detectable,] <- corrected_coords[,c(1,2)]
  
  # rebuild SpatVector
  cave_vect_corrected <- terra::vect( xy_corrected, type = "points", crs = terra::crs(cave_vect))
  terra::values(cave_vect_corrected) <- terra::values(cave_vect)

  # add a field with corrected altitudes, only in slots where there was correction
  # this also serves as a flag for corrected points
  cave_vect_corrected$altitude <- rep(NA, nrow(cave_vect_corrected))
  cave_vect_corrected$altitude[is_inside_las][is_detectable] <- round(corrected_coords[,3])
  
  # project back to original crs
  cave_vect_corrected <- terra::project(cave_vect_corrected, original_crs)
  
  # create export directory
  export_id <- format(Sys.time(), "%Y%m%d%H%M%S")
  dir.create( here::here("output", paste0("corrected_cave_dataset_", export_id)), recursive = TRUE)
  
  # export corrected cave dataset
  terra::writeVector( cave_vect_corrected, filename = here::here("output", paste0("corrected_cave_dataset_", export_id), "corrected.shp"))
  
  # attaching id attribute to cave_rois_corrected
  attr(cave_rois_corrected, "id") <- export_id
  
  # attaching radius attribute to cave_rois_corrected
  attr(cave_rois_corrected, "radius") <- radius
  
  # export cave_rois_corrected
  save( cave_rois_corrected, file = here::here("output", paste0("corrected_cave_dataset_", export_id), "cave_rois_corrected.Rdata" ))
  
  message("Corrected cave dataset done and exported. To use it with train_model(), specify corrected_rois = '", export_id, "' or pass the returned object directly.")

  # return
  invisible(cave_rois_corrected)
}