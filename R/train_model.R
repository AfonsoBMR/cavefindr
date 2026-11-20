#' Train model from subsetted and corrected cave database
#' 
#' @param corrected_rois Either the ID (character) of a corrected cave dataset stored in "output", or the corrected ROI object returned directly by correct_coordinates().
#' @param las A lidR LAS or LAScatalog object.
#' @param metrics Vector listing metrics to be used as covariates in model training, or simply "all". Possible metrics are "depth_plane", "depth_mean", "depth_half_radius_mean", "depth_w_mean", "slope", "intensity", "return_number", "R", "G", "B", "NIR".
#' @param plot Logical, default TRUE. Plot distributions of metrics to compare cave and background datasets?
#' @param var_imp Logical, default TRUE. Plot variable importance?
#'
#' @return
#' Random forest model object.
#'
#' @details
#' Uses a curated and corrected cave location database to train a Random Forest to recognise points 
#' associated with cave entrances. It first extracts a set of chosen metrics from the cave locations 
#' and an equal number of random locations in the point cloud. Then, it trains a Random Forest to 
#' discern metric values associated with caves from those of random points (randomForest R package: 
#' ntree = 1000, mtry = 2, nodesize = 5, maxnodes = unrestricted; missing values imputed with 
#' na.roughfix; other settings default). Plots of variable importance and histograms of metric 
#' distributions are automatically provided. Simple models with fewer metrics allow faster 
#' predictions, but might be less powerful: it is recommended to train a model using all metrics, 
#' check variable importances, and then create a simplified model for actual predictive usage.
#' 
#' @examplesIf FALSE
#' # get data
#' my_catalog <- lidR::readLAScatalog(here::here("data", "LIDAR_tiles"))
#'
#' my_model <- train_model(
#'   corrected_rois = '20260810122226',
#'   las = my_catalog,
#'   metrics = c(
#'     "depth_plane",
#'     "depth_mean",
#'     "depth_half_radius_mean",
#'     "depth_w_mean",
#'     "slope",
#'     "intensity",
#'     "NIR"
#'   ),
#'   plot = TRUE,
#'   var_imp = TRUE
#' )
#'
#' @export
train_model <- function(
    corrected_rois,
    las,
    metrics,
    plot = TRUE,
    var_imp = TRUE
) {
  
  # CHECK ARGUMENTS ------------------------------------------------------------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  # unconditional checks
  checkmate::assert(
    checkmate::check_string(corrected_rois, min.chars = 1),
    checkmate::check_list(corrected_rois, types = "LAS", min.len = 1),
    combine = "or",
    add = coll
  )
  checkmate::assert(
    checkmate::check_class(las, "LAS"),
    checkmate::check_class(las, "LAScatalog"), 
    add = coll
  )
  checkmate::assert_character(metrics, min.len = 1, add = coll)
  if (!identical(metrics, "all")) {
    checkmate::assert_subset(
      metrics,
      choices = c(
        "depth_plane",
        "depth_mean",
        "depth_half_radius_mean",
        "depth_w_mean",
        "slope",
        "intensity",
        "return_number",
        "R",
        "G",
        "B",
        "NIR"
      ),
      add = coll
    )
  }
  checkmate::assert_flag(plot, add = coll)
  checkmate::assert_flag(var_imp, add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  # FIRST STEPS ------------------------------------------------------------------------------------------------
  
  # convert "all" metrics to explicit list
  if ( "all" %in% metrics ){ metrics <- c("depth_plane","depth_mean","depth_half_radius_mean","depth_w_mean","slope","intensity","return_number","R","G","B","NIR")}
  
  # check if valid metrics
  missing <- setdiff(
    metrics, 
    c("depth_plane","depth_mean","depth_half_radius_mean","depth_w_mean","slope","intensity","return_number","R","G","B","NIR")
    )
  if (length(missing) > 0) {
    stop("Unknown metrics: ", paste(missing, collapse = ", "))
  }
  
  
  
  ## PREPARE TRAINING DATA ------------------------------------------------------------------------------------------------
  
  # get corrected_rois: either load by id, or use the object directly
  if (checkmate::test_string(corrected_rois)) {
    corrected_id <- corrected_rois
    load( here::here("output", paste0("corrected_cave_dataset_", corrected_id), "cave_rois_corrected.Rdata" ))
    # fetch cave_rois_corrected 
  } else {
    cave_rois_corrected <- corrected_rois
    corrected_id <- attr(cave_rois_corrected, "id")
  }
  
  # get metrics from cave points
  cave_metrics <- lapply( cave_rois_corrected, metrics_from_roi, metrics = metrics)
  cave_metrics <- do.call(rbind, cave_metrics)
  
  # get las footprint as spatvector
  footprints <- if (inherits(las, "LAScatalog")) {
    terra::vect(sf::st_union(sf::st_geometry(las@data)))
  } else {
    terra::vect(sf::st_as_sfc(sf::st_bbox(las)))
  }
  
  # get random background points
  bg_points <- terra::spatSample(footprints, size = length(cave_rois_corrected), method = "random")
  
  # get background ROI's
  bg_rois <- lidR::clip_circle( 
    las, 
    terra::crds(bg_points)[,1], terra::crds(bg_points)[,2],
    radius = attr(cave_rois_corrected, "radius") # recovers same radius as used in correct_coordinates()
    )
  
  # get metrics from backround points
  bg_metrics <- lapply( bg_rois, metrics_from_roi, metrics = metrics)
  bg_metrics <- do.call(rbind, bg_metrics)
  
  # combine all data
  # obtaining dataframe with value pb=1 for cave values and pb=0 for background values
  pb <- c(rep(1, nrow(cave_metrics)), rep(0, nrow(bg_metrics)))
  data <- data.frame(cbind(pb, rbind(cave_metrics, bg_metrics)))
  
  # comparing covariate distributions between cave and backround points
  if (plot == TRUE) {
    data_long <- tidyr::pivot_longer(data, cols = -pb, names_to = "variable", values_to = "value")
    p <- ggplot2::ggplot(data_long, ggplot2::aes(x = value, fill = factor(pb)))+
      ggplot2::geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
      ggplot2::facet_wrap(~ variable, scales = "free") +
      ggplot2::scale_fill_manual(values = c("steelblue", "orange"),
                                 name = "pb",
                                 labels = c("Background", "Caves")) 
    print(p)
  }

  
  
  ## TRAIN MODEL ------------------------------------------------------------------------------------------------
  
  # since tuning results were highly random due to small dataset, ill more or less keep to defaults and allow deep trees
  model <- suppressWarnings( # always gets pb data type warning (regression/classification) 
    randomForest::randomForest( 
      pb~. , 
      data = data, 
      ntree = 1000,
      mtry = 2, nodesize = 5, maxnodes = NULL, # hyperparameters
      na.action=randomForest::na.roughfix,
      importance = TRUE,
      do.trace = FALSE 
      )
    )
  
  # see importance of variables, higher value meaning more important
  if (var_imp == TRUE) {
    tryCatch(
      {
        randomForest::varImpPlot(model, sort = TRUE)
      },
      error = function(e) {
        message("Variable importance calculation not possible. Your random forest was probably trained of too few points.")
      }
    )
  }

  # add metric covariate names as metadata
  attr( model, "metrics") <- metrics
  
  # add radius as metadata
  attr( model, "radius") <- attr(cave_rois_corrected, "radius")
  
  message("Model was trained on ", if (!is.null(corrected_id)) paste0("corrected_rois = '", corrected_id, "'") else "a ROI object with no id", " data.")
  
  if (!is.null(corrected_id)) {
    save( model, file = here::here("output", paste0("corrected_cave_dataset_", corrected_id), "model.Rdata" ))
  } else {
    message("No id was available, so the model was not automatically exported - save it manually with save() if needed.")
  }
  
  # output model
  invisible(model)
}
