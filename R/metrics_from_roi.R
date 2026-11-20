#' Compute metrics from a single ROI
#'
#' Internal helper used by train_model() to compute
#' metrics for both cave and background ROIs. Not exported.
#'
#' @noRd
metrics_from_roi <- function(roi, metrics){
  
  ## PREPARE DATA ------------------------------------------------------------------------------------------------
  
  # get dataframe with point coordinates and other variables
  pts <- roi@data
  
  # subset points classified as "ground" (2) and as "noise" (7)
  pts <- pts[ pts$Classification == 2 | pts$Classification == 7, ]
  
  # subset ground points
  is_ground <- pts$Classification == 2
  ground_pts <- pts[is_ground,]
  
  # fit plane to ground surface: Z = a + bX + cY
  fit <- stats::lm(Z ~ X + Y, data = ground_pts)
  
  # predicted plane z values for all points
  pts$Z_plane <- stats::predict(fit, newdata = pts[,c("X","Y")] )
  
  # residuals (negative means below plane)
  # in theory, i could obtain the orthogonal distance between each point and plane, instead of the vertical distance, but this is useless, since both are always proportional
  pts$residual <- pts$Z - pts$Z_plane
  
  # get index of point lowest below plane (representative cave point)
  id <- which.min(pts$residual)
  
  # get 2D distances of all ground points to cave point
  ground_pts$dists <- sqrt(
    (ground_pts$X - pts$X[id])^2 +
      (ground_pts$Y - pts$Y[id])^2
  )
  
  # get maximum 2D distance 
  # (its usually very close to radius defined for ROI, but can be higher in a specific case: if, in the original non-corrected ROI, the cave entrance is intersected by the ROI edge, it might happen than, after correction, there are new, even lower points that were not visible in the initial ROI. This means even the corrected ROI in not centered in the deepest point. So, when we calculate dists to the new lowest point, some will be higher larger the radius)
  max_dist <- max(ground_pts$dists)
  
  
  
  ## GET METRICS ------------------------------------------------------------------------------------------------
  
  # creating a list with a function for each metric, so i can choose which ones to call
  metric_funs <- list(
    
    # depth to plane
    depth_plane = function() -pts$residual[id],
    
    # depth to mean
    depth_mean = function() mean( ground_pts$Z ) - pts$Z[id],
    
    # depth to half-radius mean (to look for holes at a smaller scale)
    depth_half_radius_mean = function() mean( ground_pts$Z[ ground_pts$dists < max_dist/2 ] ) - pts$Z[id],
    
    # depth to weighted mean
    # making points in a certain radius around minima have 0 contribution (those which are likely to also be inside entrance)
    depth_w_mean = function(){
      weights <- ifelse(
        ground_pts$dists < 1, #TUNE 1m meter around entrance gives 0 contribution
        0,
        max_dist - ground_pts$dists # closer has higher contribution
      )
      stats::weighted.mean( ground_pts$Z, weights ) - pts$Z[id]
    },
    
    # calculate slope from plane
    slope = function() atan(sqrt( stats::coef(fit)["X"]^2 + stats::coef(fit)["Y"]^2)) * 180 / pi,
    
    # other points values
    intensity = function() pts$Intensity[id],
    return_number = function() pts$ReturnNumber[id],
    R = function() pts$R[id],
    G = function() pts$G[id],
    B = function() pts$B[id],
    NIR = function() pts$NIR[id]
  )
  
  # outputting only selected metrics
  as.data.frame( lapply(metric_funs[metrics], function(f) f() ) )
}

