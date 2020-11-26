
#' @title Compute Umap data
#' 
#' @description A wrapper around the umap algorithm and \code{addDimRedData()}.
#'
#'
#' @inherit check_object params
#'
#' @inherit updated_object return
#' @export

computeUmapData <- function(object){
  
  umap_obj <- 
    tibble::column_to_rownames(stat_df, var = "cell_id") %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::as.matrix() %>% 
    base::scale() %>% 
    umap::umap()
  
  umap_df <-
    base::as.data.frame(x = umap_obj$layout) %>% 
    magrittr::set_colnames(value = c("umap1", "umap2")) %>% 
    tibble::rownames_to_column(var = "cell_id")
  
  object@data$dim_red[["umap"]] <- umap_df
  
  base::return(object)
  
}

#' @title Compute the distance between to points
#'
#' @param starting_pos,final_pos Numeric vector of length two. Denotes the two positions 
#' between which the distance is calculated 
#'
#' @return A numeric value. 
#'

compute_distance <- function(starting_pos, final_pos){
  
  # direction vector
  drvc <- final_pos - starting_pos
  
  # compute effective distance traveled ( = value of direction vector)
  base::sqrt(drvc[1]^2 + drvc[2]^2)
  
}

#' @title Compute the distances from origin 
#' 
#' @description The first values of both input-vectors are taken as 
#' the origin position. For each subsequent position the respective 
#' distance to the position of origin is computed. 
#' 
#' To be used in \code{dplyr::mutate()} to recalculate the distances 
#' of origin if the experiment includes time displaced treatment.
#'
#' @param x_coords Numeric vector. Refers to x-coordinates of object. 
#' @param y_coords Numeric vector. Refers to y-coordinates of object.
#'
#' @return A numeric vector that corresponds to the respective distances from 
#' the first position. 
#' 

compute_distances_from_origin <- function(x_coords, y_coords){
  
  n_coords <- base::length(x_coords)
  
  origin <- c(x_coords[1], y_coords[1])
  
  subsequent_positions <- 
    purrr::map2(.x = x_coords[2:n_coords],
                .y = y_coords[2:n_coords],
                .f = ~ c(.x, .y))
  
  distances_from_origin <- 
    purrr::map_dbl(
      .x = subsequent_positions,
      .f = ~ compute_distance(starting_pos = origin, final_pos = .x)
    ) %>% 
    base::round(digits = 2) %>% 
    purrr::prepend(values = 0)
  
}


#' @title Calculate migration efficiency
#'
#' @param x_coords Numeric vector. Refers to x-coordinates of object. 
#' @param y_coords Numeric vector. Refers to y-coordinates of object.
#' @param actual_distance Numeric vector/value. Refers to the total distance 
#' the object of interested has traveled. 
#'
#' @return A numeric value assessing how efficient the object of interested has
#' migrated. 

compute_migration_efficiency <- function(x_coords, y_coords, actual_distance){
  
  # compute direction vector between first and final position
  starting_pos <- c(x_coords[1], y_coords[1])
  final_pos <- c(x_coords[length(x_coords)], y_coords[length(y_coords)])
  drvc <- final_pos - starting_pos
  
  # compute effective distance travelled ( = value of direction vector)
  effective_distance <- base::sqrt(drvc[1]^2 + drvc[2]^2)
  
  # divide effective distance by actual distance
  migration_efficiency <- actual_distance/(actual_distance/effective_distance)
  
  return(migration_efficiency)
  
}


#' @title Summarize tracking data
#'
#' @inherit check_track_df params
#'
#' @return A summarized data.frame. 

compute_stat <- function(track_df){
  
  dplyr::group_by(.data = track_df,
                  cell_id, condition, cell_line, cl_condition,
                  well, well_plate_name, well_plate_index, well_image) %>% 
    dplyr::summarise(
      total_dist = base::sum(dflp),
      max_dist_fo = base::max(dfo),
      avg_dist_fo = base::mean(dfo),
      max_dist_flp = base::max(dflp),
      avg_dist_flp = base::mean(dflp),
      max_speed = base::max(speed),
      avg_speed = base::mean(speed),
      mgr_eff = compute_migration_efficiency(x_coords, y_coords, total_dist)
    ) %>% 
    dplyr::ungroup()
  
}
