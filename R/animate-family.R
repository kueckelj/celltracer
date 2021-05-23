


#' @title Animate cellular movement
#' 
#' @description This function animates the movement of cells that were covered 
#' within one well-image. 
#'
#' @inherit argument_dummy params
#' @param style Character value. If set to \emph{'cell_tracker'} imitates the tracking process of the CellTracker software.  
#' @param ... Additional arguments given to \code{gganimate::animate()}.
#'
#' @return An animated GIF.
#' @export
#'
animateWellImage <- function(object,
                             well_plate,
                             well,
                             image,
                             phase = "all",
                             style = "normal",
                             add_on_list = list(), 
                             ...){
  
  
  check_object(object)
  assign_default(object)
  
  confuns::is_value(x = image, mode = "numeric")
  
  well_image <- stringr::str_c(well, image, sep = "_")
  
  track_df <- getTracksDf(object, phase = phase) 
  
  confuns::check_one_of(
    input = well_image, 
    against = track_df$well_image %>% base::unique()
  )
  
  track_df <-   dplyr::filter(track_df, well_image == {{well_image}} & well_plate_name == {{well_plate}})
  
  
  if(style == "normal"){
    
    panel_background <-
      ggplot2::element_rect(color = "black")
    
    geom_point_add_on <-
      ggplot2::geom_point(
        size = 3, alpha = 0.75, shape = 21,
        mapping = ggplot2::aes(color = cell_id, fill = cell_id)
      )
    
    
  } else if(style == "cell_tracker"){
    
    panel_background <- ggplot2::element_rect(fill = "black")
    geom_point_add_on <- 
      ggplot2::geom_point(
        size = 5, alpha = 0.75, shape = 22,
        mapping = ggplot2::aes(color = cell_id)
      )
    
  } 
  
  gif <- 
    ggplot2::ggplot(data = track_df, mapping = ggplot2::aes(x = x_coords, y = y_coords)) + 
    geom_point_add_on +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.background = panel_background,
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      legend.position = "None"
    ) +
    ggplot2::scale_y_reverse() + 
    gganimate::transition_time(time = frame_time) + 
    ggplot2::labs(title = "Frame: {base::round(frame_time)}", x = NULL, y = NULL) + 
    add_on_list
  
  gganimate::animate(plot = gif, ...)
  
}




#' @title Animate all tracks
#' 
#' @description Animates the output of function \code{plotAllTracks()}.
#' 
#' @inherit argument_dummy params
#' @inherit plotAllTracks params
#' @param ... Additional arguments given to \code{gganimte::animate()}.
#'
#' @return An animated GIF.
#' @export
#'
animateAllTracks <- function(object,
                             across = "cell_line",
                             across_subset = NULL,
                             frame_subset = NULL,
                             phase = "all",
                             n_cells = 100,
                             color_by = across, 
                             linetype = "solid", 
                             linesize = 0.75, 
                             add_on_list = list(), 
                             verbose = NULL,
                             ...){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  
  track_df <- 
    getTracksDf(object = object,
                phase = phase,
                with_meta = TRUE, 
                with_cluster = TRUE,
                verbose = FALSE) %>% 
    hlpr_merge_conditions(
      track_df = ., 
      phase = phase, 
      across = across, 
      verbose = verbose
    ) %>% 
    confuns::check_across_subset(
      df = ., 
      across = across, 
      across.subset = across_subset
    )  
  
  cell_id_df <- 
    dplyr::select(track_df, dplyr::all_of(x = c("cell_id", across))) %>% 
    dplyr::distinct() %>% 
    dplyr::group_by(!!rlang::sym(across)) %>% 
    dplyr::slice_sample(n = n_cells)
  
  cell_ids <- 
    dplyr::pull(.data = cell_id_df, var = "cell_id")
  
  plot_df <- 
    dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(cell_id %in% {{cell_ids}}) %>% 
    dplyr::mutate(
      x_coords = x_coords - x_coords[1], 
      y_coords = y_coords - y_coords[1], 
      facet = !!rlang::sym(across)) %>% 
    dplyr::ungroup() 
  
  annotation_df <-  
    dplyr::group_by(cell_id_df, !!rlang::sym(across)) %>% 
    dplyr::tally() %>% 
    dplyr::mutate(label = stringr::str_c("n", n, sep = " = "))
  
  if(base::is.numeric(frame_subset)){
    
    plot_df <- 
      dplyr::filter(.data = plot_df, frame_num <= {{frame_subset}})
    
  }
  
  if(base::is.character(color_by)){
    
    mapping <- ggplot2::aes(group = cell_id, color = .data[[color_by]])
    
  } else {
    
    mapping <- ggplot2::aes(group = cell_id)
    
  }
  
  x_range <- base::range(plot_df$x_coords)
  y_range <- base::range(plot_df$y_coords)
  
  mxm <- base::abs(base::max(c(x_range, y_range)))
  
  gif <- 
    ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = x_coords, y = y_coords)) + 
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "lightgrey") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "lightgrey") +
    ggplot2::geom_path(mapping = mapping, linetype = linetype, size = linesize) + 
    ggplot2::geom_text(data = annotation_df, x = Inf, y = Inf, vjust = 1, hjust = 1,
                       mapping = ggplot2::aes(label = label), size = 3) +
    ggplot2::scale_x_continuous(limits = c(-mxm, mxm)) +
    ggplot2::scale_y_continuous(limits = c(-mxm, mxm)) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    ) + 
    ggplot2::facet_wrap(facets = ~ facet, scales = "fixed") + 
    gganimate::transition_reveal(along = frame_time) + 
    ggplot2::labs(title = "Frame: {base::round(frame_along)}", x = NULL, y = NULL) +  
    hlpr_caption_add_on(object = object, phase = phase) + 
    add_on_list()
  
  gganimate::animate(plot = gif, ...)
  
}




