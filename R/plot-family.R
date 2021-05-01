
#' @title ggplot_return
#' @return A ggplot. 
ggplot_return <- function(){}




#' @title Plot cell migration 
#' 
#' @description 
#'
#' @inherit check_object params
#' @inherit hlpr_subset_across params 
#' @param time_subset Numeric value. Refers to the time up to which the migration
#' is plotted. If set to NULL the entire timespan is used. 
#' @inherit phase_all params
#' @inherit phase_cluster params 
#' @inherit n_cells params
#' @inherit aes_to params
#' @inherit linetype params
#' @inherit linesize params
#' @inherit verbose params 
#'
#' @inherit ggplot_return return
#' @export
#'

plotAllTracks <- function(object,
                          across = "condition",
                          across_subset = NULL,
                          time_subset = NULL,
                          phase = NULL,
                          phase_cluster = NULL,
                          n_cells = 100,
                          color_by = NULL, 
                          linetype = "solid", 
                          linesize = 0.75,
                          clrp = "milo", 
                          ...,
                          verbose = TRUE){
  
  track_df <- 
    getTracks(object = object,
              phase = phase,
              phase_cluster = phase_cluster,
              verbose = verbose) %>% 
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
  
  x_range <- base::range(plot_df$x_coords)
  y_range <- base::range(plot_df$y_coords)
  
  mxm <- base::abs(base::max(c(x_range, y_range)))
  
  if(base::is.numeric(time_subset)){
    
    plot_df <- 
      dplyr::filter(.data = plot_df, frame_time <= {{time_subset}})
    
  }
  
  if(base::is.character(color_by)){
    
    mapping <- ggplot2::aes(group = cell_id, color = .data[[color_by]])
    
  } else {
    
    mapping <- ggplot2::aes(group = cell_id)
    
  }
  
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
      panel.grid.minor = ggplot2::element_blank(), 
      strip.background = ggplot2::element_blank(), 
      axis.text = ggplot2::element_blank(), 
      axis.ticks = ggplot2::element_blank()
    ) + 
    ggplot2::facet_wrap(facets = ~ facet, scales = "fixed", ...) + 
    ggplot2::labs(x = NULL, y = NULL,
                  subtitle = glue::glue("Time: {time_subset} {getIntervalUnit(object)}")) + 
    hlpr_caption_add_on(object = object, phase = phase) + 
    confuns::scale_color_add_on(variable = "discrete", clrp = clrp)
  
}




#' @title Plot cell count 
#' 
#' @description Visualizes the number and distribution of cells across 
#' a discrete feature of choice. 
#'
#' @inherit check_object params
#' @inherit hlpr_subset_across params  
#' @inherit aes_to params
#'
#' @inherit ggplot_return return
#' @export
#'

plotCellCount <- function(object, across, color_by){
  
  stat_df <- getStats(object)
  
  ggplot2::ggplot(data = stat_df, mapping = ggplot2::aes(x = .data[[across]])) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[color_by]])) + 
    ggplot2::theme_classic() + 
    ggplot2::labs(y = "Number of Cells") + 
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = "milo")
  
}



#' @title Plot dimensional reduction 
#' 
#' @description Visualizes the dimensional reduction method of choice.
#'
#' @inherit check_object params
#' @inherit dim_red_method params
#' @inherit aes_to params
#' @inherit pt_args 
#'
#' @inherit ggplot_return return
#' @export
#'

plotDimRed <- function(object,
                       dim_red = "umap",
                       color_by = NULL,
                       pt_size = 1,
                       pt_alpha = 0.9){
  
  dim_red_df <-
    getDimRed(object = object, dim_red_method = dim_red_method)
  
  x_y <- stringr::str_c(dim_red, 1:2, sep = "")
  
  if(base::is.character(color_by)){
    
    mapping <- ggplot2::aes(color = !!rlang::sym(color_by))
    
  } else {
    
    mapping <- ggplot2::aes()
    
  }
  
  ggplot2::ggplot(data = dim_red_df,
                  mapping = ggplot2::aes(x = .data[[x_y[1]]], y = .data[[x_y[2]]])
  ) + 
    ggplot2::geom_point(mapping = mapping, size = pt_size, alpha = pt_alpha) + 
    ggplot2::theme_classic() 
  
  
}





#' @title Plot single cell migration 
#' 
#' @description Visualizes the migration of single cells of interest. 
#'
#' @inherit check_object params
#' @inherit cell_ids params
#' @inherit aes_to params 
#' @param scales Character value. Given to argument \code{scales} of
#' \code{ggplot2::facet_wrap()}.
#'
#' @inherit ggplot_return return
#' @export
#'

plotSingleTracks <- function(object,
                             cell_ids,
                             color_by = NULL,
                             scales = "free"){
  
  track_df <-
    getTracks(object = object) %>% 
    dplyr::filter(cell_id %in% {{cell_ids}}) 
  
  start_df <- 
    dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(frame_num == base::min(frame_num))
  
  if(base::is.character(color_by)){
    
    mapping <- ggplot2::aes(x = x_coords, y = y_coords, color = !!rlang::sym(color_by))
    
  } else {
    
    mapping <- ggplot2::aes(x = x_coords, y = y_coords)
    
  }
  
  ggplot2::ggplot(data = track_df, mapping = mapping) + 
    ggplot2::geom_point(size = 0.75) + 
    ggplot2::geom_path(mapping = ggplot2::aes(group = cell_id),
                       size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(0.1, "inches"))) +
    ggplot2::geom_point(data = start_df, size = 2) + 
    ggplot2::facet_wrap(facets = ~ cell_id, scales = scales) +
    ggplot2::theme_bw() + 
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), 
      strip.background = ggplot2::element_rect(fill = ggplot2::alpha("steelblue", 0.75))
    ) + 
    ggplot2::labs(x = "x-coordinates", y = "y-coordinates")
  
  
}


#' @title Plot cell velocity over time 
#'
#' @description Uses the design of a heatmap to visualize the velocity 
#' dynamics over time across a variable of interest. 
#' 
#' @inherit check_object params 
#' @inherit hlpr_subset_across params
#' @inherit phase_all params
#' @inherit phase_cluster params 
#' @inherit n_cells params 
#' @param color The colorspectrum to be used. 
#' @param arrange_rows Character value. Either \emph{'maxima'} or \emph{'none'}.
#' @inherit check_smooth params
#' @inherit verbose params
#' 
#' @export
 
plotVelocityHeatmap <- function(object, 
                                across = "cl_condition", 
                                across_subset = NULL,
                                phase = "all",
                                phase_cluster = "first_tmt",
                                n_cells = 100,
                                color = viridis::viridis(15), 
                                smooth = TRUE, 
                                smooth_span = 0.25,
                                arrange_rows = "maxima",
                                verbose = TRUE, 
                                in_shiny = FALSE){
  
  # the speed data shifted and sliced
  speed_df <- 
    getTracks(object,
              phase = phase,
              phase_cluster = phase_cluster, 
              verbose = verbose) %>% 
    confuns::check_across_subset(
      df = ., 
      across = across, 
      across.subset = across_subset
    ) %>% 
    tidyr::pivot_wider(
      data = ., 
      id_cols = dplyr::all_of(x = c("cell_id", "frame_itvl", across)), 
      names_from = "frame_itvl", 
      values_from = "speed"
    ) %>% 
    dplyr::group_by(!!rlang::sym(across)) %>% 
    dplyr::slice_sample(n = n_cells)
  
  if(arrange_rows %in% c("maxima", "minima")){
    
    speed_df <-
      dplyr::group_modify(
        .data = speed_df,
        .f =  ~ confuns::arrange_rows(df = .x, according.to = "maxima", verbose = FALSE)
        )
  
  }
  
  # the speed data as a numeric matrix (rownames = cell_ids)
  speed_mtr <- 
    tibble::column_to_rownames(.data = speed_df, var = "cell_id") %>% 
    dplyr::select(-dplyr::all_of(x = c(across))) %>% 
    base::as.matrix()
  
  cell_ids <- base::rownames(speed_mtr)
  
  # a vector splicing the heatmap 
  gaps_row <- 
    dplyr::group_by(.data = speed_df, !!rlang::sym(across)) %>% 
    dplyr::summarise(count = dplyr::n()) %>% 
    dplyr::mutate(positions = base::cumsum(x = count)) %>% 
    dplyr::pull(var = "positions")
  
  # a data.frame annotating the heatmap indicating the group belonging of the cell ids
  annotation_row <- 
    dplyr::select(.data = speed_df, !!rlang::sym(across)) %>% 
    base::as.data.frame() %>% 
    magrittr::set_rownames(value = cell_ids)
  
  
# Smooth values -----------------------------------------------------------
  
  if(base::isTRUE(smooth)){

    if(base::isTRUE(in_shiny)){
      
      shiny_fdb(in_shiny,
                ui = glue::glue("Smoothing values of {base::length(cell_ids)} cells."),
                type = "message")
      
    } else if(base::isTRUE(verbose)){
      
      base::message(glue::glue("Smoothing values of {base::length(cell_ids)} cells."))
      
      pb <- progress::progress_bar$new(
        format = "Progress: [:bar] :percent eta: :eta", 
        total = base::length(cell_ids), clear = FALSE, width = 100
      )
      
    }
      
    time_seq <- base::seq_along(base::colnames(speed_mtr))
    
    smooth_length <- base::length(time_seq) * 10
    predict_seq <- base::seq(1, base::max(time_seq), length.out = smooth_length)
    
    plot_mtr <-
      base::matrix(nrow = base::nrow(speed_mtr), ncol = smooth_length) %>%
      magrittr::set_rownames(value = cell_ids)
    
    for(cell in base::rownames(speed_mtr)){
      
      if(base::isTRUE(verbose)){ pb$tick() }
      
      speed <- speed_mtr[cell, ]
      
      speed_loess <- stats::loess(formula = speed ~ time_seq, span = smooth_span)
      
      plot_mtr[cell, ] <-
        stats::predict(object = speed_loess, predict_seq)
      
    }
    
  } else {
    
    plot_mtr <- speed_mtr
    
  }
  
  # -----
  
  if(base::isTRUE(verbose)){base::message("Plotting. This might take a few seconds.")}
  
  velocity_heatmap <- 
    pheatmap::pheatmap(
      mat = plot_mtr, 
      annotation_row = annotation_row,
      annotation_names_row = FALSE, 
      gaps_row = gaps_row,
      cluster_rows = FALSE, 
      cluster_cols = FALSE, 
      show_rownames = FALSE, 
      show_colnames = FALSE, 
      color = color
      )
  
  base::return(velocity_heatmap)
  
}


#' @title Plot cell activity over time 
#'
#' @description Visualizes the percentage of active cells over time. 
#' 
#' @inherit check_object params 
#' @inherit phase_all params
#' @inherit phase_cluster params 
#' @inherit hlpr_subset_across params
#' @param threshold Numeric value or NULL. If set to NULL (the default) the 
#' threshold to consider a cell 'active' is equal to \code{base::mean(speed) + base::sd(speed)}
#' @inherit check_smooth params
#' @inherit colors params 
#' @inherit verbose params
#' 
#' @inherit ggplot_return params
#' 
#' @export

plotVelocityLineplot <- function(object, 
                                 across = "cl_condition", 
                                 across_subset = NULL, 
                                 phase = "all",
                                 phase_cluster = "first_tmt",
                                 threshold = NULL,
                                 smooth = TRUE, 
                                 smooth_span = 0.25, 
                                 smooth_se = FALSE,
                                 clrp = "milo", 
                                 verbose = TRUE, 
                                 in_shiny = FALSE){
  
  # speed data shifted 
  speed_df <- 
    hlpr_subset_across(
      data = getTracks(object,
                       phase = phase,
                       phase_cluster = phase_cluster,
                       verbose = verbose),
      across = across, 
      across_subset = across_subset
      ) %>% 
    tidyr::pivot_wider(
      data = ., 
      id_cols = dplyr::all_of(x = c("cell_id", "frame_itvl", across)), 
      names_from = "frame_itvl", 
      values_from = "speed"
    ) 
  
  descr_df <-
    dplyr::select(.data = speed_df, cell_id, !!rlang::sym(across))
  
  numeric_mtr <- 
    tibble::column_to_rownames(.data = speed_df, var = "cell_id") %>% 
    dplyr::select_if(.predicate = base::is.numeric) %>% 
    base::as.matrix()
  
  if(!base::is.numeric(threshold)){
    
    threshold <-
      base::mean(numeric_mtr, na.rm = TRUE) + stats::sd(numeric_mtr, na.rm = TRUE)
    
  }
  
  numeric_mtr[numeric_mtr <= threshold] <- 0
  numeric_mtr[numeric_mtr > threshold] <- 1
  
  numeric_df <- base::as.data.frame(numeric_mtr)
  
  plot_df <- 
    tibble::rownames_to_column(.data = numeric_df, var = "cell_id") %>% 
    dplyr::left_join( x = descr_df, y = ., by = "cell_id") %>% 
    dplyr::group_by(!!rlang::sym(across)) %>% 
    dplyr::mutate(group_count = dplyr::n()) %>% 
    dplyr::group_by(!!rlang::sym(across), group_count) %>% 
    dplyr::summarise(dplyr::across(.cols = base::is.numeric, .fns = ~ base::sum(.x))) %>% 
    tidyr::pivot_longer(
      cols = dplyr::ends_with(match = object@set_up$itvl_u),
      names_to = "time",
      values_to = "count_active_cells"
      ) %>% 
    dplyr::mutate(
      perc_active_cells = count_active_cells / group_count * 100,
      time = base::as.numeric(stringr::str_remove(string = time, pattern = object@set_up$itvl_u))
    ) %>% 
    dplyr::ungroup() %>% 
    tidyr::drop_na()
  
  
  if(base::isTRUE(smooth)){
    
    geom_line_add_on <-
      ggplot2::geom_smooth(
        mapping = ggplot2::aes(color = .data[[across]]),
        span = smooth_span, 
        formula = y ~ x, 
        method = "loess", 
        se = smooth_se
        )
    
  } else {
    
    geom_line_add_on <- 
      ggplot2::geom_path(
        mapping = ggplot2::aes(group = cl_condition, color = .data[[across]])
        )  
    
  }
  
  ggplot2::ggplot(data = plot_df, mapping = ggplot2::aes(x = time, y = perc_active_cells)) + 
    geom_line_add_on +
    geom_line_add_on +
    ggplot2::theme_classic() + 
    ggplot2::theme(
      title = ggplot2::element_text(face = "bold", size = 10, vjust = -1),
      axis.title = ggplot2::element_text(face = "bold", size = 10),
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "lightgrey")
    ) + 
    ggplot2::labs(x = stringr::str_c("Time [", object@set_up$itvl_u, "]", sep = ""), 
                  y = "Active Cells [%]")
  
}



#' @title Plot the well plate set up 
#' 
#' @inherit check_object params
#' @inherit well_plate params
#' @inherit aes_to params
#'
#' @inherit ggplot_return return 
#' @export
#'

plotWellPlate <- function(object, 
                          well_plate = NULL,
                          color_by = NULL, 
                          clrp_adjust = NULL,
                          make_pretty = NULL){
  
  check_object(object)
  assign_default(object)
  
  wp_df <- getWellPlateDf(object, well_plate = well_plate)  
  
  pt_size <- 13.5
  pt_stroke <- 2
  
  border <- 0.75
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(color_by == "condition"){
    
    getPhases(object)
    
    c_names <- base::colnames(wp_df$condition_df[[1]])
    
    wp_df <- 
      tidyr::unnest(wp_df, cols = "condition_df") %>% 
      dplyr::select(-condition) %>% 
      tidyr::pivot_longer(
        cols = dplyr::all_of(x = c_names), 
        names_to = "phases", 
        values_to = "condition"
      ) %>% 
      dplyr::mutate(condition = tidyr::replace_na(condition, replace = "unknown"))
    
    facet_add_on <- ggplot2::facet_wrap(facets = . ~ phases)
    
    border_add_on <- NULL
      
    
  } else {
    
    facet_add_on <- NULL
    
    border_add_on <- 
      ggforce::geom_mark_rect(
        mapping = ggplot2::aes(x = col_num, y = row_num, color = group),
        color = "black", size = 1, expand = ggplot2::unit(15, "mm")
      ) 
    
  }
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    ggplot2::geom_point(data = wp_df, mapping = ggplot2::aes(fill = .data[[color_by]]),
      size = pt_size, shape = 21, alpha = 1, stroke = pt_stroke, 
    ) + 
    ggplot2::geom_text(mapping = ggplot2::aes(label = well)) +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21)), 
      fill = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
      ) + 
    scale_color_add_on(
      aes = "fill", variable = wp_df[[color_by]], clrp = "milo", 
      clrp.adjust = c(clrp_adjust, "unknown" = "lightgrey", "unknown & unknown" = "lightgrey")
      ) + 
    ggplot2::labs(
      fill = confuns::make_capital_letters(string = color_by, capital.letters = make_pretty)
    ) + 
    facet_add_on 
  
  
}



