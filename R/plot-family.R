
#' @title ggplot_return
#' @return A ggplot. 
ggplot_return <- function(){}



#' @title ggplot_return
#' @return A ggplot. 
ggplot_family <- function(){}


#' @title Plot cell migration 
#' 
#' @description Visualizes the cells migration in a typical migration plot.
#' Scales the cell's x- and y-coordinates such that all cell's migration
#' paths start from the same position.
#'
#' @inherit argument_dummy params
#' 
#' @inherit ggplot_return return
#' @export
#'

plotAllTracks <- function(object,
                          across = "condition",
                          across_subset = NULL,
                          time_subset = NULL,
                          phase = NULL,
                          n_cells = 100,
                          color_by = across, 
                          linetype = "solid", 
                          linesize = 0.75,
                          clrp = "milo", 
                          ...,
                          verbose = TRUE){
  
  check_object(object, exp_type_req = "time_lapse")
  
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  confuns::is_value(across, "character")
  
  if(base::length(phase) >= 2 & !across %in% c("condition", "cl_condition")){
    
    base::stop(
      "Plotting all tracks over several phases while splitting by clustering variables is not allowed as cluster variables are calculated for every phase respectively."
    )
    
  } else if(base::length(phase) == 1) {
    
    confuns::check_one_of(
      input = across, 
      against = getGroupingOptions(object, phase = phase) %>% purrr::flatten_chr()
    )
    
  }
  
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
  
  confuns::check_across_subset(
    df = ., 
    across = across, 
    across.subset = across_subset
  ) %>% cell_ids <- 
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
    ggplot2::labs(x = NULL, y = NULL, color = confuns::make_capital_letters(string = across),
                  subtitle = glue::glue("Time: {time_subset} {getIntervalUnit(object)}")) + 
    hlpr_caption_add_on(object = object, phase = phase) + 
    confuns::scale_color_add_on(variable = "discrete", clrp = clrp)
  
}




#' @title Plot cell count 
#' 
#' @description Visualizes the number and distribution of cells across 
#' a discrete feature of choice. 
#'
#' @inherit argument_dummy params
#' 
#' @param color_by Character value. Denotes the discrete variable with 
#' which to color the columns of the plot. Defaults to input for 
#' argument \code{across}.
#'
#' @inherit ggplot_return return
#' @export
#'

plotCellCount <- function(object, across, color_by = across, phase = NULL){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  stat_df <-
    getStatsDf(object, verbose = FALSE, phase = phase) %>% 
    dplyr::select(-phase, -cell_id)
  
  confuns::check_one_of(
    input = across, 
    against = base::colnames(stat_df)
  )
  
  confuns::check_one_of(
    input = color_by, 
    against = base::colnames(stat_df)
  )
  
  ggplot2::ggplot(data = stat_df, mapping = ggplot2::aes(x = .data[[across]])) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[color_by]]), color = "black") + 
    ggplot2::theme_classic() + 
    ggplot2::labs(y = "Cell Count", x = NULL) + 
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = "milo")
  
}


#' @title Plot a scatterplot
#' 
#' @description Convenient wrapper around a variety of scatterplot functionalities. See details for more. 
#' 
#' @inherit argument_dummy params
#'
#' @param variable_x,variable_y Character values. The numeric variables to be plotted on the x-axis and
#' y-axis. Use \code{getNumericVariableNames()} to obtain all valid input options.
#' @param display_corr Logical value. If set to TRUE the correlation of the x- and y-variable is calculated 
#' and displayed on the plot (or each subplot if \code{across} is specified.) 
#' @param corr_method Character value. Denotes the method with which to compute the correlation values if 
#' \code{display_corr} is set to TRUE. Defaults to \emph{'pearson'}.
#' @param corr_pmin Numeric value. The minimum p-value that is displayed as a number. Everything below it is
#' displayed as \emph{ < \code{corr_pmin}}.
#' @param corr_pos_x,corr_pos_y Numeric values or NULL. Specify the exact position of the text used to display the correlation 
#' results. If set to NULL defaults to right upper corner of the plot.
#' @param corr_text_sep Character value. The string that separates correlation value and respective p-value.
#' @param corr_text_size Numeric value. The size used to print the correlation results.
#' 
#' @details In this particular case argument \code{across} can be specified of a character vector of length 2. In this case argument 
#' \code{across_subset} must be a list of character vectors whereby the names are equal to the input for \code{across}. 
#'
#' @inherit ggplot_family return
#' @export
#'
plotScatterplot <- function(object, 
                            variable_x, 
                            variable_y,
                            phase = NULL,
                            across = NULL,
                            across_subset = NULL,
                            relevel = TRUE,
                            ncol = NULL,
                            nrow = NULL,
                            scales = "fixed",
                            space = "fixed",
                            pt_alpha = 0.9,
                            pt_clr = "black",
                            pt_fill = "black",
                            pt_shape = 21,
                            pt_size = 1.5,
                            display_smooth = FALSE,
                            smooth_alpha = 0.9,
                            smooth_clr = "blue",
                            smooth_method = NULL,
                            smooth_se = FALSE,
                            display_corr = FALSE,
                            corr_method = "pearson",
                            corr_pmin = 5e-05,
                            corr_pos_x = NULL,
                            corr_pos_y = NULL,
                            corr_text_sep = "\n",
                            corr_text_size = 1){
  
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  
  stats_df <- 
    getStatsDf(object = object, phase = phase, verbose = FALSE) %>% 
    dplyr::select(-phase, -cell_id)
  
  
  confuns::plot_scatterplot(
    df = stats_df, 
    x = variable_x, 
    y = variable_y, 
    across = across, 
    across.subset = across_subset,
    relevel = relevel,
    ncol = ncol,
    nrow = nrow,
    scales = scales,
    space = space,
    pt.alpha = pt_alpha,
    pt.clr = pt_clr,
    pt.fill = pt_fill,
    pt.shape = pt_shape,
    pt.size = pt_size,
    display.smooth = display_smooth,
    smooth.alpha = smooth_alpha,
    smooth.clr = smooth_clr,
    smooth.method = smooth_method,
    smooth.se = smooth_se,
    display.corr = display_corr,
    corr.method = corr_method,
    corr.p.min = corr_pmin,
    corr.pos.x = corr_pos_x,
    corr.pos.y = corr_pos_y,
    corr.text.sep = corr_text_sep,
    corr.text.size = corr_text_size
  )
  
}



#' @title Plot single cell migration 
#' 
#' @description Visualizes the migration of single cells of interest. 
#'
#' @inherit argument_dummy params
#'
#' @inherit ggplot_return return
#' @export
#'

plotSingleTracks <- function(object,
                             cell_ids,
                             phase = "all",
                             color_by = NULL,
                             scales = "free"){
  
  track_df <-
    getTracksDf(object = object, phase = phase) %>% 
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
#' @inherit argument_dummy params
#' @param colors A vector of colors that is used as the heatmap's color spectrum. 
#' @param arrange_rows Character value. Either \emph{'maxima'} or \emph{'none'}.
#' 
#' @export
 
plotVelocityHeatmap <- function(object, 
                                across = "cl_condition", 
                                across_subset = NULL,
                                phase = "all",
                                n_cells = 100,
                                color = NA, 
                                colors = viridis::viridis(15),
                                smooth = TRUE, 
                                smooth_span = 0.25,
                                arrange_rows = "maxima",
                                verbose = TRUE, 
                                in_shiny = FALSE){
  
  check_object(object, exp_type_req = "time_lapse")
  assign_default(object)
  
  if(!base::is.na(color)){
    
    base::warning("Argument 'color' is deprecated due to naming issues. Please use argument 'colors' instead.")
    
  }
  
  phase <- check_phase(object, phase = phase)
  
  confuns::is_value(across, mode = "character")
  
  if(base::length(phase) >= 2 & !across %in% c("condition", "cl_condition")){
    
    base::stop(
      "Plotting velocity over several phases across clustering variables is not allowed as cluster variables are calculated for every phase respectively."
    )
    
  } else if(base::length(phase) == 1) {
    
    confuns::check_one_of(
      input = across, 
      against = getGroupingOptions(object, phase = phase) %>% purrr::flatten_chr()
    )
    
  }
  
  # the speed data shifted and sliced
  speed_df <- 
    getTracksDf(object,
                phase = phase,
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
      color = colors
      )
  
  base::return(velocity_heatmap)
  
}


#' @title Plot cell activity over time 
#'
#' @description Visualizes the percentage of active cells over time. 
#' 
#' @inherit argument_dummy params
#' @param threshold Numeric value or NULL. If set to NULL (the default) the 
#' threshold to consider a cell 'active' is equal to \code{base::mean(speed) + base::sd(speed)}
#' 
#' @inherit ggplot_return params
#' 
#' @export

plotVelocityLineplot <- function(object, 
                                 across = "cl_condition", 
                                 across_subset = NULL, 
                                 phase = NULL,
                                 threshold = NULL,
                                 linesize = 1,
                                 smooth = TRUE, 
                                 smooth_span = 0.25, 
                                 smooth_se = FALSE,
                                 clrp = "milo", 
                                 verbose = TRUE, 
                                 ...,
                                 in_shiny = FALSE){
  
  check_object(object, exp_type_req = "time_lapse")
  assign_default(object)
  
  # speed data shifted 
  speed_df <- 
    getTracksDf(object,
              phase = phase,
              verbose = verbose) %>% 
    hlpr_merge_conditions(
      track_df = ., 
      phase = phase, 
      across = across, 
      verbose = verbose
    ) %>% 
    hlpr_subset_across(
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
        se = smooth_se, 
        size = linesize
        )
    
  } else {
    
    geom_line_add_on <- 
      ggplot2::geom_path(
        mapping = ggplot2::aes(group = .data[[across]], color = .data[[across]]), 
        size = linesize
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
                  y = "Active Cells [%]") + 
    confuns::scale_color_add_on(
      aes = "color", variable = plot_df[[across]], clrp = clrp, ...
    )
  
}



#' @title Plot the well plate set up 
#' 
#' @inherit argument_dummy params
#'
#' @inherit ggplot_return return 
#' @export
#'

plotWellPlate <- function(object, 
                          well_plate = NULL,
                          color_by = "condition", 
                          clrp_adjust = NULL,
                          make_pretty = NULL){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(well_plate)){
    
    well_plate <- getWellPlateNames(object)[1]
    
  }
  
  wp_df <- object@well_plates[[well_plate]]$wp_df_eval
  
  pt_size <- 13.5
  pt_stroke <- 2
  
  border <- 0.75
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(base::is.character(color_by) && color_by == "condition"){
    
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
      dplyr::mutate(
        condition = tidyr::replace_na(condition, replace = "unknown"), 
        condition = dplyr::case_when(information_status == "Discarded" ~ "Discarded", TRUE ~ condition)
        )
    
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
  
  
  if(color_by == "information_status"){
    
    clrp_adjust <- colors_information_status
    
  } else {
    
    clrp_adjust <- c(clrp_adjust, "Discarded" = "lightgrey")
    
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
    confuns::scale_color_add_on(
      aes = "fill", variable = wp_df[[color_by]], clrp = "milo", 
      clrp.adjust = c(clrp_adjust, "unknown" = "lightgrey", "unknown & unknown" = "lightgrey")
      ) + 
    ggplot2::labs(
      fill = confuns::make_capital_letters(string = color_by, capital.letters = make_pretty)
    ) + 
    facet_add_on 
  
  
}



