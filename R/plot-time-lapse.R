

#' @title Visualize changes of cell characteristics over time
#' 
#' @description Uses the design of a lineplot to visualize feature 
#' dynamics over time across a grouping variable of interest. 
#' 
#' @inherit argument_dummy params
#' @inherit plotScatterplot params
#' @param variables Character vector. Denotes the track variables whoose summary you 
#' want to display over time. Use \code{getTrackVariableNames()} to obtain all 
#' valid input options. 
#' @param display_vline Logical value. If set to TRUE and more than one phase 
#' is referred to a vertical line is displayed hihglighting the phase separation.
#' @param summarize_with Character value. One of \emph{'mean'}, \emph{'median'},
#' \emph{'max'} or \emph{'min'}.
#' @param vline_clr Character value. Color of the vertical line. 
#' @param vline_type Character value. Defaults to \emph{'dashed'}.
#'
#' @inherit ggplot_family return
#' @export
#'
plotTimeLineplot <- function(object,
                             variables,
                             across = "condition", 
                             across_subset = NULL, 
                             summarize_with = "mean",
                             phase = NULL,
                             linesize = 1,
                             scales = "free_y", 
                             nrow = NULL, 
                             ncol = NULL,
                             smooth = TRUE, 
                             smooth_method = "loess",
                             smooth_span = 0.25, 
                             smooth_se = FALSE,
                             display_vline = TRUE, 
                             vline_clr = "lightgrey", 
                             vline_type = "dashed",
                             clrp = "milo", 
                             verbose = TRUE, 
                             ...,
                             in_shiny = FALSE){
  
  check_object(object, exp_type_req = "timelapse")
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  confuns::is_vec(variables, mode = "character")
  
  confuns::check_one_of(
    input = variables, 
    against = getTrackVariableNames(object)
  )
  
  
  confuns::is_value(x = summarize_with, mode = "character")
  
  confuns::check_one_of(
    input = summarize_with, 
    against = base::names(stat_funs)
  )
  
  apply_fun <- stat_funs[summarize_with]
  
  pattern_string <- stringr::str_c("_", summarize_with, "$")
  replacement_string <- stringr::str_c(" (", summarize_with, ")")
  
  plot_df <- 
    getTracksDf(
      object,
      with_grouping = TRUE,
      phase = phase,
      verbose = verbose
    ) %>% 
    hlpr_merge_conditions(
      track_df = ., 
      phase = phase, 
      across = across, 
      verbose = verbose
    ) %>% 
    confuns::check_across_subset(
      df = ., 
      across = across, 
      across.subset = across_subset,
      relevel = relevel
    ) %>% 
    dplyr::select(frame_num, dplyr::all_of(x = variables), !!rlang::sym(across)) %>% 
    dplyr::group_by(frame_num, !!rlang::sym(across)) %>% 
    dplyr::summarise(dplyr::across(.cols = dplyr::all_of(variables), .fns = apply_fun, na.rm = TRUE), .groups = "keep") %>% 
    dplyr::arrange(frame_num) %>% 
    tidyr::pivot_longer(
      cols = dplyr::all_of(stringr::str_c(variables, summarize_with, sep = "_")),
      names_to = "vars", 
      values_to = "values"
    ) %>% 
    dplyr::mutate(
      vars = stringr::str_replace_all(vars, pattern = pattern_string, replacement = replacement_string)
    )
  
  
  ggplot2::ggplot(plot_df, mapping = ggplot2::aes(x = frame_num, y = values, color = .data[[across]])) + 
    ggplot2::geom_smooth(formula = y ~ x, method = smooth_method, span = smooth_span, se = smooth_se) + 
    ggplot2::facet_wrap(facets = ~ vars, scales = scales, nrow = nrow, ncol = ncol) +
    ggplot2::theme_classic() + 
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "lightgrey"), 
      strip.background = ggplot2::element_blank()
    ) + 
    ggplot2::labs(x = stringr::str_c("Time [", object@set_up$itvl_u, "]", sep = ""), y = NULL) + 
    ggplot2::scale_x_continuous(
      labels = getFrameTimeSeq(object, phase = phase),
      breaks = base::unique(plot_df$frame_num)
    ) + 
    confuns::scale_color_add_on(
      aes = "color", variable = plot_df[[across]], clrp = clrp
    ) + 
    hlpr_caption_add_on(object, phase = phase) + 
    hlpr_phase_vertical_line(object, phase = phase, vline_clr = vline_clr, vline_type = vline_type)
  
}

#' @title Visualize changes of cell characteristics over time
#'
#' @description Uses the design of a heatmap to visualize feature 
#' dynamics over time across a grouping variable of interest. 
#' 
#' @inherit argument_dummy params
#' @param colors A vector of colors that is used as the heatmap's color spectrum. 
#' @param arrange_rows Character value. Either \emph{'maxima'} or \emph{'none'}.
#' @param ... Additional arguments given to \code{pheatmap::pheatmap()}.
#' 
#' @export

plotTimeHeatmap <- function(object,
                            variable,
                            across = "condition",
                            across_subset = NULL,
                            phase = "all",
                            n_cells = 100,
                            color = NA,
                            colors = viridis::viridis(15),
                            smooth = TRUE,
                            smooth_span = 0.25,
                            arrange_rows = "maxima",
                            verbose = TRUE,
                            ...,
                            in_shiny = FALSE){
  
  check_object(object, exp_type_req = "time_lapse")
  assign_default(object)
  
  phase <- check_phase(object, phase = phase)
  
  confuns::is_value(variable, mode = "character")
  
  confuns::check_one_of(
    input = variable, 
    against = getTrackVariableNames(object)
  )
  
  if(!base::is.na(color)){
    
    base::warning("Argument 'color' is deprecated due to naming issues. Please use argument 'colors' instead.")
    
  }
  
  confuns::is_value(across, mode = "character")
  
  if(base::length(phase) >= 2 & !across %in% c("condition")){
    
    base::stop(
      "Plotting changes over several phases across clustering variables is not allowed as cluster variables are calculated for every phase respectively."
    )
    
  } else if(base::length(phase) == 1) {
    
    confuns::check_one_of(
      input = across, 
      against = getGroupingVariableNames(object, phase = phase)
    )
    
  }
  
  # the speed data shifted and sliced
  speed_df <- 
    getTracksDf(
      object,
      phase = phase,
      with_cluster = TRUE, 
      with_meta = TRUE, 
      with_well_plate = TRUE,
      verbose = FALSE
    ) %>% 
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
      values_from = dplyr::all_of(variable)
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
      color = colors, 
      ...
    )
  
  base::return(velocity_heatmap)
  
}