
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
                          phase = "all",
                          phase_cluster = "first_tmt",
                          n_cells = 100,
                          color_to = NULL, 
                          linetype = "solid", 
                          linesize = 0.75,
                          clrp = "milo", 
                          ...,
                          verbose = TRUE){
  
  track_df <-
    hlpr_subset_across(data = getTracks(object = object,
                                        phase = phase,
                                        phase_cluster = phase_cluster,
                                        verbose = verbose),
                       across = across,
                       across_subset = across_subset)
  
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
  
  if(base::is.character(color_to)){
    
    mapping <- ggplot2::aes(group = cell_id, color = .data[[color_to]])
    
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

plotCellCount <- function(object, across, color_to){
  
  stat_df <- getStats(object)
  
  ggplot2::ggplot(data = stat_df, mapping = ggplot2::aes(x = .data[[across]])) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[color_to]])) + 
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
                       color_to = NULL,
                       pt_size = 1,
                       pt_alpha = 0.9){
  
  dim_red_df <-
    getDimRed(object = object, dim_red_method = dim_red_method)
  
  x_y <- stringr::str_c(dim_red, 1:2, sep = "")
  
  if(base::is.character(color_to)){
    
    mapping <- ggplot2::aes(color = !!rlang::sym(color_to))
    
  } else {
    
    mapping <- ggplot2::aes()
    
  }
  
  ggplot2::ggplot(data = dim_red_df,
                  mapping = ggplot2::aes(x = .data[[x_y[1]]], y = .data[[x_y[2]]])
  ) + 
    ggplot2::geom_point(mapping = mapping, size = pt_size, alpha = pt_alpha) + 
    ggplot2::theme_classic() 
  
  
}



#' @title Plot descriptive statistics
#' 
#' @description This function allows for a variety of different plots concerning 
#' descriptive statistics. 
#'
#' @inherit check_object params
#' @param variables 
#' @inherit hlpr_subset_across params
#' @inherit phase_single params
#' @param plot_type Character value. One of \emph{'histogram', 'ridgeplot', 'boxplot', 
#' 'violinplot'} or \emph{'density'} to visualize the value distribution of those 
#' variables specified via the \code{variables} argument. 
#'  
#' If set to \emph{'boxplot'} or \emph{'violinplot'} and only one variable is 
#' specified statistical test can be performed. 
#'  
#'  
#' @param binwidth Numeric value. Only relevant if \code{plot_type} is set to \emph{'histogram'}.
#' @param display_points Logical. If set to TRUE the value distribution of \code{n_cells} is additionally
#' displayed by points. 
#' @inherit pt_args params
#' @inherit n_cells params
#' @inherit aes_to 
#' @param test_pairwise Character value. One of \emph{'none', 't.test', 'wilcox.test'}. 
#' @param test_groupwise Character value. One of \emph{'none', 'anova', 'kruskal.test'}.
#' @param ref_group Character value. Denotes the reference group for the statistical tests. Must 
#' be one value of the variable specified in \code{across}. 
#' @inherit colors params 
#' @param ... Additional arguments given to \code{ggplot2::facet_wrap()}.
#' @inherit pretty_names params 
#' @inherit verbose params
#'
#' @inherit ggplot_return return
#' @export
#'

plotDistribution <- function(object,
                             variables = "all",
                             across = "cl_condition",
                             across_subset = NULL,
                             phase = "first_tmt",
                             plot_type = "boxplot",
                             binwidth = 1,
                             display_points = FALSE,
                             pt_size = 1.2, 
                             pt_alpha = 0.8, 
                             pt_color = "black",
                             n_cells = 100,
                             shape_to = NULL, 
                             test_pairwise = "none",
                             test_groupwise = "none",
                             ref_group,
                             clrp = "milo",
                             ... ,
                             pretty_names = TRUE, 
                             verbose = TRUE){
  
  # 1. Control --------------------------------------------------------------
  
  df <- getStats(object, phase = phase)
  
  if(!plot_type %in% c("histogram", "density", "ridgeplot", "boxplot", "violinplot")){
    
    base::stop("Argument 'plot_type' needs to be one of 'histogram', 'density', 'ridgeplot', 'boxplot', 'violinplot'.")
    
  }
  
  if(plot_type %in% c("violinplot", "ridgeplot", "boxplot")){
    
    max_length = 10
    
  } else {
    
    max_length = 25
    
  }
  
  confuns::is_value(clrp, "character", "clrp")
  
  # check across input
  
  confuns::is_value(across, "character", "across")
  confuns::check_data_frame(
    df = df,
    var.class = list(c("character", "factor")) %>% magrittr::set_names(across),
    ref = "df"
  )
  
  # check variable input
  confuns::is_vec(variables, "character", "variables")
  
  if(base::all(variables == "all")){
    
    if(base::isTRUE(verbose)){base::message("Argument 'variables' set to 'all'. Extracting all valid, numeric variables.")}
    
    cnames <- base::colnames(dplyr::select_if(.tbl = df, .predicate = base::is.numeric))
    
    variables <- cnames[!cnames %in% c("x", "y", "umap1", "umap2", "tsne1", "tsne2")]
    
  } else {
    
    check_list <-
      purrr::map(variables, function(i){c("numeric", "integer")}) %>%
      magrittr::set_names(value = variables)
    
    confuns::check_data_frame(
      df = df,
      var.class = check_list,
      ref = "df"
    )
    
    if(base::isTRUE(verbose)){"All specified variables found."}
    
    if(base::isTRUE(pretty_names)){
      
      across <- hlpr_pretty_value(value = across)
      variables <- purrr::map_chr(.x = variables, .f = hlpr_pretty_value)
      df <- hlpr_pretty_colnames(df = df)
      
      if(!base::is.null(shape_to)){
        
        shape_to <- hlpr_pretty_value(value = shape_to)
        
      }
      
    }
    
  }
  
  # -----
  
  # 2. Data extraction ------------------------------------------------------
  
  data <-
    tidyr::pivot_longer(
      data = df,
      cols = dplyr::all_of(x = variables),
      names_to = "variables",
      values_to = "values"
    )
  
  data <- hlpr_subset_across(data, across, across_subset)
  
  reverse <- FALSE
  
  # -----
  
  # 3. Display add on -------------------------------------------------------
  
  # ggplot main 
  if(plot_type %in% c("density", "ridgeplot", "histogram")){
    
    ggplot_main <- 
      ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = values))
    
  } else if(plot_type == "ridgeplot"){
    
    ggplot_main <- 
      ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = values, y = .data[[across]]))
    
  } else if(plot_type %in% c("violinplot", "boxplot")){
    
    ggplot_main <- 
      ggplot2::ggplot(data = data, mapping = ggplot2::aes(x = .data[[across]], y = values))
    
  }
  
  # ggplot geom
  if(plot_type == "histogram"){
    
    display_add_on <-
      list(
        ggplot2::geom_histogram(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                                color = "black", binwidth = binwidth,
                                data = data),
        ggplot2::labs(y = NULL)
      )
    
  } else if(plot_type == "density"){
    
    display_add_on <-
      list(
        ggplot2::geom_density(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                              color = "black", data = data,alpha = 0.825),
        ggplot2::labs(y = "Density")
      )
    
  } else if(plot_type == "ridgeplot"){
    
    reverse <- TRUE
    
    display_add_on <-
      list(
        ggridges::geom_density_ridges(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                                      color = "black", data = data, alpha = 0.825),
        ggplot2::labs(y = across, x = NULL)
        
      )
    
  } else if(plot_type == "violinplot"){
    
    display_add_on <-
      list(
        ggplot2::geom_violin(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                             color = "black", data = data),
        ggplot2::labs(y = NULL, x = across)
      )
    
  } else if(plot_type == "boxplot"){
    
    display_add_on <-
      list(
        ggplot2::geom_boxplot(mapping = ggplot2::aes(fill = !!rlang::sym(across)),
                              color = "black", data = data),
        ggplot2::labs(y = NULL, x = across)
      )
    
  }
  
  if(base::length(variables) > 1){
    
    facet_add_on <-
      list()
    
  } else {
    
    facet_add_on <- NULL
    
  }
  
  # -----
  
  
  # 4. Statistic add on -----------------------------------------------------
  
  max_value <- base::max(data[["values"]], na.rm = TRUE)
  labels_y <- NULL
  n_variables <- dplyr::n_distinct(data[["variables"]])
  
  # pairwise statistics
  
  if(n_variables == 1 & plot_type %in% testable_plottypes){
    
    if(test_pairwise %in% c("t.test", "wilcox.test")){
      
      comparison_list <- 
        ggpubr_comparison_list(ref.group = ref_group, groups = base::levels(data[[across]]))
      print(comparison_list)
      
      labels_y <- ggpubr_y_labels(input.list = comparison_list, max.value = max_value)

      pairwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test_pairwise, 
          comparisons = comparison_list, 
          label.y = labels_y, 
          data = data
        )
      )
      
      
    } else if(test_pairwise == "none") {
      
      if(base::isTRUE(verbose)){base::message("Skip pairwise testing.")}
      
      pairwise_add_on <- list()
      
    } else if(base::is.character(test_pairwise)){
      
      base::warning("Invalid input for argument 'test_pairwise'.")
      
    }
    
    # groupwise statistic
    if(test_groupwise %in% c("anova", "kruskal.test")){
      
      if(base::is.null(labels_y)){
        
        label_y <- max_value*1.1
        
      } else if(base::is.numeric(labels_y)){
        
        label_y <- base::max(labels_y, na.rm = TRUE)*1.1
        
      }
      
      groupwise_add_on <- list(
        ggpubr::stat_compare_means(
          method = test_groupwise, 
          label.y = label_y, 
          data = data
        )
      )
      
    } else if(test_groupwise == "none"){
      
      if(base::isTRUE(verbose)){base::message("Skip groupwise testing.")}
      
      groupwise_add_on <- list()
      
    } else {
      
      base::warning("Invalid input for argument 'test_groupwise'.")
      
      groupwise_add_on <- list()
      
    }
    
  } else {
    
    pairwise_add_on <- list()
    groupwise_add_on <- list()
    
    base::warning(ct_warnings$stat_test_requirements)
    
  }
  
  # -----
  
  
  # 5. Jitter add on  -------------------------------------------------------
  
  if(base::isTRUE(display_points) & plot_type %in% testable_plottypes){
    
    jitter_data <- 
      dplyr::group_by(.data = data, !!rlang::sym(across)) %>% 
      dplyr::slice_sample(n = n_cells)
    
    if(base::is.character(shape_to)){
      
      jitter_add_on <- 
        ggplot2::geom_jitter(
          data = jitter_data, size = pt_size, alpha = pt_alpha,
          color = pt_color, mapping = ggplot2::aes(shape = .data[[shape_to]])
        )
      
    } else {
      
      jitter_add_on <- 
        ggplot2::geom_jitter(
          data = jitter_data, size = pt_size, alpha = pt_alpha, 
          color = pt_color, height = 0.25, width = 0.25
        )
    }
    
    
  } else {
    
    jitter_add_on <- list()
    
  }
  
  
  # -----
  
  
  
  
  # 6. Plotting -------------------------------------------------------------
  
  ggplot_main +
    display_add_on +
    ggplot2::facet_wrap(facets = . ~ variables, ...) +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete",
                                clrp = clrp, guide = ggplot2::guide_legend(reverse = reverse)) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(color = "black"),
      axis.text.x = ggplot2::element_text(color = "black"),
      strip.text.y = ggplot2::element_text(angle = 0, face = "italic", size = 14),
      strip.placement = "outside",
      strip.background = ggplot2::element_rect(color = "white", fill = "white"),
      panel.spacing.y = ggplot2::unit(10, "pt")
    ) +
    ggplot2::labs(x = NULL) + 
    hlpr_caption_add_on(object = object, phase = phase) + 
    pairwise_add_on +
    groupwise_add_on + 
    jitter_add_on
  
  
  
}


#' @title Distribution of discrete features
#'
#' @description Visualize the distribution of discrete features.
#'
#' @inherit check_object params
#' @param features Character vector. Denotes the discrete variables whoose distribution 
#' is to be visualized.  
#' @param feature_compare Character vector or NULL. The discrete feature you want to compare the
#' features of \code{features} to.
#' @inherit colors params
#' @param position Character value. Given to \code{position} of \code{ggplot2::geom_bar()}. One of
#' \emph{'stack', 'dodge'} or \emph{'fill'}.
#' @param ... Additional parameters given to \code{ggplot2::facet_wrap()}.
#' @inherit ggplot_return return
#' 
#' @inherit plotDistribution params
#'
#' @export

plotDistributionDiscrete <- function(object,
                                     phase = "first_tmt",
                                     features,
                                     feature_compare = NULL,
                                     clrp = "milo",
                                     position = "fill",
                                     ...){
  
  # 1. Control --------------------------------------------------------------
  
  
  # ----
  
  
  # Additional checks and data extraction -----------------------------------
  
  if(base::is.character(feature_compare)){
    
    all_features <- c(features, feature_compare)
    facet_add_on <- list(ggplot2::facet_wrap(facets = . ~ features, scales = "free_x"))
    fill <- feature_compare
    theme_add_on <- list()
    
  } else {
    
    all_features <- features
    
    facet_add_on <- list(ggplot2::facet_wrap(facets = . ~ features, scales = "free_x", ...))
    
    if(base::length(all_features) > 1){
      
      fill = "features"
      
    } else {
      
      fill = "values"
      
    }
    
    theme_add_on <- list(ggplot2::theme(legend.position = "none"))
    
    if(position == "fill" & base::length(all_features) > 1){
      
      position <- "stack"
      
      base::warning("Argument 'feature_compare' is NULL. Using 'stack' for argument 'position'.")
      
    }
    
  }
  
  
  plot_df <-
    getStats(object, phase = phase) %>% 
    tidyr::pivot_longer(data = .,
                        cols = dplyr::all_of(features),
                        names_to = "features",
                        values_to = "values")
  
  # ----
  
  ggplot2::ggplot(data = plot_df) +
    ggplot2::geom_bar(position = position, color = "black",
                      mapping = ggplot2::aes(x = values, fill = .data[[fill]])) +
    facet_add_on +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = clrp) +
    ggplot2::theme_classic() +
    theme_add_on +
    ggplot2::theme(strip.background = ggplot2::element_blank()) +
    ggplot2::labs(y = NULL, x = "Groups / Clusters")
  
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
                             color_to = NULL,
                             scales = "free"){
  
  track_df <-
    getTracks(object = object) %>% 
    dplyr::filter(cell_id %in% {{cell_ids}}) 
  
  start_df <- 
    dplyr::group_by(.data = track_df, cell_id) %>% 
    dplyr::filter(frame_num == base::min(frame_num))
  
  if(base::is.character(color_to)){
    
    mapping <- ggplot2::aes(x = x_coords, y = y_coords, color = !!rlang::sym(color_to))
    
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
    hlpr_subset_across(data = getTracks(object,
                                        phase = phase,
                                        phase_cluster = phase_cluster, 
                                        verbose = verbose),
                       across = across, 
                       across_subset = across_subset) %>% 
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
                          well_plate = "",
                          color_to = NULL, 
                          fill_to = "cl_condition"){
  
  wp_df <- object@wp_info[[well_plate]]
  
  pt_size <- 13.5
  pt_stroke <- 2
  
  border <- 0.75
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(base::is.character(color_to)){
    
    mapping <- 
      ggplot2::aes(fill = .data[[fill_to]], color = .data[[color_to]])
    
  } else {
    
    mapping <- 
      ggplot2::aes(fill = .data[[fill_to]])
    
  }
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    ggplot2::geom_point(data = wp_df, mapping = mapping,
      size = pt_size, shape = 21, alpha = 1, stroke = pt_stroke, 
    ) + 
    ggplot2::geom_text(mapping = ggplot2::aes(label = well)) +
    ggforce::geom_mark_rect(
      mapping = ggplot2::aes(x = col_num, y = row_num, color = group), 
      color = "black", size = 1, expand = ggplot2::unit(15, "mm")
    ) +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21)), 
      fill = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
      )
  
  
}




# Clustering  -------------------------------------------------------------

#' @title Plot Medoid/Cluster Information 
#' 
#' @description Visualizes the characteristics of every medoid (cluster).
#'
#' @inherit check_object params 
#' @inherit phase_single params 
#' @inherit colors params 
#' @inherit check_pam_input params 
#' @param flip_coords Logical. Reverses x- and y-axis 
#' @param ... Additional arguments given to \code{ggplot2::facet_wrap()}.
#'
#' @inherit ggplot_return return 
#' @export

plotPamMedoidSummary <- function(object,
                                 k,
                                 phase = "first_tmt",
                                 clrp = "milo",
                                 flip_coords = FALSE,
                                 ...){
  
  # Control -----------------------------------------------------------------
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  # -----
  
  pam_obj <- getPamObject(object, k = k, phase = phase)
  
  pam_medoids <- pam_obj$medoids
  
  variables <- base::colnames(pam_medoids)
  
  shifted_data <- 
    base::scale(pam_medoids) %>% 
    scales::rescale(to = c(0,1)) %>% 
    base::as.data.frame() %>% 
    dplyr::mutate(Cluster = base::factor(dplyr::row_number())) %>% 
    tidyr::pivot_longer(
      cols = dplyr::all_of(x = variables), 
      names_to = "Variables", 
      values_to = "Values"
    )
  
  ggplot2::ggplot(data = shifted_data, mapping = ggplot2::aes(x = Variables, y = Values)) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = Cluster), color = "black", stat = "identity") + 
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(facets = ~ Cluster, ...) + 
    hlpr_coords_flip_add_on(flip_coords) +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = clrp) + 
    ggplot2::labs(y = NULL, x = NULL)
  
}


#' @title Plot Cluster Summary
#' 
#' @description Visualizes the cluster information slot of the 
#' pam object. 
#'
#' @inherit plotPamMedoidSummary params return 
#'
#' @export
#'

plotPamClusterSummary <- function(object,
                                  k,
                                  phase = "first_tmt",
                                  clrp = "milo",
                                  flip_coords = FALSE, 
                                  ...){
  
  # Control -----------------------------------------------------------------
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  # -----
  
  pam_obj <- getPamObject(object, k = k, phase = phase)
  
  pam_cluster <- pam_obj$clusinfo
  
  variables <- base::colnames(pam_cluster)
  
  shifted_data <- 
    base::as.data.frame(pam_cluster) %>% 
    dplyr::mutate(Cluster = base::factor(dplyr::row_number())) %>% 
    tidyr::pivot_longer(
      cols = dplyr::all_of(x = variables), 
      names_to = "Variables", 
      values_to = "Values"
    ) %>% 
    dplyr::filter(Variables != "size")
  
  ggplot2::ggplot(data = shifted_data, mapping = ggplot2::aes(x = Variables, y = Values)) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = Cluster), color = "black", stat = "identity") + 
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(facets = ~ Cluster, ...) + 
    hlpr_coords_flip_add_on(flip_coords) + 
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = clrp) + 
    ggplot2::labs(y = NULL, x = NULL)
  
}


#' @title Plot Number of Cells in Cluster
#' 
#' @description Visualizes the number of cells in each cluster.  
#'
#' @inherit plotPamMedoidSummary params return 
#'
#' @export
#'

plotPamClusterCount <- function(object,
                                k,
                                phase = "first_tmt",
                                clrp = "milo",
                                flip_coords = FALSE,
                                ...){
  
  # Control -----------------------------------------------------------------
  
  phase <- check_phase(object, phase, max_phases = 1)
  
  # -----
  
  cluster_name <- stringr::str_c("pam_k", k, sep = "")
  
  cluster_df <- 
    getClusterData(object, phase = phase) %>% 
    dplyr::select(dplyr::all_of(x = c("cell_id", cluster_name)))
  
  ggplot2::ggplot(data = cluster_df, mapping = ggplot2::aes(x = .data[[cluster_name]])) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[cluster_name]]), color = "black") + 
    ggplot2::theme_classic() + 
    hlpr_coords_flip_add_on(flip_coords) +
    ggplot2::labs(x = "Cluster", y = "Number of Cells") + 
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = clrp)
  
}


#' @title Plot Cluster Quality 
#' 
#' @description Visualizes the cluster quality via the average silhouette Width.   
#'
#' @inherit plotPamMedoidSummary params return 
#' @param display_average Logical. If set to TRUE the average across all clusters 
#' is displayed via a horizontal, dashed line. 
#'
#' @export
#'

plotPamSilWidth <- function(object,
                            k,
                            phase = "first_tmt",
                            clrp = "milo",
                            display_average = TRUE){
  
  pam_obj <-
    getPamObject(object, k = k, phase = phase)
  
  # extract silinfo data.frame
  sil_info_df <- 
    base::as.data.frame(pam_obj$silinfo$widths) %>% 
    dplyr::mutate(
      cells = dplyr::row_number(), 
      cluster = forcats::as_factor(x = cluster)
    ) %>% 
    dplyr::group_by(cluster)
  
  avg_sil_width <- base::round(pam_obj$silinfo$avg.width, 2)
  
  if(base::isTRUE(display_average)){
    
    avg_width_hline <- 
      ggplot2::geom_hline(
        yintercept = avg_sil_width, 
        linetype = "dashed"
      )
    
  } else {
    
    avg_width_hline <- base::list()
    
  }
  
  # plot 
  ggplot2::ggplot(data = sil_info_df, mapping = ggplot2::aes(x = cells, y = sil_width)) + 
    ggplot2::geom_bar(stat = "identity", mapping = ggplot2::aes(fill = cluster)) + 
    ggplot2::geom_hline(yintercept = 0) + 
    avg_width_hline +
    confuns::scale_color_add_on(aes = "fill", variable = "discrete", clrp = clrp) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.line.x = ggplot2::element_blank(), 
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(), 
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_text(face = "bold", size = 12.5),
      legend.title = ggplot2::element_text(face = "bold", size = 12.5),
      legend.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", size = 16.5),
      plot.subtitle = ggplot2::element_text(size = 10)
    ) + 
    ggplot2::labs(x = NULL, y = NULL, fill = "Cluster", 
                  subtitle = glue::glue("Avg. Silhouette Width: {avg_sil_width}")
    )
  
}
