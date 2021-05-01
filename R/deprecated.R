

# EXPORTED ----------------------------------------------------------------

#' @title Plot descriptive statistics
#' 
#' @description These functions are deprecated in favor of \code{plotDensityplot(),
#' plotHistogram(), plotRidgplot(), plotBoxplot() and plotViolinplot()}.
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
  
  base::warning("plotDistribution() and plotDistributionDiscrete() are deprecated in favor of 
                plot<plot_type>() (e.g. plotViolinplot(), plotBarpolot())")
  
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
#' @description This function is deprecated in favor of \code{plotBarchart()}.
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
  
  base::warning("plotDistribution() and plotDistributionDiscrete() are deprecated in favor of 
                plot<plot_type>() (e.g. plotViolinplot(), plotBarpolot())")
  
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











# NOT EXPORTED ------------------------------------------------------------


getGroups <- function(object, option){
  
  warning("Deprecate this function!")
  
  group_vec <- 
    getMeta(object) %>% 
    dplyr::pull(var = {{option}}) 
  
  if(base::is.factor(group_vec)){
    
    base::levels(x = group_vec)
    
  } else if(base::is.character(group_vec)){
    
    base::unique(group_vec)
    
  } else {
    
    base::stop(glue::glue("The result of grouping option '{option}' must be a character vector or a factor."))
    
  }
  
}




