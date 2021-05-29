
#' @title ggplot_return
#' @return A ggplot. 
ggplot_return <- function(){}



#' @title ggplot_return
#' @return A ggplot. 
ggplot_family <- function(){}




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
    getStatsDf(object,
               with_cluster = TRUE, 
               with_meta = TRUE,
               with_well_plate = TRUE,
               verbose = FALSE,
               phase = phase)
  
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
    ggplot2::labs(y = "Count", x = NULL) + 
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
#' @details Argument \code{across} can be specified of a character vector of length 2. If so, argument 
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
    getStatsDf(
      object = object,
      phase = phase,
      with_meta = TRUE, 
      with_cluster = TRUE,
      with_well_plate = TRUE,
      verbose = FALSE) %>% 
    dplyr::select(-cell_id)
  
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
                          size_well = 13.5, 
                          display_labels = TRUE,
                          clrp_adjust = NULL,
                          make_pretty = NULL, 
                          ...){
  
  check_object(object)
  assign_default(object)
  
  if(base::is.null(well_plate)){
    
    well_plate <- getWellPlateNames(object)[1]
    
  }
  
  wp_df <- object@well_plates[[well_plate]]$wp_df_eval
  
  pt_size <- size_well
  pt_stroke <- 2
  
  border <- 0.75
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(multiplePhases(object) && base::is.character(color_by) && color_by == "condition"){
    
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
    
    facet_add_on <- ggplot2::facet_wrap(facets = . ~ phases, ...)
    
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
  
  if(base::isTRUE(display_labels)){
    
    text_add_on <- ggplot2::geom_text(mapping = ggplot2::aes(label = well))
    
  } else {
    
    text_add_on <- NULL
    
  }
  
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    ggplot2::geom_point(data = wp_df, mapping = ggplot2::aes(fill = .data[[color_by]]),
      size = pt_size, shape = 21, alpha = 1, stroke = pt_stroke, 
    ) + 
    text_add_on +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 10, shape = 21)), 
      fill = ggplot2::guide_legend(override.aes = list(size = 10, shape = 21))
      ) + 
    confuns::scale_color_add_on(
      aes = "fill", variable = wp_df[[color_by]], clrp = "milo", 
      clrp.adjust = c(clrp_adjust, "unknown" = "lightgrey", "unknown & unknown" = "lightgrey")
      ) + 
    ggplot2::labs(
      fill = confuns::make_capital_letters(string = color_by, capital.letters = make_pretty)
    ) + 
    facet_add_on + 
    border_add_on
  
  
}



