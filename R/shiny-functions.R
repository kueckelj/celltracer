#' @title Display loading status
#' 
#' @description Creates a data.frame that displays information 
#' about the file availability evaluation undergone for every well
#' plate.
#'
#' @param all_wp_lists A list of well plate lists. 
#' 
#' @usage In shiny-module \code{moduleLoadData}.
#'

loading_status_table_shiny <- function(all_wp_lists){
  
  well_plates <- base::names(all_wp_lists)
  
  directories <- purrr::map_chr(.x = all_wp_lists, .f = hlpr_wp_directories)
  
  num_files <- purrr::map_int(.x = all_wp_lists, hlpr_wp_file_number)
  
  num_ambiguous <- purrr::map_int(.x = all_wp_lists, .f = hlpr_wp_ambiguous_number)
  
  num_files_expected <- purrr::map_int(.x = all_wp_lists, hlpr_wp_exp_file_number)
  
  status_df <- 
    base::data.frame(
      "wp" = well_plates, 
      "asd" = directories, 
      "nof" = num_files, 
      "enof" = num_files_expected,
      "noaf" = num_ambiguous
    ) %>% 
    dplyr::mutate(
      "ready" = dplyr::case_when(
        asd == "No directory assigned" ~ "No", 
        nof == 0 ~ "No", 
        noaf != 0 ~ "No", 
        TRUE ~ "Yes"
      )
    ) %>% 
    dplyr::rename(
      "Well Plate" = wp,
      "Assigned Directory" = asd,
      "Number of Files" = nof,
      "Expected Number of Files" = enof,
      "Number of Ambiguous Directories" = noaf, 
      "Ready to load" = ready
    )
  
}


#' @title Reads cell track data
#' 
#' @description A function to be used for argument \code{.f} of \code{purrr::map_df}.
#' Calls the respective reading function depending on the directory's ending. 
#'
#' @param directory Character value. The directory from which to read the data.frame.
#' of \code{.x} contains.
#' @param progress A shiny - R6 progress object. 
#' @param progress_n Numeric value. Indicates the iterating progress.
#'
#' @return The read in data.frame with three additional informative columns: \emph{well_image, condition}
#' and \emph{cell_line}.

read_ct_data_shiny <- function(directory, progress_n, progress){
  
  progress$set(value = progress_n)
  
  if(stringr::str_detect(string = directory, pattern = "csv$")){
    
    track_df <- readr::read_csv(file = directory)
    
  } else if(stringr::str_detect(string = directory, pattern = "xls$")){
    
    track_df <- readxl::read_xls(path = directory)
    
  } else if(stringr::str_detect(string = directory, pattern = "xlsx$")){
    
    track_df <- readxl::read_xlsx(path = directory)
    
  }
  
  track_df[base::is.na(track_df)] <- 0
  
  well_image <-
    stringr::str_extract(string = directory, pattern = file_regex) %>% 
    stringr::str_extract(pattern = well_image_regex)
  
  missing_columns <- 
    purrr::map(.x = original_ct_variables, 
               .f = check_cell_track_variables,
               df = track_df) %>%
    purrr::discard(.p = base::is.null) %>% 
    purrr::flatten_chr()
  
  n_missing_cols <- base::length(missing_columns)
  
  if(n_missing_cols > 0){
    
    ref <- base::ifelse(n_missing_cols > 1, "columns", "column")
    ref_cols <- stringr::str_c(missing_columns, collapse = "', '")
    
    base::stop(glue::glue("Missing {ref}: '{ref_cols}'"))
    
  }
  
  df_return <-
    hlpr_rename_cell_track_cols(track_df = track_df) %>% 
    dplyr::mutate(well_image = {{well_image}}) 
  
  list_return <- 
    list("track_df" = df_return, 
         "well_image" = well_image, 
         "directory" = directory)
  
  base::return(list_return)
  
}


#' @title Read in cell track data
#' 
#' @description Reads in excel- and csv-files of all valid 
#' directories while displaying the progress via a progress bar.
#'
#' @param wp_list A well plate list.
#' @param wp_name The name of the respective well plate.
#'
#' @return A nested list obtained by \code{purrr::safely()} with 
#' successfully loaded files and failed ones including the error message.
#' 
#' @export

load_cell_track_files_shiny <- function(wp_list, wp_name, session){
  
  directories <- wp_list$valid_directories
  
  well_image_files <- stringr::str_extract(string = directories, pattern = file_regex)
  
  num_files <- base::length(directories)
  
  # set up progress bar
  progress <- shiny::Progress$new(session, min = 1, max = num_files)
  base::on.exit(progress$close())
  
  progress$set(message = glue::glue("Reading data for well plate '{wp_name}' :"), 
               detail = glue::glue("Total of {num_files} files."))
  
  # read files
  data_list <- 
    purrr::map2(.x = directories,
                .y = base::seq_along(directories),
                .f = purrr::safely(read_ct_data_shiny),
                progress = progress) %>% 
    purrr::set_names(nm = well_image_files)
  
  # sort successfull and failed loading 
  track_data_list <- 
    list("successful" = purrr::keep(.x = data_list, .p = ~ base::is.null(.x[["error"]])),
         "failed" = purrr::keep(.x = data_list, .p = ~ !base::is.null(.x[["error"]])))
  
  base::return(track_data_list)
  
}


#' @title Integrate cell track tables
#' 
#' @description Subsets the successfully loaded files 
#'
#' @param track_data_list The output of \code{load_cell_track_files_shiny}.
#'
#' @return A track data.frame

assemble_track_df_shiny <- function(track_data_list, all_wp_lists){
  
  track_df_list <- 
    purrr::map(.x = track_data_list, ~.x[["successful"]]) %>% 
    purrr::map(.x = ., .f = ~ purrr::map(.x = .x, "result")) %>% 
    purrr::map(.x = ., .f = ~ purrr::map_df(.x = .x, "track_df"))
  
  all_data_list <- 
    list(track_df_list,
         all_wp_lists,
         base::seq_along(track_df_list), 
         base::names(track_df_list)
         )
  
  track_df <- 
    purrr::pmap_dfr(.l = all_data_list, .f = hlpr_assemble_track_df)
  
  cell_lines <- track_df$cell_line %>% base::unique() %>% base::sort()
  conditions <- track_df$condition %>% base::unique() %>% base::sort()
  cl_conditions <- track_df$cl_condition %>% base::unique() %>% base::sort()
  
  final_track_df <-
    dplyr::mutate(.data = track_df, 
                  cell_line = base::factor(x = cell_line, levels = cell_lines), 
                  condition = base::factor(x = condition, levels = conditions), 
                  cl_condition = base::factor(x = cl_condition, levels = cl_conditions))
  
  base::return(track_df)
  
}


#' @title Show shiny - notifications
#'
#' @param in_shiny Logical value. 
#' @param ui Given to \code{shiny::showNotification()}.
#' @param type Given to \code{shiny::showNotification()}.
#' @param ... More arguments given \code{shiny::showNotification()}.
#'
#' @return A shiny notification.

shiny_fdb <- function(in_shiny, ui, type = "message", ...){
  
  if(base::isTRUE(in_shiny)){
    
    shiny::showNotification(ui = ui, type = type, ...)
    
  }
  
}


#' @title Summarize tracking quality
#'
#' @param track_df A track data.frame

quality_check_summary_shiny <- function(track_df){
  
  dplyr::group_by(.data = track_df, cell_id, cl_condition) %>% 
    dplyr::summarise(
      last_meas = base::max(frame), 
      first_meas = base::min(frame), 
      total_meas = dplyr::n(), 
      skipped_meas = base::length(first_meas:last_meas) - total_meas
    )
  
}


#' @title Visualize the well plate
#'
#' @param wp_df A well-plate data.frame.
#' @param selected_wells_df A subsetted well-plate data.frame or NULL.
#' @param aes_fill Character value. Variable of \code{wp_df} to map on the 
#' fill-aesthetic.
#' @param aes_color Character value. Variable of \code{wp_df} to map on the 
#' color-aesthetic.
#' @param pt_size Numeric value. Size of points that represent the wells. 
#' @param pt_stroke Numeric value. Size of border that indicates what has been 
#' mapped on the color-aesthetic.
#' @param border Numeric value. Distance between the outer wells and the plate's
#' margin.
#'
#' @inherit ggplot_return return
#'

plot_well_plate_shiny <- function(wp_df,
                                  selected_wells_df = NULL,
                                  aes_fill,
                                  fill_values, 
                                  aes_color,
                                  color_values,
                                  pt_size = 13.5,
                                  pt_stroke = 2,
                                  border = 0.75){
  
  limit_x <- base::max(wp_df$col_num) + border
  limit_y <- base::max(wp_df$row_num) + border
  
  if(base::is.data.frame(selected_wells_df)){
    
    geom_point_add_on <-  
      ggplot2::geom_point( data = selected_wells_df, fill = "red",
                           size = pt_size, shape = 21, alpha = 1,
                           stroke = pt_stroke ) 
    
  } else {
    
    geom_point_add_on <- NULL
    
  }
  
  # plot output
  ggplot2::ggplot(data = wp_df, mapping = ggplot2::aes(x = col_num,y = row_num)) + 
    ggplot2::geom_point(
      data = wp_df, 
      mapping = ggplot2::aes(fill = .data[[aes_fill]], color = .data[[aes_color]]),
      size = pt_size, shape = 21, alpha = 1, stroke = pt_stroke, 
    ) + 
    geom_point_add_on +
    ggplot2::geom_text(mapping = ggplot2::aes(label = well)) +
    ggforce::geom_mark_rect(
      mapping = ggplot2::aes(x = col_num, y = row_num, color = group), 
      color = "black", size = 1, expand = ggplot2::unit(15, "mm")
    ) +
    ggplot2::scale_x_continuous(limits = c(-border, limit_x)) +
    ggplot2::scale_y_reverse(limits = c(border + limit_y, -border)) +
    ggplot2::theme_void() +
    ggplot2::scale_color_manual(values = color_values, drop = FALSE) +
    ggplot2::scale_fill_manual(values = fill_values, drop = FALSE) +
    ggplot2::guides(
      color = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21)), 
      fill = ggplot2::guide_legend(override.aes = list(size = 15, shape = 21))
    )
  
  
}

#' @title Quality check histogram
#'
#' @param track_summary_df Output of \code{quality_check_summary()}
#' @param aes_x Character value. 
#' @param aes_fill Character value.
#' @param lab_x Character value.
#' @param lab_fill Character value.

plot_qc_histogram_shiny <- function(track_summary_df, 
                                 aes_x = "skipped_meas", 
                                 lab_x = "Measurements",
                                 legend_position = "none"){
  
  labels_breaks <-
    dplyr::pull(track_summary_df, var = {{aes_x}}) %>% 
    base::unique()
  
  ggplot2::ggplot(data = track_summary_df, mapping = ggplot2::aes(x = .data[[aes_x]])) + 
    ggplot2::geom_histogram(position = "stack", color = "black", binwidth = 1) + 
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey"), 
      axis.text.x = ggplot2::element_text(vjust = 5), 
      plot.title = ggplot2::element_text(face = "bold", size = 15), 
      legend.position = "none"
    ) + 
    ggplot2::scale_x_continuous(labels = labels_breaks, breaks = labels_breaks) +
    ggplot2::labs(x = lab_x, y = NULL)
  
}


#' @title Quality check barplot
#'
#' @param df A data.frame 

plot_qc_barplot_shiny <- function(df, aes_x, aes_fill, bar_position){
  
  ggplot2::ggplot(data = df, mapping = ggplot2::aes(x = forcats::fct_infreq(.data[[aes_x]]))) + 
    ggplot2::geom_bar(mapping = ggplot2::aes(fill = .data[[aes_fill]]),
                      color = "black", position = bar_position) + 
    ggplot2::theme_minimal() + 
    ggplot2::labs(x = NULL, y = NULL, fill = NULL,
                  subtitle = stringr::str_c("n = ", base::nrow(df)))
  
}
