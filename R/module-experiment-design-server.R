
#' @title Server logic: Experiment Design
#' 
#' @description Server logic to be used as input for \code{module}-argument
#' of function \code{shiny::moduleServer()}.
#'
#' @param id Namespace ID
#'
#' @return A reactive and named list: 
#' 
#'  \describe{
#'   \item{\emph{set_up}}{Information about the experiments set up (well_plates,
#'                        number of measurements, time interval, etc.)}
#'   \item{\emph{background_info}}{Information about the responsible researchers,
#'                                 the data, the idea etc.}
#'   } 
#' 
#' @export

moduleExperimentDesignServer <- function(id){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){
  
# Reactive values ---------------------------------------------------------
  
  # a list again containing a list for each set up well plate
  all_well_plate_lists <- shiny::reactiveVal(value = list())
  well_plate_df <- shiny::reactiveVal(value = data.frame())
  
  # list containing all the information regarding the experiment's design
  # - the return value of this module
  ed_list <- shiny::reactiveValues(
    
    set_up = list(), 
    background_info = list(), 
    proceed = base::numeric()
    
  )
  
  # -----  
  
# Render UIs --------------------------------------------------------------
  
  output$ed_treatment_start <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::selectInput(inputId = ns("ed_treatment_start"),
                       label = "Treatment Start:",
                       choices = c("No treatment", "From beginning",
                                   stringr::str_c(1:input$ed_meas_num * input$ed_meas_interval,
                                                  input$ed_interval_unit,
                                                  sep = " ")
                       )
    )
    
  })  
  
  output$ed_control_variable <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(input$ed_treatment_start)
    shiny::req(input$ed_treatment_start != "No treatment")
    
    shiny::validate(
      shiny::need(
        expr = !base::identical(all_well_plate_lists(), list()), 
        message = "No well plates have been added yet."
      )
    )
    
    choices <- 
      purrr::map(.x = all_well_plate_lists(),
                 .f = ~ dplyr::filter(.x[["wp_df"]], information_status == "Complete") %>% 
                        dplyr::pull(var = "condition")
                 ) %>%
      purrr::flatten_chr() %>%
      base::unique()
    
    if(input$ed_treatment_start != "No treatment"){
      
      shiny::selectInput(inputId = ns("ed_control_variable"), 
                         label = "Control Variable:", 
                         choices = choices, 
                         multiple = FALSE)
      
    }
    
  })
  
  output$ed_added_well_plates <- shiny::renderUI({
    
    ns <- session$ns
    
    shinyWidgets::pickerInput(inputId = ns("ed_added_well_plates"), 
                              label = NULL, 
                              choices = base::names(all_well_plate_lists()), 
                              multiple = TRUE)
    
  })
  
  # -----
  
# Observe events ----------------------------------------------------------
  
  # new well plate
  oe <- shiny::observeEvent(input$ed_new_well_plate, {
    
    # data.frame (obs => well)
    well_plate_df_new <- 
      setUpWellPlateDf(type = input$ed_well_plate)
    
    # update well_plate_df()
    well_plate_df(well_plate_df_new)
    
  })
  
  # add well information 
  oe <- shiny::observeEvent(input$ed_add_well_info, {
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::nrow(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # -
    
    new_cell_line <- input$ed_cell_line
    new_condition <- input$ed_condition
    
    selected_wells <- selected_wells()$well
    
    well_plate_df_new <- all_wells()
    well_plate_df_new$selected <- well_plate_df_new$well %in% selected_wells
    
    if(new_cell_line != ""){
      
      well_plate_df_new <- 
        dplyr::mutate(.data = well_plate_df_new, 
                      cell_line = dplyr::case_when(selected ~ {{new_cell_line}},
                                                   TRUE ~ cell_line)
        )
      
    }
    
    if(new_condition != ""){
      
      well_plate_df_new <- 
        dplyr::mutate(.data = well_plate_df_new, 
                      condition = dplyr::case_when(selected ~ {{new_condition}}, 
                                                   TRUE ~ condition)
        )
      
    }
    
    well_plate_df_final <- 
      dplyr::mutate(.data = well_plate_df_new, 
                    cl_condition = stringr::str_c(cell_line, condition, sep = " & "),
                    information_status = base::as.character(information_status),
                    information_status = dplyr::case_when(
                      condition == "unknown" & cell_line == "unknown" ~ "Missing", 
                      condition != "unknown" & cell_line != "unknown" ~ "Complete", 
                      TRUE ~ "Incomplete"), 
                    information_status = base::factor(x = information_status,
                                                      levels = c("Complete", "Incomplete", "Missing"))
      ) %>% 
      dplyr::select(-selected)
    
    
    # update well_plate_df()
    well_plate_df(well_plate_df_final)
    
  })
  
  # delete well information 
  oe <- shiny::observeEvent(input$ed_delete_well_info, {
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::nrow(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # -
    
    well_plate_df_new <- 
      dplyr::mutate(.data = all_wells(), 
                    selected = well %in% selected_wells()$well, 
                    condition = dplyr::case_when(selected ~ "unknown", TRUE ~ condition),
                    cell_line = dplyr::case_when(selected ~ "unknown", TRUE ~ cell_line),
                    cl_condition = stringr::str_c(cell_line, condition, sep = " & "),
                    information_status = base::as.character(information_status),
                    information_status = dplyr::case_when(
                      condition == "unknown" & cell_line == "unknown" ~ "Missing", 
                      condition != "unknown" & cell_line != "unknown" ~ "Complete", 
                      TRUE ~ "Incomplete"), 
                    information_status = base::factor(x = information_status,
                                                      levels = c("Complete", "Incomplete", "Missing"))
      ) %>% 
      dplyr::select(-selected)
    
    
    # update well_plate_df()
    well_plate_df(well_plate_df_new)
    
    
  })
  
  # add well plate
  oe <- shiny::observeEvent(input$ed_add_well_plate, {
    
    wp_list_new <- all_well_plate_lists()
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    wp_df <- dplyr::mutate(.data = all_wells(), ipw = input$ed_images_per_well)
    
    # check input frame number per well
    checkpoint(evaluate = input$ed_images_per_well != 0, 
               case_false = "invalid_image_number")
    
    # check information status
    if(base::isFALSE(input$ed_dismiss_unknown)){
      
      check <- check_wp_df_shiny(wp_df = wp_df)
      
      checkpoint(evaluate = check$evaluate, 
                 case_false = "case_false", 
                 error_notifications = check, 
                 duration = 15) 
      
    } 
    
    # check well plate name & softwar
    wp_name <- input$ed_well_plate_name 
    new_name <- !wp_name %in% base::names(wp_list_new)
    valid_name <- !wp_name == ""
    
    checkpoint(evaluate = base::all(new_name, valid_name), 
               case_false = "invalid_wp_name")
    
    # add new well plate
    wp_list_new[[wp_name]] <-
      list("wp_df" = wp_df,
           "wp_colors" = ed_well_plate_colors())
    
    # update reactive values
    all_well_plate_lists(wp_list_new)
    well_plate_df(data.frame())
    
  })
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_save_ed, {
    
    checkpoint(evaluate = base::length(all_well_plate_lists()) != 0, 
               case_false = "no_well_plates_added")
    
    checkpoint(evaluate = (input$ed_software != "     "), 
               case_false = "missing_software")
    
    ed_list$set_up <-
      list("all_well_plate_lists" = all_well_plate_lists(),
           "nom" = input$ed_meas_num, 
           "itvl" = input$ed_meas_interval, 
           "itvl_u" = input$ed_interval_unit, 
           "tmt_start" = input$ed_treatment_start, 
           "software" = input$ed_software)
    
    ed_list$proceed <- input$ed_save_ed
    
    shiny_fdb(in_shiny = TRUE, ui = "Proceed below with 'Load Data'.")
    
  })
  
  # -----
  

# Reactive expressions ----------------------------------------------------

  # well data.frames ---
  all_wells <- shiny::reactive({
    
    well_plate_df()
    
  })
  
  selected_wells <- shiny::reactive({
    
    xmin <- input$well_plate_brush$xmin
    xmax <- input$well_plate_brush$xmax
    
    ymin <- input$well_plate_brush$ymin
    ymax <- input$well_plate_brush$ymax
    
    is_selected <-
      !base::any(
        base::is.null(xmin), 
        base::is.null(xmax), 
        base::is.null(ymin), 
        base::is.null(ymax)
       )
    
    if(base::isTRUE(is_selected)){
      
      selected_wells <- 
      dplyr::filter(.data = well_plate_df(),
                    dplyr::between(x = col_num, xmin, xmax), 
                    dplyr::between(x = row_num, ymin, ymax))
      
    } 
    
    base::return(selected_wells)
    

    
  })
  
  # ---
  
  # well plate visualization ---
  
  ed_well_plate_colors <- shiny::reactive({
    
    # make sure that "unkown & unknown" are displayed in grey
    color_to_vars <- base::unique(all_wells()[[input$ed_color_to]])
    n_color_to_vars <- base::length(color_to_vars)
    
    colors_named <- 
      magrittr::set_names(x = colors_unnamed[1:n_color_to_vars], 
                          value = color_to_vars) %>% 
      base::append(x = colors_grey, 
                   values = .)
    
    base::return(colors_named)
    
  })
  
  ed_well_plate_plot <- shiny::reactive({
    
    shiny::validate(
      shiny::need(expr = !base::identical(all_wells(), base::data.frame()), 
                  message = "No well plate chosen.")
      )
    
    plot_well_plate_shiny(wp_df = all_wells(), 
                          selected_wells_df = selected_wells(), 
                          aes_fill = input$ed_color_to, 
                          aes_color = "information_status",
                          color_values = status_colors, 
                          fill_values = ed_well_plate_colors()) +
      ggplot2::labs(color = "Information Status") 
    
  })
  
  # ---
  
  # -----
  
  
# Plot outputs ------------------------------------------------------------
  
  output$ed_well_plate_plot <- shiny::renderPlot({
    
    ed_well_plate_plot()
    
  })
  
  # -----
  
# Table outputs -----------------------------------------------------------
  
  output$ed_well_plate_folders <- DT::renderDataTable({
    
    shiny::validate(
      shiny::need(
        expr = base::names(all_well_plate_lists()), 
        message = "No well plates have been added yet."
      )
    )
    
    data.frame(
      "Name" = base::names(all_well_plate_lists()),
      "Type" = purrr::map_chr(.x = all_well_plate_lists(), ~ base::unique(.x[["wp_df"]][["type"]])), 
      "Frames per Well" = purrr::map_dbl(.x = all_well_plate_lists(), ~ base::unique(.x[["wp_df"]][["ipw"]]))
    )
    
  })
  
# Text outputs ------------------------------------------------------------
  
  output$ed_chosen_dir <- shiny::renderText({
    
    well_plate_dir()
    
  })
  
# Module return value -----------------------------------------------------
  
  return_value <- shiny::reactive({ 
    
    rv <- 
    list(set_up = ed_list$set_up, 
         proceed = ed_list$proceed)
    
    assign(x = "rv_experiment_design", value = rv, .GlobalEnv)
    
  })
  
  base::return(return_value)
  
  })

}




