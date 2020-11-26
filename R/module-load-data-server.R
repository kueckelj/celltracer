#' @title Server logic: Load Data
#' 
#' @description Server logic to be used as input for \code{module}-argument
#' of function \code{shiny::moduleServer()}.
#'
#' @param id Namespace ID
#' @param ed_input A reactive and named list. See value of \code{moduleExperimentSetUpServer()}.
#'
#' @return A named list: 
#' 
#' @export

moduleLoadDataServer <- function(id, ed_input){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      
      # Reactive values ---------------------------------------------------------
      
      all_wp_lists <- shiny::reactiveVal(value = list())
      all_wp_names <- shiny::reactiveVal(value = character())
      
      read_in_data <- shiny::reactiveVal(value = list())
      
      # list containing all the information regarding the experiment's design
      # - the return value of this module
      ld_output <- shiny::reactiveValues(
        
        track_df = list(),
        all_wp_lists = list(),
        proceed = numeric()
        
      )
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      output$ld_added_well_plates <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::validate(
          shiny::need(
            expr = all_wp_names(), 
            message = "No well plates have been added yet."
          )
        )
        
        
        shiny::tagList(
          #shiny::h5(shiny::strong("Added Well Plates:")),
          shiny::selectInput(inputId = ns("ld_added_well_plates"), 
                             label = NULL, 
                             choices = all_wp_names())
        )
        
      })
      
      output$ld_well_plate_errors <- shiny::renderUI({
        
        shiny::validate(
          shiny::need(
            expr = base::is.list(read_in_data()) & base::length(read_in_data()) != 0, 
            message = "No folders have been loaded yet."
          )
        )
        
        shiny::req(well_plates_with_errors())
        
        ns <- session$ns
        
        shinyWidgets::pickerInput(inputId = ns("ld_well_plate_errors"), 
                                  label = "Well Plate:", 
                                  choices = well_plates_with_errors(), 
                                  choicesOpt = list(
                                    subtext = stringr::str_c(
                                      "Errors", 
                                      well_plate_error_count(), 
                                      sep = ": "
                                    )
                                  )
        )
        
      })
      
      output$ld_well_image_errors <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::req(well_plates_with_errors())
        
        shinyWidgets::pickerInput(inputId = ns("ld_well_image_errors"), 
                                  label = "Failed files:", 
                                  choices = well_images_with_errors())
        
      })
      
      output$ld_ignore_errors <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::validate(
          shiny::need(
            expr = base::is.list(read_in_data()) & base::length(read_in_data()) != 0, 
            message = "No folders have been loaded yet."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = well_plates_with_errors(), 
            message = "No loading errors occured."
          )
        )
        
        shiny::checkboxInput(inputId = ns("ld_ignore_errors"), 
                             label = "Ignore errors", 
                             value = FALSE)
        
      })
      
      # -----
      
      # Observe events ----------------------------------------------------------

      # initiate data loading by clicking 'proceed' in previous module
      oe <- shiny::observeEvent(ed_input()$proceed, {

        all_wp_lists_new <- ed_input()$set_up$all_well_plate_lists
        
        # update well_plate_list
        all_wp_lists(all_wp_lists_new)
        all_wp_names(base::names(all_wp_lists_new))
        
      })
      
      # add new directory to well plate
      oe <- shiny::observeEvent(dir_string(), {
        
        # actual code !!!
        checkpoint(evaluate = !base::is.null(all_wp_lists()),
                   case_false = "no_set_up_saved")
        
        all_wp_lists_new <- all_wp_lists()
        wp_name <- input$ld_added_well_plates
        
        all_wp_lists_new[[wp_name]][["directory"]] <- dir_string()
        
        all_wp_lists_new[[wp_name]] <-
          evaluate_file_availability(
            wp_list = all_wp_lists_new[[wp_name]],
            recursive = input$ld_recursive, 
            keep = input$ld_keep_filetype
          )
        
        assign(x = "xlist", value = all_wp_lists_new, .GlobalEnv)
        
        check <- check_wp_directories(all_wp_lists = all_wp_lists_new)
        
        if(check != "unique"){
          
          message <-
            glue::glue("There are well plates that share their directories: '{check}'
                       Please assign an unambiguous directory to each well plate.")
          
          shiny::showNotification(
            ui = message, 
            type = "warning",
            duration = 20
          )
          
        }
        
        # update all_wp_lists and -names
        all_wp_lists(all_wp_lists_new)
        all_wp_names(base::names(all_wp_lists()))
        
      })
      
      # load data
      oe <- shiny::observeEvent(input$ld_load_data,{
        
        checkpoint(evaluate = base::all(loading_status()[["Ready to load"]] == "Yes"),
                   case_false = "well_plates_not_ready")
        
        data_list <- 
          purrr::map2(.x = all_wp_lists(),
                      .y = all_wp_names(),
                      .f = load_cell_track_files_shiny, 
                      session = session)
        
        shiny::showNotification(ui = "Reading done.", type = "message")
        
        assign("data_list", data_list, .GlobalEnv)
        
        # update read_in_data
        read_in_data(data_list)
        
      })
      
      # save and proceed 
      oe <- shiny::observeEvent(input$ld_proceed, {
        
        checkpoint(evaluate = base::is.list(read_in_data()) & base::length(read_in_data()) != 0,
                   case_false = "no_data_read_in")
        
        if(shiny::isTruthy(well_plates_with_errors())){
          
          checkpoint(evaluate = base::isTRUE(input$ld_ignore_errors),
                     case_false = "errors_left",
                     duration = 15)
          
        }
        
        ld_output$all_wp_lists <- all_wp_lists()
        
        ld_output$track_df <- track_df()
        
        ld_output$proceed <- input$ld_proceed
        
        shiny_fdb(in_shiny = TRUE, ui = "Proceed below with 'Quality Check'.")
        
        
      })
      
      # ----- 
      
      # Reactive expressions ----------------------------------------------------
      
      # directory handling ---
      
      # shinyFiles::shinyDirButton() - server
      
      dir_roots <- shinyFiles::getVolumes()
      
      shinyFiles::shinyDirChoose(input = input, 
                                 id = "ld_well_plate_dir", 
                                 session = session, 
                                 roots = dir_roots()
      )
      
      # assembled directory 
      dir_string <- shiny::reactive({ 
        
        shiny::validate(
          shiny::need(expr = base::is.list(x = input$ld_well_plate_dir), 
                      message = "No folder chosen.")
        )
        
        hlpr_assemble_directory(input_list = input$ld_well_plate_dir)
        
      })
      
      # ---
      
      # current well plate
      current_well_plate <- shiny::reactive({
        
        shiny::req(input$ld_added_well_plates)
        
        all_wp_lists()[[input$ld_added_well_plates]]
        
      })
      
      # current, evaluated well-plate data.frame ready to be plotted
      evaluated_wp_df <- shiny::reactive({
        
        shiny::req(input$ld_added_well_plates)
        
        shiny::validate(
          shiny::need(
            expr = base::is.data.frame(current_well_plate()[["wp_df_eval"]]), 
            message = "No folder has been chosen for this well plate.")
        )
        
        current_well_plate()[["wp_df_eval"]]
        
      })
      
      # loading status
      loading_status <- shiny::reactive({
        
        shiny::validate(
          shiny::need(
            expr = all_wp_names(), 
            message = "No well plates have been added yet."
          )
        )
        
        loading_status_table_shiny(all_wp_lists = all_wp_lists())
        
      })
      
      # well plate plot visualizes the file availability
      well_plate_plot <- shiny::reactive({
        
        plot_well_plate_shiny(wp_df = evaluated_wp_df(), 
                              selected_wells_df = NULL, 
                              aes_fill = "availability_status", 
                              aes_color = "availability_status", 
                              fill_values = ggplot2::alpha(status_colors, .5),
                              color_values = ggplot2::alpha(status_colors, .5)
        ) + 
          ggplot2::labs(fill = "File Availability") + 
          ggplot2::guides(color = FALSE)
        
      })
      
      # read in data error processing ---
      
      failed_list <- shiny::reactive({
        
        shiny::req(read_in_data())
        
        purrr::map(.x = read_in_data(), "failed") %>% 
          purrr::discard(.p = base::is.null) %>% 
          purrr::map(.f =  ~ purrr::map(.x = .x, "error"))
        
      })
      
      well_plates_with_errors <- shiny::reactive({
        
        base::names(failed_list())
        
      })
      
      well_plate_error_count <- shiny::reactive({
        
        shiny::req(failed_list())
        
        purrr::map_int(.x = failed_list(), .f = base::length) %>% 
          base::unname()
        
      })
      
      well_images_with_errors <- shiny::reactive({
        
        shiny::req(input$ld_well_plate_errors)
        
        base::names(failed_list()[[input$ld_well_plate_errors]])
        
      })
      
      well_image_error_message <- shiny::reactive({
        
        shiny::req(input$ld_well_image_errors)
        
        failed_list()[[input$ld_well_plate_errors]][[input$ld_well_image_errors]] %>% 
          base::as.character()
        
      })
      
      # ---
      
      # assemble track data.frame
      
      track_df <- shiny::reactive({
        
        assemble_track_df_shiny(track_data_list = read_in_data(),
                                all_wp_lists = all_wp_lists())
        
      })
      
      # -----
      
      # Plot outputs ------------------------------------------------------------
      
      # well plate 
      output$ld_well_plate_plot <- shiny::renderPlot({
        
        well_plate_plot()
        
      })
      
      # -----
      
      # Text outputs ------------------------------------------------------------
      
      output$ld_all_missing_files <- shiny::renderText({
        
        shiny::validate(
          shiny::need(
            expr = !base::is.null(current_well_plate()[["missing_files"]]), 
            message = "No folder has been chosen for this well plate."
          )
        )
        
        shiny::validate(
          shiny::need(
            expr = base::length(current_well_plate()[["missing_files"]]) == 0, 
            message = "No missing files."
          )
        )
        
        stringr::str_c(current_well_plate()[["missing_files"]], collapse = ", ")
        
      })
      
      output$ld_chosen_dir <- shiny::renderText({
        
        shiny::req(input$ld_added_well_plates)
        shiny::req(current_well_plate()[["directory"]])
        
        current_well_plate()[["directory"]]
        
      })
      
      output$ld_error_message <- shiny::renderText({
        
        shiny::req(well_plates_with_errors())
        
        well_image_error_message()
        
      })
      
      # -----
      
      
      # Table outputs -----------------------------------------------------------
      
      
      output$ld_ambiguous_directories <- DT::renderDataTable({
          
          shiny::validate(
            shiny::need(
              expr = current_well_plate()[["ambiguous_directories"]], 
              message = "No folder has been chosen for this well plate."
            )
          )
          
          shiny::validate(
            shiny::need(
              expr = !base::identical(current_well_plate()[["ambiguous_directories"]], base::data.frame()), 
              message = "No ambiguous directories detected."
            )
          )
          
          # print output
          current_well_plate()[["ambiguous_directories"]]
          
        })
      
      output$ld_loading_status <- DT::renderDataTable({
        
        loading_status()
        
      })
      
      # Module return value -----------------------------------------------------
      
      return_value <- shiny::reactive({ 
        
        rv <- 
        list(all_wp_lists = ld_output$all_wp_lists,
             track_df = ld_output$track_df, 
             proceed = ld_output$proceed)
        
        assign(x = "rv_load_data", value = rv, .GlobalEnv)
        
        return(rv)
        
      })
      
      base::return(return_value)
      
      
    })
  
}