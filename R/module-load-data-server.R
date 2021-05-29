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

moduleLoadDataServer <- function(id, object){
  
  shiny::moduleServer(
    id = id, 
    module = function(input, output, session){
      
      
      # Reactive values ---------------------------------------------------------
      
      well_plate_list <- shiny::reactiveVal(value = object@well_plates)
      well_plate_names <- shiny::reactiveVal(value = base::names(object@well_plates))
      
      read_in_data <- shiny::reactiveVal(value = list())
      
      # list containing all the information regarding the experiment's design
      # - the return value of this module
      ld_output <- shiny::reactiveValues(
        
        track_list = list(),
        well_plate_list = list(),
        proceed = numeric()
        
      )
      
      # -----
      
      # Render UIs --------------------------------------------------------------
      
      output$ld_added_well_plates <- shiny::renderUI({
        
        ns <- session$ns
        
        shiny::tagList(
          #shiny::h5(shiny::strong("Added Well Plates:")),
          shiny::selectInput(inputId = ns("ld_added_well_plates"), 
                             label = NULL, 
                             choices = well_plate_names())
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
      
      
      output$ld_loading_box <- shiny::renderUI({
        
        ns <- session$ns 
        
        if(base::all(loading_status()[["Ready to load"]] == "Yes")){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        shinydashboard::box(title = "Well Plate Status", status = status, width = 12, 
                            solidHeader = TRUE,
                            shiny::fluidRow(
                              shiny::column(width = 12, 
                                            DT::dataTableOutput(outputId = ns("ld_loading_status"))
                              )
                            ),
                            shiny::HTML("<br><br>"), 
                            shiny::fluidRow(width = 12, 
                                            hs(12, align = "center",
                                               shiny::actionButton(inputId = ns("ld_load_data"), label = "Load Data")
                                            )
                            )
        ) %>% add_helper(content = helper_content$well_plate_status)
        
      })
      
      
      output$ld_save_and_proceed_box <- shiny::renderUI({
        
        ns <- session$ns
        
        if(base::is.list(read_in_data()) & base::length(read_in_data()) != 0){
          
          status <- "success"
          
        } else {
          
          status <- "warning"
          
        }
        
        
        shinydashboard::box(title = "Load Files & Proceed", status = status, width = 12, 
                            solidHeader = TRUE, collapsible = FALSE, 
                            shiny::uiOutput(outputId = ns("ld_well_plate_errors")),
                            shiny::uiOutput(outputId = ns("ld_well_image_errors")),
                            shiny::textOutput(outputId = ns("ld_error_message")),
                            shiny::HTML("<br>"),
                            shiny::column(width = 12, align = "center",
                                          shiny::splitLayout(
                                            cellWidths = c("50%", "50%"),
                                            shiny::actionButton(inputId = ns("ld_save_and_proceed"), label = "Save & Proceed")
                                          )
                            )
        )  %>% add_helper(content = helper_content$load_files_and_proceed)
        
      })
      
      
      # -----
      
      # Observe events ----------------------------------------------------------

      
      # add new directory to well plate
      oe <- shiny::observeEvent(dir_string(), {
        
        # actual code !!!
        checkpoint(evaluate = !base::is.null(well_plate_list()),
                   case_false = "no_set_up_saved")
        
        well_plate_list_new <- well_plate_list()
        wp_name <- input$ld_added_well_plates
        
        well_plate_list_new[[wp_name]][["directory"]] <- dir_string()
        
        well_plate_list_new[[wp_name]] <-
          evaluate_file_availability_shiny(
            wp_list = well_plate_list_new[[wp_name]],
            recursive = input$ld_recursive, 
            keep = input$ld_keep_filetype
          )
        
        assign(x = "xlist", value = well_plate_list_new, .GlobalEnv)
        
        check <- check_wp_directories(well_plate_list = well_plate_list_new)
        
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
        
        # update well_plate_list and -names
        well_plate_list(well_plate_list_new)
        well_plate_names(base::names(well_plate_list()))
        
      })
      
      # load data
      oe <- shiny::observeEvent(input$ld_load_data,{
        
        checkpoint(evaluate = base::all(loading_status()[["Ready to load"]] == "Yes"),
                   case_false = "well_plates_not_ready")
        
        data_list <- 
          purrr::map2(.x = well_plate_list(),
                      .y = well_plate_names(),
                      .f = load_data_files_shiny, 
                      object = object, 
                      session = session)
        
        shiny::showNotification(ui = "Reading done.", type = "message")
        
        assign("data_list", data_list, .GlobalEnv)
        
        # update read_in_data
        read_in_data(data_list)
        
      })
      
      # save and proceed 
      oe <- shiny::observeEvent(input$ld_save_and_proceed, {
        
        checkpoint(evaluate = base::is.list(read_in_data()) & base::length(read_in_data()) != 0,
                   case_false = "no_data_read_in")
        
        ld_output$well_plate_list <- well_plate_list()
        ld_output$track_list <- track_list()
        
        object@well_plates <- well_plate_list()
        
        object@cdata$stats <- stat_list()
        object@cdata$tracks <- track_list()
        
        ld_output$proceed <- input$ld_save_and_proceed
        ld_output$object <- object
        
        shiny_fdb(in_shiny = TRUE, ui = "Results have been saved. Click on 'Return Cypro Object' and proceed with checkDataQuality().")
        
        
      })
      
      # ----- 
      
      # Reactive expressions ----------------------------------------------------
      
      # directory handling ---
      
      # shinyFiles::shinyDirButton() - server
      
      system_info <- base::Sys.info()
      sysname <- system_info["sysname"]
      
      if(sysname == "Windows"){
        
        dir_roots <- shinyFiles::getVolumes()
        
      } else {
        
        dir_roots <- c("wd" = "~")
        
      }
      
      if(sysname == "Windows"){
        
        shinyFiles::shinyDirChoose(input = input, 
                                   id = "ld_well_plate_dir", 
                                   session = session, 
                                   roots = dir_roots()
        )
        
      } else {
        
        shinyFiles::shinyDirChoose(input = input, 
                                   id = "ld_well_plate_dir", 
                                   session = session, 
                                   roots = dir_roots, 
                                   restrictions = base::system.file(package = "base")
                                   )
        
      }
      

      
      # assembled directory 
      dir_string <- shiny::reactive({ 
        
        shiny::validate(
          shiny::need(expr = base::is.list(x = input$ld_well_plate_dir), 
                      message = "No folder chosen.")
        )
        
        if(sysname == "Windows"){
          
          hlpr_assemble_directory(input_list = input$ld_well_plate_dir)
          
        } else {
          
          shinyFiles::parseDirPath(roots = c(wd = "~"), input$ld_well_plate_dir)
          
        }
        
      })
      
      # ---
      
      # current well plate
      current_well_plate <- shiny::reactive({
        
        shiny::req(input$ld_added_well_plates)
        
        well_plate_list()[[input$ld_added_well_plates]]
        
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
            expr = well_plate_names(), 
            message = "No well plates have been added yet."
          )
        )
        
        loading_status_table_shiny(well_plate_list = well_plate_list())
        
      })
      
      # well plate plot visualizes the file availability
      well_plate_plot <- shiny::reactive({
        
        plot_well_plate_shiny(wp_df = evaluated_wp_df(), 
                              selected_wells_df = NULL, 
                              aes_fill = "availability_status", 
                              aes_color = "availability_status", 
                              fill_guide = TRUE,
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
      
      # assemble results lists depending on data input type
      
      stat_list <- shiny::reactive({
        
        if(!isTimeLapseExp(object)){
          
          stat_list <- assemble_stat_list_shiny(stat_data_list = read_in_data(), 
                                                well_plate_list = well_plate_list(), 
                                                object = object)
          
        } else {
          
          stat_list <- list()
          
        }
        
        base::return(stat_list)
        
        
      })
      
      track_list <- shiny::reactive({
        
        if(isTimeLapseExp(object)){
          
          track_list <- assemble_track_list_shiny(track_data_list = read_in_data(),
                                                  well_plate_list = well_plate_list(), 
                                                  object = object)
          
        } else {
          
          track_list <- list()
          
        }
        
        base::return(track_list)
        
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
        
      }, options = list(scrollX = TRUE))
      
      # Module return value -----------------------------------------------------
      
      # currently not in use !!! ------- start
      return_value <- shiny::reactive({ 
        
        rv <- 
        list(well_plate_list = ld_output$well_plate_list,
             track_list = ld_output$track_list, 
             proceed = ld_output$proceed, 
             object = ld_output$object)
        
        assign(x = "rv_load_data", value = rv, .GlobalEnv)
        
        return(rv)
        
      })
      
      # currently not in use !!! ------- end
      
      
      base::return(ld_output)
      
      
    })
  
}