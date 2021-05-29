
#' @title Server logic: Experiment Design
#' 
#' @description Server logic to be used as input for \code{module}-argument
#' of function \code{shiny::moduleServer()}.
#'
#' @param id Namespace ID
#'
#' @export

moduleExperimentDesignServer <- function(id, usage = "in_function"){

  shiny::moduleServer(
    id = id,
    module = function(input, output, session){
  
      options(shiny.maxRequestSize = 50*1024^2)
      
# Reactive values ---------------------------------------------------------
  
  # a list again containing a list for each set up well plate
      
  measurements_string <- shiny::reactiveVal(value = base::character())
  
  example_list <- shiny::reactiveVal(value = list(loaded = FALSE))
  
  well_plate_df <- shiny::reactiveVal(value = data.frame())
  well_plate_name <- shiny::reactiveVal(value = base::character(1))
  well_plate_list <- shiny::reactiveVal(value = list())
  
  
  # list containing all the information regarding the experiment's design
  # - the return value of this module
  ed_list <- shiny::reactiveValues(
    
    default_directory = base::character(),
    experiment_name = base::character(),
    module_progress = list(overall_information = FALSE, 
                           imaging_set_up = FALSE, 
                           experiment_phases = FALSE),
    proceed = base::logical(),
    set_up = list()
    
  )
  
  # -----  
  
# Render UIs --------------------------------------------------------------
  
  
  # currently not in use !!! ------- start
  
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
        expr = !base::identical(well_plate_list(), list()), 
        message = "No well plates have been added yet."
      )
    )
    
    choices <- 
      purrr::map(.x = well_plate_list(),
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
  
  # currently not in use !!! ------- end

  ###--- overall information
  
  output$ed_example <- shiny::renderUI({
    
    ns <- session$ns 
    
    shiny::tagList(
      
      shiny::fluidRow(
        shiny::column(width = 6, 
                      shiny::h5(shiny::strong("Example Data:")) %>% add_helper(content = helper_content$example), 
                      shiny::actionButton(inputId = ns("ed_example_choose"), label = "Choose")              
                      ) 
        
      ) 
      
    )
    
  })
    
  output$example_table <- DT::renderDataTable({
    
    shiny::validate(
      shiny::need(
        expr = example_list()$df, 
        message = "No example data has been loaded yet."
      )
    )
    
    utils::head(example_list()$df)
    
  }, options = list(scrollX = TRUE))
  
  output$example_x_col <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(example_list()$df)
    
    shinyWidgets::pickerInput(
      inputId = ns("example_x_col"), 
      label = "x-Coordinates:", 
      options = list(`live-search` = TRUE), 
      choices = c("none", base::colnames(example_list()$df)), 
      selected = example_selected(lst = example_list(), col = "x", nth = 1)
    )
    
  })
  
  output$example_y_col <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(example_list()$df)
    
    shinyWidgets::pickerInput(
      inputId = ns("example_y_col"), 
      label = "y-Coordinates:",
      options = list(`live-search` = TRUE), 
      choices = c("none", base::colnames(example_list()$df)), 
      selected = example_selected(lst = example_list(), col = "y", nth = 2)
    )
    
  })
  
  output$example_id_col <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(example_list()$df)
    
    shinyWidgets::pickerInput(
      inputId = ns("example_id_col"), 
      label = "Cell-ID:", 
      options = list(`live-search` = TRUE), 
      choices = base::colnames(example_list()$df),
      selected = example_selected(lst = example_list(), col = "id", nth = 3)
    )
    
  })
  
  output$example_frame_col <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(example_list()$df)

    if(input$ed_exp_type == "time_lapse"){
      
      shinyWidgets::pickerInput(
        inputId = ns("example_frame_col"), 
        label = "Frame:", 
        choices = base::colnames(example_list()$df),
        selected = example_selected(lst = example_list(), col = "frame", nth = 4)
      )
      
    }

    
  })
  
  output$example_keep_col <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(example_list()$df)
    
    example_cols <- base::colnames(example_list()$df)
    
    choices <- example_cols[!example_cols %in% c(input$example_x_col, input$example_y_col, input$example_id_col, input$example_frame_col)]
    
    shinyWidgets::pickerInput(
      inputId = ns("example_keep_col"), 
      choices = choices, 
      options = list(`live-search` = TRUE, `actions-box` = TRUE),
      multiple = TRUE,
      selected = example_selected(lst = example_list(), col = "additional", nth = NULL, choices = choices)
    )
    
  })
  
  output$example_save <- shiny::renderUI({
    
    ns <- session$ns 
    
    if(base::isTRUE(example_input_valid())){
      
      color <- "success"
      
    } else {
      
      color <- "warning"
      
    }
    
    act_button(inputId = ns("example_save"), label = "Save & Proceed", color = color, style = "simple")
    
    
  })
  
  output$example_buttons <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::tagList(
      hs(width = 4, act_button(inputId = ns("example_reset"), label = "Reset", style = "simple")),
      hs(width = 4, act_button(inputId = ns("example_cancel"), label = "Cancel", style = "simple"))
    )
    
    
  })
  
  
  ###--- imaging set up
  
  output$ed_imaging_set_up <- shiny::renderUI({
    
    ns <- session$ns
    
    # make sure that overall information has been provided
    shiny::validate(
      shiny::need(
        expr = base::isTRUE(ed_list$module_progress$overall_information), 
        message = "Overall information has not been saved yet."
      )
    )
    
    # make sure that it's a time lapse experiment
    shiny::validate(
      shiny::need(
        expr = ed_list$set_up$experiment_type == "time_lapse", 
        message = "Imaging set up not required for non time-lapse experiments. (Proceed with well plate set up.)"
      )
    )
    
    output_list <- 
      shiny::tagList(
        shiny::fluidRow(
          shiny::column(width = 6, 
                        shiny::numericInput(inputId = ns("ed_meas_num"), 
                                            label = "Total Number of Images:",
                                            min = 0, step = 1, value = 0)
          )
        ),
        shiny::fluidRow(
          hs(6,
             shiny::h5(shiny::strong("Interval:")), 
             shiny::numericInput(inputId = ns("ed_meas_interval"), 
                                 label = NULL, 
                                 min = 1, step = 0.5, value = 1)), 
          hs(6,
             shiny::h5(shiny::strong("Interval Unit:")),
             shiny::selectInput(inputId = ns("ed_interval_unit"), 
                                label = NULL, 
                                choices = interval_options, 
                                selected = "hours"))
        ),
        shiny::actionButton(inputId = ns("ed_imaging_set_up_save"), 
                            label = "Save & Proceed")
        
      )
    
  })
  
  
  ###--- experiment phases set up
  output$ed_phases_number <- shiny::renderUI({
    
    ns <- session$ns
    
    # make sure that imaging set up has been provided
    shiny::validate(
      shiny::need(
        expr = base::isTRUE(ed_list$module_progress$imaging_set_up), 
        message = "Imaging set up has not been saved yet."
      )
    )
    
    # make sure that it's a time lapse experiment
    shiny::validate(
      shiny::need(
        expr = ed_list$set_up$experiment_type == "time_lapse", 
        message = "Phase set up not required for non time-lapse experiments. (Proceed with well plate set up.)"
      )
    )
    
    shiny::numericInput(inputId = ns("ed_phases_number"), 
                        label = "Number of Phases:",
                        value = 1, min = 1, max = 10, step = 1
    )
    
  })
  
  output$ed_phases_start <- shiny::renderUI({
    
    ns <- session$ns
    
    # not going to appear if non time lapse exp as ed_phasees_number is not going to appear 
    #shiny::req(base::isTRUE(ed_list$module_progress$imaging_set_up) & shiny::isTruthy(input$ed_phases_number))
    
    shiny::req(ed_list$set_up$experiment_type == "time_lapse" & shiny::isTruthy(input$ed_phases_number))
    
    all_phases <- 1:input$ed_phases_number
    
    ordinal_phases <- 
      english::ordinal(all_phases) %>%
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Phase starts from:", sep = " ")
    
    measurements <- measurements_string()
    
    n_meas <- base::length(measurements)
    
    output_list <- 
      purrr::map2(
        .x = all_phases,
        .y = ordinal_phases, 
        .f = function(x, y){
          
          if(x == 1){
            
            choices <- measurements[1]
            
          } else {
            
            choices <- measurements[x:n_meas]
            
          }
          
          shiny::selectInput(
            inputId = ns(stringr::str_c("ed_phases_start", x, sep = "_")),
            label = y,
            choices = choices
            )
          
        }
      )
    
    base::return(output_list[2:n_meas])
    
  })
  
  output$ed_phases_save <- shiny::renderUI({
    
    ns <- session$ns
    
    # not going to appear if non time lapse exp as ed_phases_start is not going to appear
    #shiny::req(ed_list$module_progress$imaging_set_up & shiny::isTruthy(input$ed_phases_number))
    
    shiny::req(ed_list$set_up$experiment_type == "time_lapse" & shiny::isTruthy(input$ed_phases_number))
    
    
    shiny::actionButton(inputId = ns("ed_phases_save"), label = "Save & Proceed")
    
  })
  
  
  ###--- well plate set up 
  output$ed_added_well_plates <- shiny::renderUI({
    
    ns <- session$ns
    
    shinyWidgets::pickerInput(inputId = ns("ed_added_well_plates"), 
                              label = NULL, 
                              choices = base::names(well_plate_list()), 
                              multiple = TRUE)
    
  })
  
  
  output$ed_conditions <- shiny::renderUI({
    
    ns <- session$ns
    
    shiny::req(ed_list$set_up$phases)
    
    if(base::length(all_phases()) == 1){
      
      output_list <- 
        shiny::tagList(
          shiny::h5(shiny::strong("Condition:")),
          shiny::textInput(
            inputId = ns("ed_conditions"), 
            label = NULL, 
            value = "", 
            placeholder = "condition"
          )
        )
      
    } else {
      
      output_list <- 
        purrr::map(.x = all_phases(), .f = function(p){
                     
          
          label <- stringr::str_c("Condition",
                                  confuns::make_capital_letters(english::ordinal(p), collapse.with = NULL),
                                  "Phase:",
                                  sep = " ")
          
          shiny::tagList(
            shiny::h5(shiny::strong(label)), 
            shiny::textInput(
              inputId = ns(stringr::str_c("ed_condition", p, sep = "_")), 
              label = NULL, 
              value = "", 
              placeholder = "condition"
            )
            
          )
          
                     
                     
                   })
      
    }
    
    base::return(output_list)
    
  })
  
  
  output$ed_save_and_proceed_box <- shiny::renderUI({
    
    ns <- session$ns
    
    if(base::length(well_plate_list()) >= 1){
      
      color <- "success"
      
    } else {
      
      color <- "warning"
      
    }
    
    shinydashboard::box(width = 6, title = "Save Experiment Design & Proceed",
                        solidHeader = TRUE, status = color,
                        shiny::fluidRow(width = 12,
                                        shiny::column(width = 12,
                                                      DT::dataTableOutput(outputId = ns("ed_well_plate_folders")))
                        ), 
                        shiny::HTML("<br><br>"),
                        # proceed with data loading
                        shiny::fluidRow(width = 12,
                                        shiny::column(width = 12, align = "center",
                                                      shiny::actionButton(inputId = ns("ed_save_and_proceed"), 
                                                                          label = "Save & Proceed" 
                                                      )
                                        )
                        )
    )
    
  })
  
  # -----
  
# Observe events ----------------------------------------------------------
  
  ### --- overall information 
  
  # example data
  oe <- shiny::observeEvent(input$ed_example_choose, {
    
    ns <- session$ns
    
    # shiny helper 
    shiny::showModal(
      ui = shiny::modalDialog(
        shiny::tagList(
          shiny::fluidRow(
            
            shiny::column(width = 12, 
                          
                          shiny::h4(shiny::strong("Step 1: Load Template File")), 
                          shiny::helpText("Choose a .csv- file that serves as a template for all files you are going to read in. (Only the first 6 rows will be displayed.)"),
                          shiny::fileInput(inputId = ns("example_load_table"), label = "Choose", accept = c(".xls", ".xlsx", ".csv")), 
                          DT::dataTableOutput(outputId = ns("example_table")),
                          shiny::HTML("<br><br>"),
                          shiny::h4(shiny::strong("Step 2: Denote Identifier Variables")),
                          shiny::helpText("The variables necessary to identify your observations unambiguously switch between only 'Cell ID' in case of one time imaging or already summarized data and 'Cell ID' & 'Frame' in case of time lapse experiments."),
                          shiny::HTML("<br>"),
                          shiny::fluidRow(
                            hs(3, shiny::uiOutput(outputId = ns("example_id_col"))),
                            hs(3, shiny::uiOutput(outputId = ns("example_frame_col")))
                          ), 
                          shiny::HTML("<br><br>"),
                          shiny::h4(shiny::strong("Step 3: Denote Variables of Interest")), 
                          shiny::helpText("Choose the variables that contain the information you want to analyze later on. If you want to use certain build-in modules you can provide the corresponding 
                                          variable names here."),
                          shiny::fluidRow(
                            shiny::column(width = 12, 
                                          shiny::h5(shiny::strong("3.1 Module: Cell Localisation & Migration")),
                                          shiny::fluidRow(
                                            hs(3, shiny::uiOutput(outputId = ns("example_x_col"))),
                                            hs(3, shiny::uiOutput(outputId = ns("example_y_col")))
                                          )
                            )
                          ),
                          shiny::fluidRow(
                            shiny::column(width = 12,
                                          shiny::h5(shiny::strong("3.2 Module: Miscellaneous")),
                                          shiny::uiOutput(outputId = ns("example_keep_col"))
                            )
                          )
                          
            )
            
          )
          
          
        ), 
        
        footer = shiny::tagList(
          shiny::fluidRow(
            hs(width = 4, shiny::uiOutput(outputId = ns("example_save"))), 
            shiny::uiOutput(outputId = ns("example_buttons"))
          )
        ), 
        
        size = "l", 
        
        easyClose = FALSE
      )
    )
    
  })
  
  oe <- shiny::observeEvent(input$example_load_table, {
    
    input_file <- input$example_load_table
    directory <- input_file$datapath
    
    if(stringr::str_detect(string = directory, pattern = ".csv$")){
      
      
      df <- 
        base::suppressMessages({
          
          base::suppressWarnings({
            
            readr::read_csv(file = directory)
            
          })
          
        })
      
    }
    
    if(stringr::str_detect(string = directory, pattern = ".xlsx$")){
      
      df <- readxl::read_xlsx(path = directory, sheet = 1)
      
    }
    
    if(stringr::str_detect(string = directory, pattern = ".xls")){
      
      df <- readxl::read_xls(path = directory, sheet = 1)
      
    }
    
    
    if(tibble::has_rownames(df)){
      
      df <- tibble::rownames_to_column(var = "Rownames")
      
    }

    
    df <- dplyr::mutate_if(df, .predicate = is.numeric, .funs = base::round, digits = 2)    
    
    checkpoint(
      evaluate = base::ncol(df) >= 4, 
      case_false = "invalid_example_df"
    )
    
    example_list <- example_list()
    
    example_list$df <- df
    example_list$loaded <- TRUE
    
    example_list(example_list)
    
  })
  
  oe <- shiny::observeEvent(input$example_reset, {
    
    example_list(list(loaded = FALSE))
    
  })
  
  oe <- shiny::observeEvent(input$example_cancel, {
    
    # reset example list
    shiny::removeModal()
    
  })
  
  oe <- shiny::observeEvent(input$example_save, {
    
    denoted_columns <- 
      list(
        x_coords = input$example_x_col, 
        y_coords = input$example_y_col, 
        frame = input$example_frame_col, 
        cell_id = input$example_id_col, 
        additional = c(input$example_keep_col)
      )
    
    denoted_columns <- 
      purrr::map(.x = denoted_columns, .f = function(x){
        
        if(base::all(x == "none")){
          
          x <- NULL
          
        }
        
        base::return(x)
        
      }) %>% 
      purrr::discard(.p = base::is.null)
    
    # update example list
    
    example_list <- example_list()
    
    
    if(base::any(base::is.null(denoted_columns$x_coords), base::is.null(denoted_columns$y_coords))){
      
      checkpoint(
        evaluate = !base::is.null(denoted_columns$additional), 
        case_false = "invalid_column_input"
      )
      
    } 
    
    example_list$denoted_columns <- denoted_columns
    
    example_list$loaded <- TRUE
    
    # check if column denotation is valid 
    valid_example_cols(example_list)
    
    example_list(example_list)
    
    assign(x = "example_list", value = example_list, envir = .GlobalEnv)
    
    shiny::removeModal()
    
  })
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_overall_info_save, {
    
    checkpoint(
      evaluate = shiny::isTruthy(ed_list$default_directory), 
      case_false = "no_storage_directory"
    )
    
    checkpoint(
      evaluate = input$ed_software %in% c("cell_tracker", "cell_profiler"), 
      case_false = "no_software_input"
    )
    
    checkpoint(
      evaluate = input$ed_software == "cell_tracker" | base::isTRUE(example_list()$loaded), 
      case_false = "incomplete_software_input"
    )
    
    ed_list$experiment_name <- input$ed_experiment_name
    
    ed_list$storage_info$default_directory <- full_experiment_dir()
    
    ed_list$set_up$experiment_type <- input$ed_exp_type
    ed_list$set_up$example <- example_list()
    ed_list$set_up$example$software <- input$ed_software
    
    ed_list$module_progress$overall_information <- TRUE
    
    if(ed_list$set_up$experiment_type == "time_lapse"){
    
      # set to FALSE to make sure that changing experiment type does not make 
      # the respective input options appear
      ed_list$module_progress$imaging_set_up <- FALSE
      ed_list$module_progress$experiment_phases <- FALSE
      
      shiny_fdb(in_shiny = TRUE, ui = "Overall information successfully stored. Proceed with imaging set up.")  
      
    } else {
      
      shiny_fdb(in_shiny = TRUE, ui = "Overall information successfully stored. Proceed with well plate set up.")  
      
      # no imaging set up needed
      ed_list$set_up$nom <- NULL
      ed_list$set_up$n_meas <- NULL
      ed_list$set_up$itvl <- NULL
      ed_list$set_up$itvl_u <- NULL
      ed_list$set_up$image_string <- NULL
      ed_list$set_up$measurement_string <- NULL
      ed_list$set_up$time_string <- NULL
      
      ed_list$module_progress$imaging_set_up <- TRUE
      
      # no phases set up needed
      ed_list$set_up$phases <- list(first = "One Image")
      ed_list$module_progress$experiment_phases <- TRUE
      
    }
    
  })
  
  
  ###--- imaging set up 
  
  # save and proceed 
  oe <- shiny::observeEvent(input$ed_imaging_set_up_save, {
    
    check_set_up_progress(progress_input = ed_list$module_progress,
                          subset = "overall_information", 
                          proceed_with = "the imaging set up.")
    
    checkpoint(
      evaluate = input$ed_meas_num > 0, 
      case_false = "invalid_meas_number"
    )
    
    measurements_string(stringr::str_c(time_string(), image_string(), sep = " / "))
    
    ed_list$set_up$nom <- input$ed_meas_num
    ed_list$set_up$n_meas <- input$ed_meas_num
    ed_list$set_up$itvl <- input$ed_meas_interval
    ed_list$set_up$itvl_u <- input$ed_interval_unit
    ed_list$set_up$image_string <- image_string()
    ed_list$set_up$measurement_string <- measurements_string()
    ed_list$set_up$time_string <- time_string()
    
    ed_list$module_progress$imaging_set_up <- TRUE
    
    shiny_fdb(in_shiny = TRUE, ui = "Imaging set up successfully saved. Proceed with experiment phases.")
    
  })
  
  
  ###--- experiment phases
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_phases_save, {
    
    all_phases <- 1:input$ed_phases_number
        
    phase_list <- purrr::map(
      .x = all_phases,
      .f = function(p){
        
        if(p == 1){
          
          res <- measurements_string()[1]
          
        } else {
          
          res <- 
            input[[stringr::str_c("ed_phases_start", p, sep = "_")]]
          
        }
        
        base::return(res)
      
      
    }) %>% 
      purrr::set_names(nm = english::ordinal(all_phases))

    
    well_plate_df(data.frame())
    well_plate_list(list())
    
    ed_list$set_up$phases <- phase_list
    ed_list$module_progress$experiment_phases <- TRUE
    
    shiny_fdb(in_shiny = TRUE, ui = "Experiment phases successfully saved. Proceed with well plate set up.")
    
  })
  
  ###--- well plate set up 
  
  # new well plate
  oe <- shiny::observeEvent(input$ed_new_well_plate, {
    
    check_set_up_progress(progress_input = ed_list$module_progress)
    
    # check input frame number per well
    checkpoint(evaluate = input$ed_images_per_well != 0, 
               case_false = "invalid_image_number")
    
    # check well plate name
    wp_list_new <- well_plate_list()
    wp_name <- input$ed_well_plate_name 
    new_name <- !wp_name %in% base::names(wp_list_new)
    valid_name <- !wp_name == ""
    
    checkpoint(evaluate = base::all(new_name, valid_name), 
               case_false = "invalid_wp_name")
    
    # update current well plate name
    well_plate_name(wp_name)
    
    
    # data.frame (obs => well)
    well_plate_df_new <- 
      setUpWellPlateDf(type = input$ed_well_plate, phases = all_phases())
    
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
    
    if(base::length(all_phases()) > 1){
     
      new_condition <- 
        purrr::map(.x = all_phases(), 
                   .f = function(p){
                     
                     input[[stringr::str_c("ed_condition", p, sep = "_")]]
                     
                   }) %>% 
        purrr::flatten_chr()
      
    } else {
      
      new_condition <- input$ed_conditions
      
    }
    
    if(!base::all(new_condition == "")){
      
      checkpoint(
        evaluate = !base:::any(new_condition == ""), 
        case_false = "missing_condition_input", 
        duration = 10
      )
      
    }
    
    checkpoint(
      evaluate = !base::all(new_condition == "") | !base::all(new_cell_line == ""), 
      case_false = "missing_both_inputs"
    )
    
    checkpoint(
      evaluate = base::length(base::intersect(x = new_cell_line, y = new_condition)) == 0, 
      case_false = "cell_line_condition_overlap"
    )
    
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
    
    if(base::all(new_condition != "")){
      
      well_plate_df_new$condition <-
        update_condition(
          var_condition = well_plate_df_new$condition, 
          var_selected = well_plate_df_new$selected, 
          input_condition = new_condition
          )
      
      
      well_plate_df_new$condition_df <- 
        update_condition_df(
          var_condition_df = well_plate_df_new$condition_df, 
          var_selected = well_plate_df_new$selected, 
          input_condition = new_condition
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
    
    # give feedback 
    shiny_fdb(in_shiny = TRUE, ui = "Well info successfully added.")
    
  })
  
  # delete well information 
  oe <- shiny::observeEvent(input$ed_delete_well_info, {
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    checkpoint(evaluate = base::nrow(selected_wells()) != 0, 
               case_false = "no_wells_chosen")
    
    # -
    
    condition_df_exmpl <- all_wells()$condition_df[[1]]
    phase_names <- base::colnames(condition_df_exmpl)
    
    # empty condition df
    ecdf <-
      purrr::map_df(.x = condition_df, .f = function(phase){return(NA)}) %>% 
      purrr::set_names(phase_names) %>% 
      base::as.data.frame()
    
    well_plate_df_new <- 
      dplyr::mutate(.data = all_wells(), 
                    selected = well %in% selected_wells()$well, 
                    condition = dplyr::case_when(selected ~ "unknown", TRUE ~ condition),
                    cell_line = dplyr::case_when(selected ~ "unknown", TRUE ~ cell_line),
                    cl_condition = stringr::str_c(cell_line, condition, sep = " & "),
                    condition_df = purrr::map2(.x = condition_df, .y = selected, ecdf = ecdf, .f = function(cdf, selected, ecdf){
                      
                      if(base::isTRUE(selected)){
                        
                        return(ecdf)
                        
                      } else{
                        
                        return(cdf)
                        
                      }
                      
                    }),
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
    
    shiny_fdb(in_shiny = TRUE, ui = "Well information successfully removed.")
    
    
  })
  
  # add well plate
  oe <- shiny::observeEvent(input$ed_add_well_plate, {
    
    wp_list_new <- well_plate_list()
    
    # check all_wells()
    checkpoint(evaluate = !base::identical(all_wells(), base::data.frame()), 
               case_false = "no_well_plate_chosen")
    
    
    checkpoint(evaluate = base::sum(all_wells()$information_status == "Complete") >= 1, 
               case_false = "empty_well_plate")
    
    wp_df <- dplyr::mutate(.data = all_wells(), ipw = input$ed_images_per_well)
    
    # check information status
    if(base::isFALSE(input$ed_dismiss_unknown)){
      
      check <- check_wp_df_shiny(wp_df = wp_df)
      
      checkpoint(evaluate = check$evaluate, 
                 case_false = "case_false", 
                 error_notifications = check, 
                 duration = 15) 
      
    } 
    
    # add new well plate
    wp_list_new[[well_plate_name()]][["wp_df"]] <- wp_df
    
    # update reactive values
    well_plate_list(wp_list_new)
    well_plate_df(data.frame())
    
  })
  
  # save and proceed
  oe <- shiny::observeEvent(input$ed_save_and_proceed, {
    
    checkpoint(evaluate = base::length(well_plate_list()) != 0, 
               case_false = "no_well_plates_added")
    
    ed_list$proceed <- TRUE
    
    ed_list$object <- 
      methods::new(Class = "cto", 
                   information = list(directory_cto = ed_list$default_directory),
                   name = ed_list$experiment_name,
                   set_up = ed_list$set_up, 
                   well_plates = well_plate_list()
      )
    
    
    shiny_fdb(in_shiny = TRUE, ui = "Experiment design successfully saved. Click on 'Return Cypro Object' and proceed with loadData().")
    
  })
  
  # -----
  
# Reactive expressions ----------------------------------------------------

  ###--- overall information 
  
  system_info <- base::Sys.info()
  sysname <- system_info["sysname"]
  
  if(sysname == "Windows"){
    
    dir_roots <- shinyFiles::getVolumes()
    
  } else {
    
    dir_roots <- c("wd" = "~")
    
  }
  
  if(sysname == "Windows"){
    
    shinyFiles::shinyDirChoose(input = input, 
                               id = "ed_experiment_dir", 
                               session = session, 
                               roots = dir_roots()
    )
    
  } else {
    
    shinyFiles::shinyDirChoose(input = input, 
                               id = "ed_experiment_dir", 
                               session = session, 
                               roots = dir_roots, 
                               restrictions = base::system.file(package = "base")
    )
    
  }
  
  full_experiment_dir <- shiny::reactive({
    
    shiny::validate(
      shiny::need(
        expr = shiny::isTruthy(input$ed_experiment_name), 
        message = "Experiment has not been named yet."
      )
    )
    
    shiny::validate(
      shiny::need(expr = base::is.list(x = input$ed_experiment_dir), 
                  message = "No folder chosen.")
    )
    
    if(sysname == "Windows"){
      
      folder_dir <- hlpr_assemble_directory(input_list = input$ed_experiment_dir)
      
    } else {
      
      folder_dir <- shinyFiles::parseDirPath(roots = c(wd = "~"), input$ed_experiment_dir)
      
    }
    
    full_dir <- stringr::str_c(folder_dir, "/", input$ed_experiment_name, ".RDS", sep = "")
    
    ed_list$default_directory <- full_dir
    
    base::return(full_dir)
    
  })
  
  
  example_input_valid <- shiny::reactive({
    
    # valid identifier variables 
    
    # cell id 
    
    return(TRUE)
    
    
  })
  
  
  ###--- imaging set up 
  time_string <- shiny::reactive({
    
    stringr::str_c(1:input$ed_meas_num * input$ed_meas_interval,
                   input$ed_interval_unit,
                   sep = " ")
    
  })
  
  image_string <- shiny::reactive({
    
    english::ordinal(x = 1:input$ed_meas_num) %>% 
      confuns::make_capital_letters(collapse.with = NULL) %>% 
      stringr::str_c(., "Image", sep = " ")
    
  })
  
  ###--- experiment phases 
  
  all_phases <- shiny::reactive({
    
    base::seq_along(ed_list$set_up$phases)
    
  })
  
  
  ###--- well plate set up
  
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
                    dplyr::between(col_num, xmin, xmax), 
                    dplyr::between(row_num, ymin, ymax)
                    )
      
    } 
    
    base::return(selected_wells)
    
  })
  
  hovered_well_info <- shiny::reactive({
    
    xh <- input$well_plate_hover$x
    
    yh <- input$well_plate_hover$y
    
    hovering <-
      base::all(
        base::is.numeric(xh), 
        base::is.numeric(yh) 
      )
    
    shiny::validate(
      shiny::need(
        expr = base::isTRUE(hovering), 
        message = "Hover over a well to have it's information printed here."
      )
    )
    
    xh <- base::round(xh, digits = 0)
    yh <- base::round(yh, digits = 0)
      
    hovered_well <- 
      dplyr::filter(.data = well_plate_df(), row_num == yh & col_num == xh) %>% 
      dplyr::ungroup()
    
    shiny::validate(
      shiny::need(
        expr = base::nrow(hovered_well) == 1, 
        message = "Hover over a well to have it's information printed here."
      )
    )
      
    condition_df <- hovered_well$condition_df[[1]]
    
    if(base::ncol(condition_df) == 1){
      
      base::colnames(condition_df) <- "Condition"
      
    } else {
      
      base::colnames(condition_df) <- 
        stringr::str_replace_all(base::colnames(condition_df), 
                                 pattern = "Phase", 
                                 replacement = "Condition")
      
    }
    
    condition_df <- 
      purrr::map_df(.x = condition_df, .f = ~ tidyr::replace_na(data = .x, replace = "unknown"))
    
    well_cell_line_df <-
      dplyr::select(hovered_well, `Well:` = well, Status = information_status, `Cell Line:` = cell_line)
    
    base::return(base::cbind(well_cell_line_df, condition_df))
    
  })
  
  
  # well plate visualization 

  
  ed_well_plate_plot <- shiny::reactive({
    
    shiny::validate(
      shiny::need(expr = !base::identical(all_wells(), base::data.frame()), 
                  message = "No well plate chosen.")
      )
    
    plot_well_plate_shiny(wp_df = all_wells(), 
                          selected_wells_df = selected_wells(), 
                          aes_fill = "cl_condition", 
                          aes_color = "information_status",
                          color_values = status_colors) +
      ggplot2::labs(color = "Information Status", subtitle = stringr::str_c("Name:", well_plate_name(), sep = " ")) 
    
  })
  
  # -----
  
# Plot outputs ------------------------------------------------------------
  
  output$ed_well_plate_plot <- shiny::renderPlot({
    
    ed_well_plate_plot()
    
  })
  
  # -----
  
# Table outputs -----------------------------------------------------------
  
  output$ed_well_info <- shiny::renderTable({
    
    hovered_well_info()
    
  })
  
  
  output$ed_well_plate_folders <- DT::renderDataTable({
    
    shiny::validate(
      shiny::need(
        expr = base::names(well_plate_list()), 
        message = "No well plates have been added yet."
      )
    )
    
    data.frame(
      "Name" = base::names(well_plate_list()),
      "Type" = purrr::map_chr(.x = well_plate_list(), ~ base::unique(.x[["wp_df"]][["type"]])), 
      "Fields of Interest" = purrr::map_dbl(.x = well_plate_list(), ~ base::unique(.x[["wp_df"]][["ipw"]]))
    )
    
  })
  
# Text outputs ------------------------------------------------------------
  
  output$ed_chosen_dir <- shiny::renderText({
    
    well_plate_dir()
    
  })
  
  output$ed_experiment_path <- shiny::renderText({
    
    full_experiment_dir()
    
  })
  
  
# Module return value -----------------------------------------------------
  
  base::return(ed_list)
  
  })

}





# helper ------------------------------------------------------------------

#' Title
#'
#' @description Helps to generate the rendered UIs with which to denote the 
#' x-, y-, etc. colums if the data has been generated by cell profiler.

example_selected <- function(lst, slot = "denoted_columns", col, nth, choices = NULL){
  
  if(base::is.character(lst[[slot]][[col]])){
    
    base::return(lst$denoted_columns[[col]])
    
  } else if(col != "additional") {
    
    base::return(base::colnames(lst$df)[nth])
    
  } else if(col == "additional"){
    
    additional_cols <- base::is.character(lst[[slot]][[col]])
    
    if(base::is.character(additional_cols) & base::length(additional_cols) >= 1){
      
      base::return(lst[[slot]][[col]])
      
    } else {
    
      base::return(choices)  
      
    }
    
    
    
  }
  
}


#' Title
#'
#' @description Take shiny input and update the condition and the condition_df variables
#' of the well plate data.frame.

update_condition <- function(var_condition, var_selected, input_condition){
  
  purrr::map2(.x = var_condition, 
              .y = var_selected, 
              .f = function(x, .selected){
                
                if(base::isTRUE(.selected)){
                  
                  if(base::length(input_condition) > 1){
                    
                    input_condition <- 
                      stringr::str_c(
                        base::seq_along(input_condition), 
                        input_condition, 
                        sep = "."
                      )
                    
                    res <- stringr::str_c(input_condition, collapse = " -> ")
                    
                  } else {
                    
                    res <- input_condition
                  }
                  
                } else {
                  
                  res <- x
                  
                }
                
                base::return(res)
                
              }) %>% 
    purrr::flatten_chr()
  
}

#' @rdname update_condition 
update_condition_df <- function(var_condition_df, var_selected, input_condition, input_phases){
  
  purrr::map2(.x = var_condition_df,
              .y = var_selected, 
              .f = function(df, .selected){
                
                phases <- base::colnames(df)
                
                if(base::isTRUE(.selected)){
                  
                  for(phase in base::seq_along(phases)){
                    
                    df[1, phase] <- input_condition[phase]
                    
                  }
                  
                } 
                
                base::return(df)
                
              })
  
}


#' Title
#'
#' @description Make sure that information input is sufficient before proceeding with well plates. 
check_set_up_progress <- function(progress_input,
                                  subset = c("overall_information", "imaging_set_up", "experiment_phases"), 
                                  proceed_with = "the well plate set up"){
  
  missing <- 
    purrr::keep(progress_input[subset], .p = base::isFALSE) 
  
  progress_named <- c("overall_information" = "Overall Information", 
                      "imaging_set_up" = "Imaging Set Up", 
                      "experiment_phases" = "Experiment Phases")
  
  if(base::length(missing) >= 1){
    
    missing_names <- progress_named[base::names(missing)]
    
    msg <- glue::glue("Make sure to provide and save required information about '{missing_names}' before proceeding with {proceed_with}.", 
                      missing_names = glue::glue_collapse(missing_names, sep = "', '", last = "' and '"))
    
    shiny_fdb(in_shiny = TRUE, ui = msg, type = "error", duration = 10)
    
    shiny::req(FALSE)
    
  }
  
}


#' Title
#' 
#' @description Make sure that all denoted columns appear only one time. 
#'
valid_example_cols <- function(example_list){
  
  col_count <- 
    example_list$denoted_columns %>%
    purrr::flatten_chr() %>%
    base::table() %>%
    base::as.data.frame() %>% 
    purrr::set_names(nm = c("col", "count")) %>% 
    dplyr::filter(count > 1)
  
  if(base::nrow(col_count) >= 1){
    
    dupl_names <- glue::glue_collapse(col_count$col, sep = "', '", last = "' and '")
    
    ref1 <- confuns::adapt_reference(col_count$col, sg = "variable", pl = "variables")
    ref2 <- confuns::adapt_reference(col_count$col, sg = "is", pl = "are")
    
    msg <- glue::glue("Invalid variable denotation. The {ref1} '{dupl_names}' {ref2} used more than one time.")
    
    shiny_fdb(in_shiny = TRUE, ui = msg, type = "error", duration = 20)
    
    shiny::req(FALSE)
    
  }
  
}



