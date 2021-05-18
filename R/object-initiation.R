#' @title Object initiation: Step 1
#' 
#' @description Opens an interactive application in which the experiment can be set 
#' up. This includes in particular the design of the well plates. 
#'
#' @return An empty celltracer object. 
#' @export
#'
designExperiment <- function(){
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleExperimentDesignUI(id = "ed"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cto")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ed_results <-
          moduleExperimentDesignServer(id = "ed", usage = "in_function")
        
        
        output$return_cto <- shiny::renderUI({
          
          ed_list <- shiny::reactiveValuesToList(ed_results)
          
          if(base::isTRUE(ed_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(inputId = "return_cto",
                                   label = "Return Celltracer Object", 
                                   color = color, 
                                   style = "gradient")
          
        })
        
        oe <- shiny::observeEvent(input$return_cto, {
          
          ed_list <- shiny::reactiveValuesToList(ed_results)
          
          check <- base::tryCatch({
            
            base::class(ed_list$object) == "cto"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cto")
          
          ct_object <- ed_list$object
          
          ct_object@set_up$progress$experiment_design <- TRUE
          
          shiny::stopApp(returnValue = ct_object)
          
        })
        
      }
    )
  )
  
}


#' @title Object initiation: Step 2
#' 
#' @description Opens an interactive application in which the folders containing 
#' the data files are assigned to the well plates and where the data files 
#' are read in. 
#'
#' @inherit argument_dummy params 
#'
#' @return An updated celltracer object.
#' @export
#'
loadData <- function(object){
  
  check_object(object, set_up_req = "experiment_design")
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleLoadDataUI(id = "ld"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cto")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        ld_results <-
          moduleLoadDataServer(id = "ld", object = object)
        
        output$return_cto <- shiny::renderUI({
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          if(shiny::isTruthy(ld_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(inputId = "return_cto",
                                   label = "Return Celltracer Object", 
                                   color = color, 
                                   style = "gradient")
          
        })
        
        
        oe <- shiny::observeEvent(input$return_cto, {
          
          ld_list <- shiny::reactiveValuesToList(ld_results)
          
          check <- base::tryCatch({
            
            base::class(ld_list$object) == "cto"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cto2")
          
          ct_object <- ld_list$object
          
          ct_object@set_up$progress$load_data <- TRUE
          
          if(!isTimeLapseExp(ct_object)){
            
            ct_object@set_up$progress$quality_check <- TRUE
            
          }
          
          shiny::stopApp(returnValue = ct_object)
          
        })
        
      }
    )
  )
  
}


#' @title Object initiation: Step 3
#' 
#' @description Opens an interactive application in which the quality of the data can be assessed and 
#' where the data set can be filtered accordingly. 
#'
#' @inherit argument_dummy params 
#'
#' @return An updated celltracer object.
#' @export
#'
checkDataQuality <- function(object){
  
  check_object(object, set_up_req = "load_data")
  
  if(!isTimeLapseExp(object)){
    
    base::stop("Provided object does not contain time lapse information. Proceed with function 'processData()'.")
    
  }
  
  shiny::runApp(
    shiny::shinyApp(
      ui = function(){
        shinydashboard::dashboardPage(
          
          header = shinydashboard::dashboardHeader(title = app_title), 
          
          sidebar = shinydashboard::dashboardSidebar(
            collapsed = TRUE, 
            shinydashboard::sidebarMenu(
              shinydashboard::menuItem(
                text = "New Session", 
                tabName = "new_session", 
                selected = TRUE
              )
            )
          ), 
          
          body = shinydashboard::dashboardBody(
            
            shinydashboard::tabItems(
              shinydashboard::tabItem(tabName = "new_session",
                                      
                                      moduleQualityCheckUI(id = "qc"), 
                                      shiny::fluidRow(
                                        shiny::column(width = 12, align = "center", 
                                                      
                                                      shiny::uiOutput(outputId = "return_cto")
                                                      
                                        )
                                      )
              )
            )
            
          )
          
        )
      }, 
      server = function(input, output, session){
        
        # shiny helper 
        shinyhelper::observe_helpers()
        
        qc_results <-
          moduleQualityCheckServer(id = "qc", object = object)
        
        output$return_cto <- shiny::renderUI({
          
          qc_list <- shiny::reactiveValuesToList(qc_results)
          
          if(shiny::isTruthy(qc_list$proceed)){
            
            color <- "success"
            
          } else {
            
            color <- "warning"
            
          }
          
          shinyWidgets::actionBttn(inputId = "return_cto",
                                   label = "Return Celltracer Object", 
                                   color = color, 
                                   style = "gradient")
          
        })
        
        oe <- shiny::observeEvent(input$return_cto, {
          
          qc_list <- shiny::reactiveValuesToList(qc_results)
          
          check <- base::tryCatch({
            
            base::class(qc_list$object) == "cto"
            
          }, error = function(error){
            
            FALSE
            
          })
          
          checkpoint(evaluate = check, case_false = "incomplete_cto2")
          
          ct_object <- qc_list$object
          
          ct_object@set_up$progress$quality_check <- TRUE
          
          shiny::stopApp(returnValue = ct_object)
          
        })
        
      }
    )
  )
  
  
  
  
}




#' @title Object initiation: Step 4
#' 
#' @description Processes the data and constructs all needed slots. Afterwards the 
#' celltracer object is set for all subsequent analysis and visualization steps. 
#'
#' @inherit argument_dummy params
#'
#' @return An updated celltracer object.
#' @export
#'
processData <- function(object, verbose = TRUE){
  
  check_object(object, set_up_req = "quality_check")
  
  # post process track list
  confuns::give_feedback(msg = "Processing data.", verbose = verbose)
  
  # create meta data 
  confuns::give_feedback(msg = "Creating cell meta data.", verbose = verbose)
  
  data_slot <- base::ifelse(test = isTimeLapseExp(object), yes = "tracks", no = "stats")
  
  object@data$meta <-
    purrr::map_df(object@data[[data_slot]], .f = ~ .x) %>% 
    dplyr::select(dplyr::all_of(x = meta_variables)) %>% 
    dplyr::distinct()
  
  # create grouping data
  confuns::give_feedback(msg = "Creating cell grouping data.", verbose = verbose)
  
  object@data$grouping <- 
    purrr::map2(.x = object@data[[data_slot]], 
                .y = getPhases(object), 
                .f = function(df, phase){
                  
                  dplyr::mutate(df, phase = {{phase}}) %>% 
                    dplyr::select(cell_id, phase, cell_line, condition) %>% 
                    dplyr::distinct()
                  
                }) %>% 
    purrr::set_names(nm = getPhases(object))
  
  # declare module usability
  if(isTimeLapseExp(object)){
    
    # module: migration (TRUE if coordinates vars are available )
    coords_available <- base::all(c("x_coords", "y_coords") %in% base::names(set_up$example$denoted_columns))
    object@information$modules$migration <- coords_available
    
    
  } else {
    
    # module: migration 
    object@information$modules$migration <- FALSE
    
  }
  
  
  # process time lapse data if available 
  if(isTimeLapseExp(object)){
    
    object@data$tracks <- 
      purrr::map2(.x = object@data$tracks,
                  .y = getPhases(object),
                  set_up = object@set_up,
                  verbose = verbose,
                  .f = function(df, phase, set_up, verbose){
                    
                    itvl <- set_up$itvl
                    itvl_u <- set_up$itvl_u
                    exp_type <- set_up$experiment_type
                    
                    mutated_df <- 
                      dplyr::mutate(
                        .data = df, 
                        frame_num = frame,
                        frame_time = frame * itvl,
                        frame_itvl = stringr::str_c(frame_time, itvl_u, sep = " "),
                        frame = NULL) %>% 
                      dplyr::filter(frame_num <= object@set_up$nom)
                    
                    if(isTimeLapseExp(object)){
                      
                      confuns::give_feedback(msg = glue::glue("Computing migration data from {phase} phase."), verbose = verbose)
                      
                      mutated_df <- 
                        dplyr::group_by(.data = mutated_df, cell_id) %>% 
                        dplyr::mutate(
                          dfo = compute_distances_from_origin(x_coords, y_coords),
                          dflp = compute_distances_from_last_point(x_coords, y_coords), 
                          speed = dflp / object@set_up$itvl
                        )
                      
                      final_df <-
                        dplyr::select(.data = mutated_df,
                                      cell_id, x_coords, y_coords, speed, dfo, dflp,
                                      dplyr::starts_with(match = "frame"), 
                                      dplyr::everything() 
                        ) %>% 
                        dplyr::select(-dplyr::starts_with(match = "well"))
                      
                    } else {
                      
                      final_df <- mutated_df
                      
                    }
                    
                    base::return(final_df)
                    
                  }) %>% 
      purrr::set_names(nm = getPhases(object))
    
    # compute statistics 
    object@data$stats <- 
      purrr::map2(.x = object@data$tracks,
                  .y = getPhases(object),
                  object = object,
                  .f = compute_cell_stats,
                  verbose = verbose
      ) %>% 
      purrr::set_names(nm = getPhases(object))
    
  } else {
    
    cnames <- base::colnames(object@data$stats$first)
    
    # shift cell location info to track data if available 
    if(base::all(c("x_coords", "y_coords") %in% cnames)){
      
      object@data$tracks$first <- 
        dplyr::select(object@data$stats$first, cell_id, phase, x_coords, y_coords) %>% 
        dplyr::mutate(frame_num = 1)
      
    }
    
    if("x_coords" %in% cnames){
      
      object@data$stats$first$x_coords <- NULL
      
    }
    
    if("y_coords" %in% cnames){
      
      object@data$stats$first$y_coords <- NULL
      
    }
    
    # process statistics
    object@data$stats$first <-
      dplyr::select(object@data$stats$first, -dplyr::starts_with("well"), -cell_line, -condition, -cl_condition)
    
  }
  
  # compute variable summary 
  confuns::give_feedback(msg = "Computing variable statistics.", verbose = verbose)
  
  object <- compute_variable_statistics(object)
  
  
  object@default <- c(object@default, default_list)
  
  object@set_up$progress$process_data <- TRUE
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


