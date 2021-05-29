#' @title Object initiation: Step 1
#' 
#' @description Opens an interactive application in which the experiment can be set 
#' up. This includes in particular the design of the well plates. 
#'
#' @return An empty cypro object. 
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
                                   label = "Return Cypro Object", 
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
#' @return An updated cypro object.
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
                                   label = "Return Cypro Object", 
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
#' @inherit updated_object return
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
                                   label = "Return Cypro Object", 
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
#' cypro object is set for all subsequent analysis and visualization steps. 
#'
#' @inherit argument_dummy params
#'
#' @inherit updated_object return
#' @export
#'
processData <- function(object, verbose = TRUE){
  
  check_object(object, set_up_req = "quality_check")
  
  if(base::isTRUE(object@set_up$progress$process_data)){
    
    base::stop("Object has already been processed.")
    
  }

  # update progress slot
  object@set_up$progress$process_data <- TRUE
  
  # declare module usability ------------------------------------------------
  
  if(isTimeLapseExp(object)){
    
    # module: migration (TRUE if coordinates vars are available)
    coords_available <- 
      base::all(c("x_coords", "y_coords") %in% base::names(object@set_up$example$denoted_columns))
    
    object@information$modules$migration <- coords_available
    
  } else {
    
    # module: migration 
    object@information$modules$migration <- FALSE
    
  }

  confuns::give_feedback(msg = "Processing data.", verbose = verbose)
  
  if(isTimeLapseExp(object)){
    
    confuns::give_feedback(msg = "Counting missing values.", verbose = verbose)
    
    # summarise how many missing values every cell id has across all variables
    track_df <- 
      purrr::map_df(object@cdata$tracks, .f = ~ .x)
    
    all_cell_ids <- base::unique(track_df$cell_id)
    all_frames <- 1:object@set_up$nom
    
    object@information$track_na_count <- 
      tidyr::expand_grid(cell_id = {{all_cell_ids}}, frame = {{all_frames}}) %>% 
      dplyr::left_join(x = ., y = track_df, by = c("cell_id", "frame")) %>% 
      dplyr::group_by(cell_id) %>% 
      dplyr::summarise_all(.funs = function(var){ base::is.na(var) %>% base::sum()}) %>% 
      dplyr::select(-phase, -frame)
    
  }
  
  # set up different data slots ---------------------------------------------
  
  # create wp data
  object <- set_up_cdata_well_plate(object, verbose = verbose)
  
  # create meta data 
  object <- set_up_cdata_meta(object, verbose = verbose)
  
  # create cluster data 
  object <- set_up_cdata_cluster(object, verbose = verbose)
  
  # create stats and tracks 
  object <- set_up_cdata_tracks_and_stats(object, verbose = verbose)
  
  # create variable data
  object <- set_up_vdata(object, verbose = verbose)
  

  # miscellaneous -----------------------------------------------------------
  
  # remove wp_df in favor of wp_df_eval
  object@well_plates <-
    purrr::map(.x = object@well_plates, .f = function(wp_list){
                 
                 wp_list$wp_df <- NULL
                 
                 base::return(wp_list)
                 
                 })
  
  # add default list 
  object@default <- c(object@default, default_list)
  
  confuns::give_feedback(msg = "Done.", verbose = verbose)
  
  base::return(object)
  
}


