#' @title UI logic: Load Data
#' 

moduleLoadDataUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(title = "Load Data", width = 12, 
               shiny::fluidRow(width = 12, 
                               shiny::column(width = 6, align = "left",
                                             shiny::wellPanel(
                                               shiny::fluidRow(width = 12, 
                                                               shiny::column(width = 12, 
                                                                             shiny::fluidRow(width = 12,
                                                                                             shiny::column(width = 12, 
                                                                                                           shiny::h4(shiny::strong("Assign Folder to Well Plate")) %>% 
                                                                                                             add_helper(content = helper_content$assign_folder)
                                                                                             )
                                                                             ), 
                                                                             shiny::fluidRow(width = 12,
                                                                                             shiny::column(width = 12, 
                                                                                                           shiny::plotOutput(outputId = ns("ld_well_plate_plot")),
                                                                                                           shiny::textOutput(outputId = ns("ld_chosen_dir"))
                                                                                             ),
                                                                             ), 
                                                                             shiny::fluidRow(width = 12, 
                                                                                             hs(4, shiny::h5(shiny::strong("Choose Well Plate:")),
                                                                                                   shiny::uiOutput(outputId = ns("ld_added_well_plates"))
                                                                                                ),
                                                                                             hs(4, shiny::h5(shiny::strong("If ambiguous:")),
                                                                                                   shiny::selectInput(inputId = ns("ld_keep_filetype"), 
                                                                                                                label = NULL, 
                                                                                                                choices = c( "keep .csv - files" = "csv$",
                                                                                                                             "keep .xls - files" = "xls$",
                                                                                                                             "keep .xlsx - files" = "xlsx$"), 
                                                                                                                selected = "csv$")
                                                                                                ),
                                                                                             hs(2, shiny::h5(shiny::strong("Include Subfolders")),
                                                                                                   shinyWidgets::materialSwitch(inputId = ns("ld_recursive"), 
                                                                                                                          label = NULL, 
                                                                                                                          value = TRUE,
                                                                                                                          status = "success")
                                                                                                ),
                                                                                             hs(2, shiny::h5(shiny::strong("Assign Folder:")),
                                                                                                   shinyFiles::shinyDirButton(id = ns("ld_well_plate_dir"), 
                                                                                                                        label = "Browse", 
                                                                                                                        title = NULL)
                                                                                                )
                                                                             )
                                                               )
                                               )
                                             )
                               ), 
                               shiny::column(width = 6,
                                             shiny::fluidRow(
                                               hs(1), hs(10, shiny::uiOutput(outputId = ns("ld_loading_box")), hs(1))
                                             ), 
                                             shiny::fluidRow(
                                               hs(1), hs(10,shiny::uiOutput(outputId = ns("ld_save_and_proceed_box"))), hs(1)
                                             )
                                             
                                )
               )
      )
    )
  )
  
}