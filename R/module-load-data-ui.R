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
                                                                                                           shiny::h4(shiny::strong("Assign Folder to Well Plate"))
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
                                             shinydashboard::box(title = "Missing Well-Image Directories", status = "warning", width = 12,
                                                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, 
                                                                 shiny::textOutput(outputId = ns("ld_all_missing_files"))
                                             ),
                                             shinydashboard::box(title = "Ambiguous Well-Image Directories", status = "warning", width = 12,
                                                                 solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                                                                 DT::dataTableOutput(outputId = ns("ld_ambiguous_directories"))
                                             ),
                                             shinydashboard::box(title = "Well Plate Status", status = "warning", width = 12, 
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
                                             ), 
                                             shinydashboard::box(title = "Load Files & Proceed", status = "success", width = 12, 
                                                                 solidHeader = TRUE, collapsible = FALSE, 
                                                                 shiny::uiOutput(outputId = ns("ld_well_plate_errors")),
                                                                 shiny::uiOutput(outputId = ns("ld_well_image_errors")),
                                                                 shiny::textOutput(outputId = ns("ld_error_message")),
                                                                 shiny::HTML("<br>"),
                                                                 shiny::column(width = 12, align = "center",
                                                                               shiny::splitLayout(
                                                                                 cellWidths = c("50%", "50%"),
                                                                                 shiny::actionButton(inputId = ns("ld_proceed"), label = "Save & Proceed"),
                                                                                 shiny::uiOutput(outputId = ns("ld_ignore_errors"))
                                                                               )
                                                                 )
                                             )
                                )
               )
      )
    )
  )
  
}