
#' @title UI logic: Experiment Set Up
#' 

moduleExperimentDesignUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(title = "Experiment Design", width = 12, 
               shiny::column(width = 12, 
                               shiny::fluidRow(width = 12, 
                                               # overall information
                                               shiny::fluidRow(width = 12, 
                                                               shiny::column(width = 5, 
                                                                             shiny::wellPanel(
                                                                               shiny::fluidRow(width = 12, 
                                                                                               shiny::column(width = 12,
                                                                                                             shiny::fluidRow(
                                                                                                               shiny::column(width = 6, 
                                                                                                                             shiny::h4(shiny::strong("1. Overall Information"))           
                                                                                                                             )
                                                                                                             ),
                                                                                                             shiny::fluidRow(
                                                                                                               shiny::column(width = 4, 
                                                                                                                             shiny::h5(shiny::strong("Experiment Name:")),
                                                                                                                             shiny::textInput(inputId = ns("ed_experiment_name"),
                                                                                                                                              placeholder = "experiment name", 
                                                                                                                                              label = NULL)
                                                                                                                             ), 
                                                                                                               shiny::column(width = 4, 
                                                                                                                             shiny::h5(shiny::strong("Storage Folder:")),
                                                                                                                             shinyFiles::shinyDirButton(id = ns("ed_experiment_dir"), 
                                                                                                                                                        label = "Browse", 
                                                                                                                                                        title = NULL)
                                                                                                                             ), 
                                                                                                               shiny::column(width = 4, 
                                                                                                                             shiny::h5(shiny::strong("Storage Directory:")), 
                                                                                                                             shiny::textOutput(outputId = ns("ed_experiment_path")))
                                                                                                             ),
                                                                                                             shiny::fluidRow(width = 12, 
                                                                                                                             shiny::column(width = 4, 
                                                                                                                                           shiny::h5(shiny::strong("Software:")),
                                                                                                                                           shiny::selectInput(inputId = ns("ed_software"),
                                                                                                                                                              label = NULL,
                                                                                                                                                              choices = c("     ", "Cell Tracker" = "cell_tracker", 
                                                                                                                                                                          "Cell Profiler & Others" = "cell_profiler")), 
                                                                                                                                           shiny::actionButton(inputId = ns("ed_software_save"), label = "Save & Proceed")
                                                                                                                             ),
                                                                                                                             shiny::conditionalPanel(condition = "input.ed_software == 'cell_profiler'", ns = ns, 
                                                                                                                                                     shiny::column(width = 8, 
                                                                                                                                                                   shiny::uiOutput(outputId = ns("ed_software_example")) 
                                                                                                                                                                   )
                                                                                                                                                     )
                                                                                                             )
                                                                                               )
                                                                               )
                                                                             )
                                                               ), 
                                                               
                                                               # measurements
                                                               shiny::column(width = 3,
                                                                             shiny::wellPanel(
                                                                               shiny::fluidRow(width = 12, 
                                                                                               shiny::column(width = 12,
                                                                                                             shiny::fluidRow(width = 12, 
                                                                                                                             shiny::column(width = 12, 
                                                                                                                                           shiny::fluidRow(
                                                                                                                                             shiny::column(width = 6, 
                                                                                                                                                           shiny::h4(shiny::strong("2. Imaging Set Up"))
                                                                                                                                                           )
                                                                                                                                           ), 
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
                                                                                                                                           shiny::uiOutput(outputId = ns("ed_imaging_set_up_save"))
                                                                                                                             )
                                                                                                             ) 
                                                                                               )
                                                                               )
                                                                             )
                                                               ), 
                                                               shiny::column(width = 4, 
                                                                             shiny::wellPanel(
                                                                               shiny::fluidRow(width = 12, 
                                                                                               shiny::column(width = 12, 
                                                                                                             shiny::h4(shiny::strong("3. Experiment Phases")), 
                                                                                                             shiny::fluidRow(width = 12, 
                                                                                                                             shiny::column(width = 8, 
                                                                                                                                           shiny::uiOutput(outputId = ns("ed_phases_number")), 
                                                                                                                                           shiny::uiOutput(outputId = ns("ed_phases_start")), 
                                                                                                                                           shiny::uiOutput(outputId = ns("ed_phases_save"))
                                                                                                                             )
                                                                                                             )
                                                                                               )
                                                                               )
                                                                             ) 
                                                               )
                                                               
                                               ),
                                               shiny::HTML("<br>"),
                                               shiny::wellPanel(
                                                 shiny::fluidRow(
                                                   shiny::column(width = 12, 
                                                                 shiny::h4(shiny::strong("4. Well Plate Set Up")),
                                                                 # new well plate
                                                                 shiny::fluidRow(width = 12,
                                                                                 shiny::column(width = 2,
                                                                                               shiny::h5(shiny::strong("Step 1:")),
                                                                                               shiny::actionButton(inputId = ns("ed_new_well_plate"), 
                                                                                                                   label = "New Well Plate", 
                                                                                                                   width = "100%")
                                                                                 ),
                                                                                 shiny::column(width = 2,
                                                                                               shiny::h5(shiny::strong("Well Plate:")),
                                                                                               shiny::selectInput(inputId = ns("ed_well_plate"), 
                                                                                                                  label = NULL, 
                                                                                                                  choices = valid_well_plates, 
                                                                                                                  selected = "8x12 (96)")
                                                                                 ), 
                                                                                 shiny::column(width = 2,
                                                                                               shiny::h5(shiny::strong("Well Plate Name:")), 
                                                                                               shiny::textInput(inputId = ns("ed_well_plate_name"), 
                                                                                                                label = NULL, 
                                                                                                                placeholder = "well plate name")), 
                                                                                 shiny::column(width = 2,
                                                                                               shiny::h5(shiny::strong("Covered Areas per Well:")), 
                                                                                               shiny::numericInput(inputId = ns("ed_images_per_well"),
                                                                                                                   min = 0, value = 0, step = 1,
                                                                                                                   label = NULL))
                                                                 ),
                                                                 # well information 
                                                                 shiny::fluidRow(width = 12, 
                                                                                 shiny::column(width = 4,
                                                                                               shiny::h5(shiny::strong("Step 2:")),
                                                                                               shiny::splitLayout(
                                                                                                 shiny::actionButton(inputId = ns("ed_add_well_info"), 
                                                                                                                     label = "Add Info",
                                                                                                                     width = "100%"), 
                                                                                                 shiny::actionButton(inputId = ns("ed_delete_well_info"), 
                                                                                                                     label = "Delete Info", 
                                                                                                                     width = "100%"), 
                                                                                                 cellWidths = c("50%", "50%")
                                                                                               ), 
                                                                                               shiny::fluidRow(width = 12, 
                                                                                                               shiny::column(width = 6,
                                                                                                                             shiny::h5(shiny::strong("Cell Line:")),
                                                                                                                             shiny::textInput(inputId = ns("ed_cell_line"), 
                                                                                                                                              label = NULL,
                                                                                                                                              placeholder = "cell line") 
                                                                                                               ),
                                                                                                               shiny::column(width = 6,
                                                                                                                             shiny::uiOutput(outputId = ns("ed_conditions"))
                                                                                                               )
                                                                                               )
                                                                                 ),
                                                                                 shiny::column(width = 8,
                                                                                               # well plate plot
                                                                                               shiny::plotOutput(outputId = ns("ed_well_plate_plot"), brush = ns("well_plate_brush"), hover = ns("well_plate_hover"))
                                                                                 )
                                                                                 
                                                                 ),
                                                                 shiny::HTML("<br>"),
                                                                 # add well plate
                                                                 shiny::fluidRow(width = 12, 
                                                                                 shiny::column(width = 2,
                                                                                               shiny::h5(shiny::strong("Step 3:")),
                                                                                               shiny::actionButton(inputId = ns("ed_add_well_plate"), 
                                                                                                                   label = "Add Well Plate",
                                                                                                                   width = "100%")
                                                                                 ),
                                                                                 shiny::column(width = 2,
                                                                                               shiny::h5(shiny::strong("Empty Wells:")),
                                                                                               shinyWidgets::materialSwitch(inputId = ns("ed_dismiss_unknown"), 
                                                                                                                            label = NULL, 
                                                                                                                            value = FALSE, 
                                                                                                                            status = "warning")
                                                                                 ), 
                                                                                 shiny::column(width = 8, 
                                                                                               shiny::h5(shiny::strong("Well Information:")), 
                                                                                               shiny::tableOutput(outputId = ns("ed_well_info")))
                                                                 )
                                                   )
                                                 )
                                               ),
                                             shiny::HTML("<br>"),
                                             shiny::uiOutput(outputId = ns("ed_save_and_proceed_box"))

               )
               
      )
      )
    )
  )
  
}
