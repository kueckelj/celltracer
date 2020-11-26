
#' @title UI logic: Experiment Set Up
#' 

moduleExperimentDesignUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(title = "Experiment Design", width = 12, 
                               shiny::fluidRow(width = 12, 
                                               shiny::column(width = 6, 
                                               shiny::wellPanel(
                                                 shiny::fluidRow(
                                                   shiny::column(width = 12, 
                                             shiny::h4(shiny::strong("Well Plate Set Up")),
                                             # new well plate
                                             shiny::fluidRow(width = 12,
                                                             shiny::column(width = 4,
                                                                           shiny::h5(shiny::strong("Step 1:")),
                                                                           shiny::actionButton(inputId = ns("ed_new_well_plate"), 
                                                                                               label = "New Well Plate", 
                                                                                               width = "100%")
                                                             ),
                                                             shiny::column(width = 4,
                                                                           shiny::h5(shiny::strong("Well Plate:")),
                                                                           shiny::selectInput(inputId = ns("ed_well_plate"), 
                                                                                              label = NULL, 
                                                                                              choices = valid_well_plates, 
                                                                                              selected = "8x12 (96)")
                                                             )
                                             ),
                                             # well plate plot
                                             shiny::plotOutput(outputId = ns("ed_well_plate_plot"), 
                                                               brush = ns("well_plate_brush")),
                                             shiny::HTML("<br>"),
                                             # well information 
                                             shiny::fluidRow(width = 12, 
                                                             shiny::column(width = 3,
                                                                           shiny::h5(shiny::strong("Step 2:")),
                                                                           shiny::splitLayout(
                                                                             shiny::actionButton(inputId = ns("ed_add_well_info"), 
                                                                                                 label = "Add Info",
                                                                                                 width = "100%"), 
                                                                             shiny::actionButton(inputId = ns("ed_delete_well_info"), 
                                                                                                 label = "Delete Info", 
                                                                                                 width = "100%"), 
                                                                             cellWidths = c("50%", "50%")
                                                                             )
                                                                           ),
                                                             shiny::column(width = 3,
                                                                           shiny::h5(shiny::strong("Cell Line:")),
                                                                           shiny::textInput(inputId = ns("ed_cell_line"), 
                                                                                            label = NULL,
                                                                                            placeholder = "cell line") 
                                                             ),
                                                             shiny::column(width = 3,
                                                                           shiny::h5(shiny::strong("Condition:")),
                                                                           shiny::textInput(inputId = ns("ed_condition"), 
                                                                                            label = NULL, 
                                                                                            placeholder = "condition")
                                                             ), 
                                                             shiny::column(width = 3, 
                                                                           shiny::h5(shiny::strong("Color to:")),
                                                                           shinyWidgets::pickerInput(inputId = ns("ed_color_to"), 
                                                                                                     label = NULL, 
                                                                                                     choices = c("Cell Line" = "cell_line", "Condition" = "condition"), 
                                                                                                     selected = "condition", 
                                                                                                     multiple = FALSE)
                                                                           
                                                                           )
                                             ),
                                             shiny::HTML("<br>"),
                                             # add well plate
                                             shiny::fluidRow(width = 12, 
                                                             shiny::column(width = 3,
                                                                           shiny::h5(shiny::strong("Step 3:")),
                                                                           shiny::actionButton(inputId = ns("ed_add_well_plate"), 
                                                                                               label = "Add Well Plate",
                                                                                               width = "100%")
                                                             ), 
                                                             shiny::column(width = 3, 
                                                                           shiny::h5(shiny::strong("Well Plate Name:")), 
                                                                           shiny::textInput(inputId = ns("ed_well_plate_name"), 
                                                                                            label = NULL, 
                                                                                            placeholder = "well plate name")
                                                             ),
                                                             shiny::column(width = 3, 
                                                                           shiny::h5(shiny::strong("Images per Well:")), 
                                                                           shiny::numericInput(inputId = ns("ed_images_per_well"),
                                                                                               min = 0, value = 0, step = 1,
                                                                                               label = NULL)
                                                             ),
                                                             shiny::column(width = 3,
                                                                           shiny::h5(shiny::strong("Dismiss 'unknown':")),
                                                                           shinyWidgets::materialSwitch(inputId = ns("ed_dismiss_unknown"), 
                                                                                                        label = NULL, 
                                                                                                        value = FALSE, 
                                                                                                        status = "warning")
                                             )
                                             )
                               )))),
                               shiny::column(width = 6, 
                                             # overall information
                                             shiny::wellPanel(
                                               shiny::fluidRow(width = 12, 
                                                               shiny::column(width = 12,
                                                                             shiny::h4(shiny::strong("Overall Information")),
                                                                             shiny::textInput(inputId = ns("ed_experiment_name"),
                                                                                              placeholder = "experiment name", 
                                                                                              label = NULL), 
                                                                             shiny::textInput(inputId = ns("ed_responsible_researcher"), 
                                                                                              placeholder = "responsible researcher", 
                                                                                              label = NULL), 
                                                                             shiny::textInput(inputId = ns("ed_supervisor"), 
                                                                                              placeholder = "supervisor", 
                                                                                              label = NULL),
                                                                             shiny::fluidRow(width = 12, 
                                                                                             shiny::column(width = 4, 
                                                                                                           shiny::selectInput(inputId = ns("ed_software"),
                                                                                                                              label = "Software:",
                                                                                                                              choices = c("     ", "Cell Tracker" = "cell_tracker"))
                                                                                             ), 
                                                                                             shiny::conditionalPanel(condition = "input.ed_software == 'cell_tracker'", ns = ns,
                                                                                                                     shiny::column(width = 4, 
                                                                                                                                   shiny::selectInput(inputId = ns("ed_speed_unit_d"),
                                                                                                                                                      label = "Speed Unit (distance)", 
                                                                                                                                                      choices = c("nm", "um", "mm"))
                                                                                                                     ),
                                                                                                                     shiny::column(width = 4,
                                                                                                                                   shiny::selectInput(inputId = ns("ed_speed_unit_t"), 
                                                                                                                                                      label = "Speed Unit (time)", 
                                                                                                                                                      choices = c("sec", "min", "hour", "day"))
                                                                                                                     )
                                                                                             )
                                                                             )
                                                               )
                                               )
                                             ), 
                                             shiny::HTML("<br>"),
                                             # measurements
                                             shiny::wellPanel(
                                               shiny::fluidRow(width = 12, 
                                                               shiny::column(width = 12,
                                                                             shiny::h4(shiny::strong("Measurement Set Up  & Treatment")), 
                                                                             shiny::fluidRow(width = 12, 
                                                                                             shiny::column(width = 4, 
                                                                                                           shiny::numericInput(inputId = ns("ed_meas_num"), 
                                                                                                                               label = "Total number:",
                                                                                                                               min = 2, step = 1, value = 2)
                                                                                             ), 
                                                                                             shiny::column(width = 4,
                                                                                                           shiny::numericInput(inputId = ns("ed_meas_interval"), 
                                                                                                                               label = "Interval:", 
                                                                                                                               min = 1, step = 0.5, value = 1)
                                                                                             ), 
                                                                                             shiny::column(width = 4, 
                                                                                                           shiny::selectInput(inputId = ns("ed_interval_unit"), 
                                                                                                                              label = "Interval Unit:", 
                                                                                                                              choices = interval_options, 
                                                                                                                              selected = "hours")
                                                                                             )
                                                                             ) 
                                                               )
                                                ),
                                               shiny::fluidRow(width = 12, 
                                                               shiny::column(width = 6,
                                                                             shiny::uiOutput(outputId = ns("ed_treatment_start"))
                                                               ), 
                                                               shiny::column(width = 6, 
                                                                             shiny::uiOutput(outputId = ns("ed_control_variable"))
                                                               )
                                               )
                                             ),
                                             shiny::HTML("<br>"),
                                             shinydashboard::box(width = 12, title = "Save Experiment Design & Proceed",
                                                                 solidHeader = TRUE, status = "success",
                                                                 shiny::fluidRow(width = 12,
                                                                                 shiny::column(width = 12,
                                                                                               DT::dataTableOutput(outputId = ns("ed_well_plate_folders")))
                                                                 ), 
                                                                 shiny::HTML("<br><br>"),
                                                                 # proceed with data loading
                                                                 shiny::fluidRow(width = 12,
                                                                                 shiny::column(width = 12, align = "center",
                                                                                               shiny::actionButton(inputId = ns("ed_save_ed"), 
                                                                                                                   label = "Safe & Proceed" 
                                                                                               )
                                                                                 )
                                                                 )
                                             )

                               )
               )
      )
    )
  )
  
}
