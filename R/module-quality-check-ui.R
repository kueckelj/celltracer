#' @title UI logic: Quality Check
#' 

moduleQualityCheckUI <- function(id){
  
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::fluidRow(
      blue_box(title = "Quality Check", width = 12, 
               shiny::fluidRow(
                 shiny::column(width = 6,
                               shiny::wellPanel(
                                 shiny::h4(shiny::strong("Total number of measurements")) %>% 
                                   shinyhelper::helper(content = c("The number of measurements each cell undergoes can vary due to a variety of reasons (e.g. 'Cell Tracker' could not
                                                      identify it as a cell in an image, the cell temporarily moved out of the cameras range, the cell emerges from mitosis during the experiment,
                                                      etc.). In order not to include cells that were covered poorly this plot provides information about how many cells feature a certain number of 
                                                      measurements."),
                                                       type = "inline", title = "What does this plot dislay?",
                                                       buttonLabel = "Got it!", size = "s"),
                                 shiny::plotOutput(ns("total_meas"), brush = shiny::brushOpts(id = ns("brush_total_meas"), direction = "x")),
                                 shiny::HTML("<br>"),
                                 shiny::uiOutput(outputId = ns("total_meas_opt"))
                               )),
                 shiny::column(width = 6,
                               shiny::wellPanel(
                                 shiny::h4(shiny::strong("Total number of skipped measurements")) %>% 
                                   shinyhelper::helper(content = c("Cells might skip measurements due to a variety of reasons. In order not to include cells that skipped too many measurements this plot provides
                                                         information about how many cells skipped a certain number of measurements."),
                                                       type = "inline", title = "What does this plot dislay?",
                                                       buttonLabel = "Got it!", size = "s"),
                                 shiny::plotOutput(outputId = ns("skipped_meas"), brush = shiny::brushOpts(id = ns("brush_skipped_meas"), direction = "x")),
                                 shiny::HTML("<br>"),
                                 shiny::uiOutput(outputId = ns("skipped_meas_opt"))
                               ))
               ),
               HTML("<br>"),
               shiny::fluidRow(
                 shiny::column(width = 6,
                               shiny::wellPanel(
                                 shiny::h4(shiny::strong("First measurement")) %>% 
                                   shinyhelper::helper(content = c("This plot displays how many cells started to be covered at different measurements."),
                                                       type = "inline", title = "What does this plot dislay?",
                                                       buttonLabel = "Got it!", size = "s"),
                                 shiny::plotOutput(outputId = ns("first_meas"), brush = shiny::brushOpts(id = ns("brush_first_meas"), direction = "x")),
                                 shiny::HTML("<br>"),
                                 shiny::uiOutput(outputId = ns("first_meas_opt"))
                               )),
                 shiny::column(width = 6,
                               shiny::wellPanel(
                                 shiny::h4(shiny::strong("Last measurement")) %>% 
                                   shinyhelper::helper(content = c("This plot displays how many cells stopped to be covered at different measurements."),
                                                       type = "inline", title = "What does this plot dislay?",
                                                       buttonLabel = "Got it!", size = "s"),
                                 shiny::plotOutput(outputId = ns("last_meas"), brush = shiny::brushOpts(id = ns("brush_last_meas"), direction = "x")),
                                 shiny::HTML("<br>"),
                                 shiny::uiOutput(ns("last_meas_opt"))
                               )),
               ),
               shiny::fluidRow(width = 12, 
                               shiny::column(width = 2),
                               shiny::column(width = 8, 
                                             shinydashboard::box(title = "Save Filter Criteria & Proceed", width = 12,
                                                                 solidHeader = TRUE, status = "success",
                                                                 tags$h4(shiny::strong("Remaining Cells")), 
                                                                 shiny::plotOutput(outputId = ns("remaining_cells_plot")),
                                                                 shiny::fluidRow(
                                                                   shiny::column(width = 12, align = "center", 
                                                                                 shiny::actionButton(inputId = ns("apply_filter"), label = "Apply Filter"), 
                                                                                 shiny::actionButton(inputId = ns("qc_save_and_proceed"), label = "Save & Proceed")
                                                                   )
                                                                 )
                                             )
          
                               ), 
                               shiny::column(width = 2)
               )
      )
    )
  )
  
}











