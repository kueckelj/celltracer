

# Action buttons ----------------------------------------------------------

plot_and_save <- function(ns){
  
  shiny::tagList(
    shiny::fluidRow(
      hs(4,shiny::h5(shiny::strong("Plot:")),
         shiny::actionButton(inputId = ns("update_plot"), label = "Update")), 
      hs(4,shiny::h5(shiny::strong("Save:")), 
         shiny::downloadButton(outputId = ns("save_as_pdf"), label = "PDF"))
    )
  )
  
}

act_button <- function(...,
                       color = "primary",
                       style = "simple",
                       size = "sm"){
  
  shinyWidgets::actionBttn(
    ..., 
    color = color,
    style = style, 
    size = size
  )
  
}



# Layout ------------------------------------------------------------------

blue_box <- purrr::partial(.f = shinydashboard::box, solidHeader = TRUE, status = "primary")

well_panel <- function(...){
  
  shiny::wellPanel(
    shiny::fluidRow(
      shiny::column(width = 12, ...)
    )
  )
  
}


#' @title Horizontal Separation (width = 3)

hs <- function(width = 3, ..., offset = 0){
  
  shiny::column(width = width, ..., offset = offset)
  
}


# Picker inputs -----------------------------------------------------------

bar_position_picker_input <- function(ns, id = "bar_position", selected = "fill", ...){
  
  shinyWidgets::pickerInput(inputId = ns(id),
                            label = "Bar Position:",
                            choices = shiny_bar_positions, 
                            selected = selected,
                            ...)
  
}

phase_picker_input <- function(ns,
                               id = "phase",
                               choices = pretty_phases,
                               selected = pretty_phases[2],
                               ...){
  
  shinyWidgets::pickerInput(inputId = ns(id), 
                            label = "Phase:", 
                            choices = choices,
                            selected = selected,
                            ...
  )
  
}


phase_cluster_picker_input <- function(ns,
                                       id = "phase_cluster",
                                       ...){
  
  shinyWidgets::pickerInput(inputId = ns(id), 
                            label = "Phase Cluster:", 
                            choices = pretty_phases[1:2],
                            selected = pretty_phases[2],
                            ...
  )
  
}


across_picker_input <- function(ns,
                                id = "across",
                                choices,
                                selected = "condition",
                                ...){
  
  shinyWidgets::pickerInput(inputId = ns("across"), 
                            label = "Across Group:", 
                            choices = choices, 
                            selected = selected,
                            ...)
  
}


across_subset_picker_input <- function(ns,
                                       id = "across_subset",
                                       choices,
                                       selected,
                                       multiple = TRUE,
                                       ...){
  
  shinyWidgets::pickerInput(inputId = ns(id), 
                            label = "Subset Groups:", 
                            choices = choices, 
                            multiple = multiple, 
                            selected = selected)
  
}

numeric_variables_picker_input <- function(ns,
                                           id = "numeric_variables",
                                           choices,
                                           ...){
  
  shinyWidgets::pickerInput(inputId = ns(id), 
                            label = "Include Variables:", 
                            choices = choices, 
                            selected = choices, 
                            options = list(`actions-box`= TRUE),
                            multiple = TRUE, 
                            ...)
  
}


change_order_input <- function(ns,
                               id = "change_order",
                               items,
                               item_class = "default",
                               ...){
  
  shinyjqui::orderInput(
    inputId = ns(id), 
    label = "Change Order:", 
    items = items, 
    item_class = "default"
  )
  
}


clrp_picker_input <- function(ns, id, selected = "milo", ...){
  
  shinyWidgets::pickerInput(
    inputId = ns(id), 
    label = "Colorpanel:",
    choices = confuns::all_colorpanels() %>% purrr::flatten(), 
    selected = selected, 
    multiple = FALSE, 
    ...
  )
  
}

color_to_picker_input <- function(ns,
                                  id = "color_to",
                                  choices,
                                  selected = "condition",
                                  ...){
  
  shinyWidgets::pickerInput(inputId = ns("color_to"), 
                            label = "Color to:", 
                            choices = choices,
                            selected = selected,
                            multiple = FALSE, 
                            ...)
  
}



# -----


# Numeric inputs ----------------------------------------------------------

n_cells_numeric_input <- function(ns, id = "n_cells", value = 100, min = 10, max = 1000){
  
  shiny::numericInput(inputId = ns(id),
                      label = "Number of Cells:", 
                      value = value,
                      min = min,
                      max = max,
                      step = 1)
  
}



# Validation --------------------------------------------------------------

validate_timedisplaced_tmt <- function(object){
  
  shiny::validate(
    shiny::need(
      expr = time_displaced_tmt(object), 
      message = "Treatment includes entire timespan."
    )
  )
  
}


# -----