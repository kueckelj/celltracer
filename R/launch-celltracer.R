
#' @title Access interactive application 
#' 
#' @description Opens an interactive application in which most of the 
#' data visualization can be done conveniently.
#'
#' @inherit check_object params
#'
#' @export
#'

launchCellTracer <- function(object = NULL){
  
  base::stopifnot(methods::is(object, "cto"))
  
  new_object <- 
    shiny::runApp(
      shiny::shinyApp(
        ui = function(){
          shinydashboard::dashboardPage(
            
            header = shinydashboard::dashboardHeader(title = app_title), 
            
            sidebar = shinydashboard::dashboardSidebar(
              collapsed = FALSE, 
              shinydashboard::sidebarMenu(
 
                # experiment overview
                shinydashboard::menuItem(
                  text = "Experiment Overview", 
                  tabName = "experiment_overview"
                ), 
                # cluster analysis
                shinydashboard::menuItem(
                  text = "Cluster Analysis",
                  tabName = "cluster_analysis"
                ),
                # descriptive statistics
                shinydashboard::menuItem(
                  text = "Descriptive Statistics", 
                  tabName = "descriptive_statistics"
                ), 
                # cell velocity
                shinydashboard::menuItem(
                  text = "Cell Velocity", 
                  tabName = "cell_velocity"
                ), 
                # cell migration 
                shinydashboard::menuItem(
                  text = "Cell Migration", 
                  tabName = "cell_migration"
                )
              )
            ), 
            
            body = shinydashboard::dashboardBody(
              
              shinybusy::add_busy_spinner(spin = "cube-grid", margins = c(0, 10), color = "red"),
              
              shinydashboard::tabItems(
                
                # experiment overview
                shinydashboard::tabItem(tabName = "experiment_overview"), 
                
                # cluster analysis
                shinydashboard::tabItem(
                  tabName = "cluster_analysis",
                  
                  moduleClusteringPamUI(id = "pam")
                  
                  ),
                
                # descriptive statistics
                shinydashboard::tabItem(
                  tabName = "descriptive_statistics",
                  
                  shiny::fluidRow(
                    moduleDescrStatPlotUI(id = "descr_stat", module_width = 6, module_headline = "Descriptive Statistics"),
                    moduleCategStatPlotUI(id = "cat_stat", module_width = 6, module_headline = "Categorical Statistics"),
                  )
                  
                ), 
                
                # cell velocity 
                shinydashboard::tabItem(
                  tabName = "cell_velocity", 
                  shiny::fluidRow(
                    moduleVelocityHeatmapUI(id = "cv_heatmap", module_width = 6, module_headline = "Velocity Heatmap"),
                    moduleVelocityLineplotUI(id = "cv_lineplot", module_width = 6, module_headline = "Velocity Lineplot")
                  )
                ), 
                
                # cell migration 
                shinydashboard::tabItem(
                  tabName = "cell_migration", 
                  shiny::fluidRow(
                    moduleAllTracksUI(id = "all_tracks", module_width = 6, module_headline = "Migration Plot")
                  )
                )
                
                
                
                
              )
              
            )
          )
          
          
          
          
        }, 
        server = function(input, output, server){
          
          # shiny helper 
          shinyhelper::observe_helpers()
          
          # shiny busy indicator
          shinybusy::add_busy_spinner(color = "red")


# Core reactive values and set up -----------------------------------------

          object <- shiny::reactiveVal(value = object)

# Tab: Cluster Analysis ---------------------------------------------------
          
          cluster_analyis <- 
            moduleClusteringPamServer(id = "pam", object = object())
          
          # --------------------

# Tab: Descriptive Statistics -------------------------------------------
          
          moduleDescrStatPlotServer(id = "descr_stat", object = object())
          moduleCategStatPlotServer(id = "cat_stat", object = object())
          
          # --------------------
          
          

# Tab: Cell Velocity ----------------------------------------------------

          moduleVelocityHeatmapServer(id = "cv_heatmap", object = object())
          moduleVelocityLineplotServer(id = "cv_lineplot", object = object())
          
          # --------------------
          
          

# Tab: Cell Migration ---------------------------------------------------
          
          moduleAllTracksServer(id = "all_tracks", object = object())
          
          # --------------------
          

          
          
          
        }
      )
    )
  
  
}
