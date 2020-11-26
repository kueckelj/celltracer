
#' dummy
#' @return An updated cell tracer object. 
updated_object <- function(){}

#' dummy
#' @param object A valid cell tracer object. 
check_object <- function(object){}


# Data.frame documentation  -----------------------------------------------

#' cluster_df
#' @param cluster_df A data.frame that contains at least two variables:
#' 
#'  \describe{
#'   \item{\emph{cell_id}}{Character. The cells ids that are used to join the cluster variable unambiguously}
#'   \item{\code{cluster_name}}{Character or factor. The cluster variable that is to be joined.}
#'  }
#'  
cluster_df <- function(cluster_df){}

#' input_df
#' @param input_df A data.frame that contains at least two variables:
#' 
#'  \describe{
#'   \item{\emph{cell_id}}{Character. The cells ids that are used to join the cluster variable unambiguously}
#'   \item{\code{variable__name}}{Character or factor. The discrete variable that is to be joined.}
#'  }
#'  
input_df <- function(input_df){}


#' dim_red_df
#' @param dim_red_df A data.frame that contains at least two variables:
#' 
#'  \describe{
#'   \item{\emph{cell_id}}{Character. The cells ids that are used to join the cluster variable unambiguously}
#'   \item{\emph{x}}{Numeric. The cell id's position on the x-axis.}
#'   \item{\emph{y}}{Numeric. The cell id's position on the y-axis.}
#'  }
#'  
dim_red_df <- function(dim_red_df){}

# -----


# Miscellaneous -----------------------------------------------------------


#' add_on_list
#' @param add_on_list A list of ggplot2-add-ons that are supposed to be integrated in 
#' the visualization process.
#' 
add_on_list <- function(add_on_list){}

#' aes_to 
#' @param color_to Character value. The variable that is to be displayed or highlighted by the dots color. 
#' @param shape_to Character value. The variable that is to be displayed or highlighted by the dots shape. 
#' @param fill_to Character value. The varaible that is to be displayed or highlighted by the dots filling. 
#' 
aes_to <- function(color_to, fill_to, shape_to){}


#' cell_ids
#' @param cell_ids Character vector. Denotes the cell ids of interest. 
cell_ids <- function(cell_ids){}

#' colors
#' @param clrp Character value. The color panel to be used. Valid input options can be obtained
#' via \code{allColorpanels()}.
#' @param clrsp Character value. The color spectrum to be used for continuous variables. Valid 
#' input options can be obtained via \code{allColorspectra()}.
#' 
colors <- function(clrp, clrsp){}

#' dim_red_method
#' @param dim_red_method Character value. The dimensional reduction method.
#' 
dim_red_method <- function(dim_red_method){}

#' image
#' @param image Numeric value. The well-image of interest. 
#' 
image <- function(image){}


#' linetype 
#' @param linetype Character value. Valid options are \emph{'solid', 'twodash', 'longdash', 'dotted'}
#' and \emph{'dotdash'}.
#'
linetype <- function(linetype){}
  
#' linesize
#' @param linesize Numeric value. Denotes the size of the lines drawn. 
linesize <- function(linesize){}

#' n_cells
#' @param n_cells Numeric calue. Determines the number of cells that are randomly chosen from 
#' every group to be displayed. Useful to keep plots insightful and aesthetically pleasing.
#'
n_cells <- function(n_cells){}

#' phase_all
#' @param phase Character value. Refers to the phase of the experiment. Valid inputs are: 
#'  
#'  \describe{
#'   \item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
#'   \item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
#'   \item{\emph{'entire'}}{Uses the complete data of the entire timespan.}
#'   }  
#'   
#' Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
#' started right from the beginning. 
#' 
phase_all <- function(phase){}

#' phase_cluster
#' @param phase_cluster Character value. Refers to the phase of the experiment from which to take 
#' the clustering variables. Valid inputs are: 
#'  
#'  \describe{
#'   \item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
#'   \item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
#'   }  
#'   
#' Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
#' started right from the beginning. 
#' 
phase_cluster <- function(phase){}

#' phase_single
#' @param phase Character value. Refers to the phase of the experiment. Valid inputs are: 
#'  
#'  \describe{
#'   \item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
#'   \item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
#'   }  
#'   
#' Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
#' started right from the beginning. 
#' 
phase_single <- function(phase){}


#' pretty_names
#' @param pretty_names Logical. If set to TRUE the function attempts to convert the concisely named 
#' variables into more aesthetically pleasing ones. 
#' 
pretty_names <- function(pretty_names){}

#' pt_args 
#' @param pt_size,pt_alpha Numeric value. Denotes the size and alpha values of the points of the scatterplot.
#' @param pt_color Character value. Denotes the color of the points of the scatterplot. 
#' 
pt_args <- function(pt_color, pt_size, pt_alpha){}


  

  
#' well
#' @param well Character value. The well of interest (e.g. \emph{'A1'}, \emph{'B12'})
#'
well <- function(well){}


#' well_plate
#' @param well_plate Character value. The name of the well plate of interest. Valid inputs can be obtained 
#' via \code{getWellPlateNames()}.
#' 
well_plate <- function(well_plate){}


#' with_clsuter
#' @param with_cluster Logical. If set to TRUE the discrete variables of the meta data slot are added
#' to the output data.frame. 
#' 
with_cluster <- function(with_cluster){}

#' with_meta
#' @param with_meta Logical. If set to TRUE the discrete variables of the meta data slot are added
#' to the output data.frame. 
#' 
with_meta <- function(with_meta){}


#' with_stats
#' @param with_stats Logical. If set to TRUE the numeric variables of the stat data slots are added
#' tot he output data.frame. 
#' 
with_stats <- function(with_stats){}

#' 

#' verbose
#' @param verbose Logical. If set to TRUE informative messages regarding
#' the computational progress will be printed.
#'
#' (Warning messages will always be printed.)

verbose <- function(verbose){
  
}




#' @title Check smooth
#'
#' @param smooth Logical. If set to TRUE the values are smoothed. 
#' @param smooth_se Logical. If set to TRUE the standard error will be displayed. 
#' @param smooth_span NUmeric value. Denotes the smoothing span used. 

check_smooth <- function(smooth, smooth_se, smooth_span){}




#' @title Check pam input 
#'
#' @param k NUmeric value. The k-value used to calculate the pam-cluster. 

check_pam_input <- function(k){}



