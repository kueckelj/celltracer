
#' dummy
#' @return An updated version of the input \code{cypro}-object. 
updated_object <- function(){}

#' @rdname updated_object
update_object <- function(){}



# Data.frame documentation  -----------------------------------------------

#' cluster_df
#' @param cluster_df A data.frame that contains at least two variables:
#' 
#'  \describe{
#'   \item{\emph{cell_id}}{Character. The cells ids that are used to join the cluster variable unambiguously}
#'   \item{\code{cluster_name}}{Character or factor. The cluster variable that is to be joined.}
#'  }
#'  
cluster_df_descr <- function(cluster_df){}

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


#' @title argument_dummy
#' 
#' 
#' @inherit check_object params
#' @inherit check_phase params
#' @param across Character value or NULL. Specifies the grouping variable of interest.
#'
#' Use \code{getGroupingOptions()} to obtain all variable names that group the
#' cells of your experiment in a certain manner.
#'
#' @param across_subset Character vector or NULL. Specifies the particular groups
#' of interest the grouping variable specified in argument \code{across} contains.
#'
#' If set to NULL all of them are chosen. You can prefix groups you are NOT interested in
#' with a \emph{'-'}. (Saves writing if there are more groups you are interested in
#' than groups you are not interested in.)
#'
#' Use \code{getGroupNames()} to obtain all valid input options.
#'
#' @param add_on_list A list of ggplot2-add-ons that are supposed to be integrated in 
#' the visualization process.
#' @param cell_ids Character vector. Denotes the cell ids of interest. 
#' @param clrp Character value. Specifies the color palette to be used to represent
#' groups of discrete variables. Run \code{validColorPalettes()} to obtain valid
#' input options.
#'
#' @param clrp_adjust Named character vector or NULL. If character, it adjusts the
#' color palette that is used to represent the groups. Names of the input vector must refer
#' to the group and the respective named element denotes the color with which to
#' represent the group.
#'
#' @param clrsp Character value. Specifies the color spectrum to be used to represent
#' continuous values of numeric variables. Run \code{validColorSpectra()} to obtain
#' valid input options.
#'
#' @param discrete_feature Character value. Specifies the name of the grouping variable
#' of interest. Use \code{getGroupingOptions()} to obtain all valid input options.
#'
#' @param display_cols Logical value. If set to TRUE columns are used additionally
#' to display the results.
#' @param display_facets Logical value. If set to TRUE the plot is split via
#' \code{ggplot2::facet_wrap()} such that each variable gets it's own subplot.
#' @param display_legend Logical value. If set to TRUE a legend is displayed. 
#' @param display_line Logical value. If set to TRUE a line is used additionally
#' to display the results.
#' @param display_points Logical value. If set to TRUE points are used additionally
#' to display the results.
#' @param display_smooth Logical value. If set to TRUE a smoothed line is displayed to emphasize 
#' the overall trend of the data. Use the \code{smooth_*} arguments to additionally adjust they way
#' the trend is plotted. 
#' @param display_title Logical value. If set to TRUE an informative title is displayed.
#' @param force Logical value. Needs to be set to TRUE if you want to overwrite an already existing 
#' set up or already existing results. 
#' @param image Numeric value. The well-image of interest. 
#' @param linesize Numeric value. Denotes the size of the lines drawn. 
#' @param linetype Character value. Valid options are \emph{'solid', 'twodash', 'longdash', 'dotted'}
#' and \emph{'dotdash'}.
#' 
#' @param method_aggl Character vector (or value see details for more.) Denotes the agglomeration 
#' method(s) of interest according to which the existing distance matrices are agglomerated to hierarchical
#' trees. 
#' 
#' Use \code{validAgglomerationMethods()} to obtain all valid input options. 
#' 
#' @param method_corr Character value. Denotes the correlation method of interest. Either \code{'pearson'}
#' or \code{'spearman'}.
#' @param method_dist Character vector (or value see details for more.) Denotes the distance method(s)
#' of interest (e.g. \emph{'euclidean'} or \emph{'manhattan'}).
#' 
#' Use \code{validDistanceMethods()} to obtain all valid input options.
#' 
#' @param method_kmeans Character vector (or value see details for more.) Denotes the algorithm of interest defaults 
#' to \emph{'Hartigan-Wong'}. 
#' 
#' Use \code{validKmeansMethods()} to obtain all valid input options.
#' 
#' @param method_outlier Character vector. Specifies the method/algorithm of interest. 
#' 
#' Use \code{validOutlierDetectionMethods()} to obtain all valid input options. 
#' 
#' @param method_pam Character vector (or value see details for more.) Denotes the algorithm of interest. 
#' Valid input options are \emph{'euclidean'} and \emph{'manhattan'}.
#' 
#' @param n_cells Numeric calue. Determines the number of cells that are randomly chosen from 
#' every group to be displayed. Useful to keep plots insightful and aesthetically pleasing.
#' @param pretty_names Logical. If set to TRUE the function attempts to convert the concisely named 
#' variables into more aesthetically pleasing ones. 
#' @param pt_alpha Numeric value. Specifies the degree of transparency of all points.
#' @param pt_clr Character value. Specifies the color of all points.
#' @param pt_clrp The color palette to be used if the specified variable displayed by
#' color is categorical/discrete. Run \code{validColorPalettes()} to see valid input.
#' @param pt_clrsp The color spectrum to be used if the specified variable displayed by
#' color is continuous. Run \code{validColorSpectra()} to see valid input.
#' @param pt_size Numeric value. Specifies the size of all points.
#' 
#' @param relevel Logical value. If set to TRUE the input order of \code{across_subset}
#' determines the order in which the groups of interest are displayed. Groups that
#' are not included are dropped which affects the colors with which they are displayed.
#' 
#' @param scales,space,ncol,nrow Given to \code{ggplot2::facet_wrap()}. Affects the way the subplots
#' are displayed.
#'
#' @param simplify Logical. If set to TRUE the output list is simplified to a vector if possible. If set
#' to FALSE a list is returned.
#' 
#' @param smooth Logical. If set to TRUE the values are smoothed. 
#' @param smooth_clr Character value. Denotes the color of the smoothed line. 
#' @param smooth_method Character value. Denotes the model type used to display the line. Defaults to 
#' \emph{'lm'} (linear model). Given to argument \code{method} of function \code{ggplot2::geom_smooth()}.
#' @param smooth_se Logical. If set to TRUE the standard error will be displayed. 
#' @param smooth_span NUmeric value. Denotes the smoothing span used. 
#'
#' @param well_plate Character value. Denotes the well plate of interest. Use function \code{getWellPlateNames()}
#' to obtain all valid input options.
#' @param well Character value. The well of interest (e.g. \emph{'A1'}, \emph{'B12'})
#' @param well_plate Character value. The name of the well plate of interest. Valid inputs can be obtained 
#' via \code{getWellPlateNames()}.
#' @param with_cluster Logical. If set to TRUE the discrete variables of the meta data slot are added
#' to the output data.frame. 
#' @param with_meta Logical. If set to TRUE the discrete variables of the meta data slot are added
#' to the output data.frame. 
#' @param with_stats Logical. If set to TRUE the numeric variables of the stat data slots are added
#' tot he output data.frame. 
#' @param variable_set Character value. Denotes the variable set of interest. Use \code{getVariableSetNames()}
#' to obtain all names of currently stored variable sets in your object.
#' @param verbose Logical. If set to TRUE informative messages regarding
#' the computational progress will be printed.
#'
#' (Warning messages will always be printed.)
argument_dummy <- function(object, phase, pt_alpha, pt_clr, pt_clrp, pt_clrsp, pt_size){}




#' Title
#'
#' @return
#' @export
#'
ggplot_family <- function(){}



#' Title
#'
#' @return
#' @export
#'
variables_num <- function(){}

#' variables_num
#' @param variables Character vector. Denotes the numeric variables of interest.
#' Run \code{getNumericVariableNames()} with you cypro-object to obtain 
#' all valid input options.


