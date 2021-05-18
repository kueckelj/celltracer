


# NOT EXPORTED ------------------------------------------------------------


#' Title
#'
#' @inherit argument_dummy params
run_dim_red <- function(object,
                        phase = NULL,
                        variables_subset = NULL, 
                        method_dim_red = "pca",
                        force = FALSE,
                        verbose = NULL,
                        ...){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  dim_red_obj <- object@analysis$dim_red[[method_dim_red]][[phase]]
  
  if(base::class(dim_red_obj) != "dim_red_conv" | base::isTRUE(force)){
    
    stat_df <- getStatsDf(object = object, 
                          phase = phase,
                          with_cluster = FALSE, 
                          with_meta = FALSE) %>% 
      dplyr::select(-phase)
    
    cell_ids <- stat_df$cell_id
    stat_df$cell_id <- NULL
    
    stat_df <- hlpr_select(stat_df, variables_subset = variables_subset)
    
    stat_df <- base::as.data.frame(stat_df)
    base::rownames(stat_df) <- cell_ids
    
    dim_red_obj <- 
      confuns::compute_dim_red(
        data = stat_df, 
        key.name = "cell_id", 
        method.dim.red = method_dim_red, 
        ...
      )
    
    msg <- glue::glue("Successfully calculated dimensional reduction (method = {method_dim_red}) for {phase} phase with variables: '{remaining_vars}'", 
                      remaining_vars = glue::glue_collapse(x = base::colnames(stat_df), sep = "', '", last = "' and '"))
    
    # remove data to prevent the object from becomming to big
    dim_red_obj@data <- base::matrix()
    dim_red_obj@meta <- base::data.frame()
    
    confuns::give_feedback(msg = msg, verbose = verbose, with.time = FALSE)
    
    
  } else {
    
    msg <- glue::glue("Dimensional reduction (method = {method_dim_red}) for {phase} phase already exists. Set argument 'force' to TRUE in order to overwrite it.")
    
    confuns::give_feedback(msg = msg, fdb.fn = "stop", with.time = FALSE)
    
  }
  
  object@analysis$dim_red[[method_dim_red]][[phase]] <- dim_red_obj
  
  base::return(object)
  
}

#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dim_red 
#'
#' @return
#'
get_dim_red_obj <- function(object, phase = NULL, method_dim_red){
  
  check_object(object)
  assign_default(object)
  
  phase <- check_phase(object, phase = phase, max_phases = 1)
  
  dim_red_obj <- object@analysis$dim_red[[method_dim_red]][[phase]]
  
  ref_fun <-
    stringr::str_c("run",
                   confuns::make_capital_letters(method_dim_red),
                   glue::glue("(..., phase = '{phase}')"), sep = "")
  
  check_availability(
    evaluate = base::class(dim_red_obj) == "dim_red_conv", 
    ref_input = glue::glue("{method_dim_red} results"), 
    ref_fun = ref_fun, 
    phase = phase
  )
  
  numeric_vars <- dim_red_obj@variables_num
  
  stat_df <- getStatsDf(object, phase = phase, verbose = FALSE)
  
  dim_red_obj@data <- 
    dplyr::select(stat_df, cell_id, dplyr::all_of(numeric_vars)) %>% 
    tibble::column_to_rownames(var = "cell_id") %>% 
    base::as.matrix()
  
  
  dim_red_obj@meta <- 
    dplyr::select(stat_df, where(base::is.character), where(base::is.factor))
  
  
  base::return(dim_red_obj)
  
}


#' Title
#'
#' @param object 
#' @param phase 
#' @param method_dim_red 
#' @param color_by 
#' @param color_aes 
#' @param clrp 
#' @param clrp_adjust 
#' @param clrsp 
#' @param pt_alpha 
#' @param pt_clr 
#' @param pt_fill 
#' @param pt_size 
#'
plot_dim_red <- function(object,
                         phase = NULL, 
                         method_dim_red = "pca", 
                         color_by = NULL, 
                         color_aes = "fill",
                         pt_clrp = NULL, 
                         clrp_adjust = NULL, 
                         pt_alpha = NULL, 
                         pt_clr = NULL,
                         pt_clrsp = NULL,
                         pt_fill = NULL,
                         pt_size = NULL){
  
  check_object(object)
  assign_default(object)
  
  dim_red_obj <- 
    get_dim_red_obj(object, phase = phase, method_dim_red = method_dim_red)
  
  confuns::plot_dim_red(
    dimred.obj = dim_red_obj, 
    clr.by = color_by,
    clr.aes = color_aes, 
    pt.clrp = pt_clrp, 
    pt.clrsp = pt_clrsp, 
    pt.alpha = pt_alpha, 
    pt.fill = pt_fill, 
    pt.shape = base::ifelse(color_aes == "fill", 21, 19), 
    pt.size = pt_size 
  )
  
}



# EXPORTED ----------------------------------------------------------------



#' @title Compute dimensional reductions
#' 
#' @description Reduces the dimensions of all specified numeric variables using the respective 
#' algorithm. 
#'
#' @inherit argument_dummy
#' @param variables_subset Character vector or NULL. Specifies the numeric variables the dimensional reduction
#' algorithms will include.
#' 
#' If set to NULL all of them are chosen. You can prefix variables you do NOT want to influence the clustering
#' with a \emph{'-'}. (Saves writing if there are more variables you are interested in
#' than variables you are not interested in.)
#' 
#' Use \code{getNumericVariableNames()} to obtain all valid input options.
#'
#' @return An updated celltracer object.
#' @export
#'
runPca <- function(object,
                   phase = NULL,
                   variables_subset = NULL, 
                   force = FALSE,
                   verbose = NULL,
                   ...){
  
  check_object(object)
  assign_default(object)
  
  object <- run_dim_red(object = object, 
                        phase = phase, 
                        method_dim_red = "pca",
                        variables_subset = variables_subset, 
                        force = force, 
                        verbose = verbose, 
                        ...)

  base::return(object)  
  
}


#' @rdname runPca
#' @export
runTsne <- function(object,
                     phase = NULL,
                     variables_subset = NULL, 
                     force = FALSE,
                     verbose = NULL,
                     ...){
  
  check_object(object)
  assign_default(object)
  
  object <- run_dim_red(object = object, 
                        phase = phase, 
                        method_dim_red = "tsne",
                        variables_subset = variables_subset, 
                        force = force, 
                        verbose = verbose, 
                        ...)
  
  base::return(object)  
  
}

#' @rdname runPca
#' @export
runUmap <- function(object,
                    phase = NULL,
                    variables_subset = NULL, 
                    force = FALSE,
                    verbose = NULL,
                    ...){
  
  check_object(object)
  assign_default(object)
  
  object <- run_dim_red(object = object, 
                        phase = phase, 
                        method_dim_red = "umap",
                        variables_subset = variables_subset, 
                        force = force, 
                        verbose = verbose, 
                        ...)
  
  base::return(object)  
  
}




# get ---------------------------------------------------------------------





#' @title Obtain dimensional reduction objects
#' 
#' @description Returns the S4 objects in which the dimensional reduction
#' results are stored. 
#'
#' @inherit argument_dummy params
#' 
#' @return An S4 object of class \emph{'dim_red_conv'}.
#' @export
#'
getPcaConv <- function(object, phase = NULL){
  
  get_dim_red_obj(object = object, 
                  phase = phase, 
                  method_dim_red = "pca")
  
}


#' @rdname getPcaConv
#' @export
getTsneConv <- function(object, phase = NULL){
  
  get_dim_red_obj(object = object, 
                  phase = phase, 
                  method_dim_red = "tsne")
  
}

#' @rdname getPcaConv
#' @export
getUmapConv <- function(object, phase = NULL){
  
  get_dim_red_obj(object = object, 
                  phase = phase, 
                  method_dim_red = "umap")
  
}





# plot --------------------------------------------------------------------


#' @title Plot dimensional reduction results 
#' 
#' @description Visualizes dimensional reduction in a scatterplot.
#'
#' @inherit argument_dummy params
#' @param color_by Character value or NULL. If character, denotes either the numeric- or grouping variable whoose values
#' are displayed by color. If set to NULL the color is specified by the argument \code{pt_clr}.
#' @param color_aes Character value. Only relevant if \code{color_by} is specified. 
#' Denotes the aesthetic with which colors are displayed. Either \emph{'color'} or \emph{'fill'}. Depending on that as well as the input for argument
#' \code{pt_shape} the design of the geometric objects (points) varies according to the rules 
#' of the ggplot2-framework.
#'
#' @inherit ggplot_family return
#' @export
#'
plotPca <- function(object,
                     phase = NULL, 
                     color_by = NULL, 
                     color_aes = "fill",
                     pt_clrp = NULL, 
                     pt_clrsp = NULL,
                     pt_alpha = NULL, 
                     pt_clr = NULL,
                     pt_fill = NULL,
                     pt_size = NULL, 
                     clrp_adjust = NULL){
  
  plot_dim_red(
    object = object, 
    phase = phase, 
    method_dim_red = "pca",
    color_by = color_by, 
    color_aes = color_aes, 
    pt_clrp = pt_clrp, 
    pt_clrsp = pt_clrsp, 
    pt_alpha = pt_alpha, 
    pt_clr = pt_clr, 
    pt_fill = pt_fill, 
    pt_size = pt_size, 
    clrp_adjust = clrp_adjust
  )
  
}


#' @rdname plotPca
#' @export

plotTsne <- function(object,
                    phase = NULL, 
                    color_by = NULL, 
                    color_aes = "fill",
                    pt_clrp = NULL, 
                    pt_clrsp = NULL,
                    pt_alpha = NULL, 
                    pt_clr = NULL,
                    pt_fill = NULL,
                    pt_size = NULL, 
                    clrp_adjust = NULL){
  
  plot_dim_red(
    object = object, 
    phase = phase, 
    method_dim_red = "tsne",
    color_by = color_by, 
    color_aes = color_aes, 
    pt_clrp = pt_clrp, 
    pt_clrsp = pt_clrsp, 
    pt_alpha = pt_alpha, 
    pt_clr = pt_clr, 
    pt_fill = pt_fill, 
    pt_size = pt_size, 
    clrp_adjust = clrp_adjust
  )
  
}


#' @rdname plotPca
#' @export
plotUmap <- function(object,
                    phase = NULL, 
                    color_by = NULL, 
                    color_aes = "fill",
                    pt_clrp = NULL, 
                    pt_clrsp = NULL,
                    pt_alpha = NULL, 
                    pt_clr = NULL,
                    pt_fill = NULL,
                    pt_size = NULL, 
                    clrp_adjust = NULL){
  
  plot_dim_red(
    object = object, 
    phase = phase, 
    method_dim_red = "umap",
    color_by = color_by, 
    color_aes = color_aes, 
    pt_clrp = pt_clrp, 
    pt_clrsp = pt_clrsp, 
    pt_alpha = pt_alpha, 
    pt_clr = pt_clr, 
    pt_fill = pt_fill, 
    pt_size = pt_size, 
    clrp_adjust = clrp_adjust
  )
  
}






























