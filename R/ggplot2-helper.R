

#' @title Manipulate labels of ggplot output 
#' 
#' @description Allows to rotate the labels of the x-/y-axis 
#' of ggplot2 plots. Useful in case of overlap. 
#'
#' @param angle Numeric value. Denotes the angle with which 
#' the labels are to be rotated. 
#'
#' @return gg object that can be added to ggplots via the \code{+}-operator
#' @export
#'

labelsXrotate <- function(angle = 90){
  
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = angle)
  )
  
}

#' @rdname labelsXrotate
#' @export
labelsYrotate <- function(angle = 90){
  
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(angle = angle)
  )
  
}

#' @rdname labelsXrotate
#' @export
labelsXremove <- function(){
  
  ggplot2::theme(
    axis.text.x = ggplot2::element_blank(),
    axis.ticks.x = ggplot2::element_blank()
  )
  
}

#' @rdname labelsXrotate
#' @export
labelsYremove <- function(){
  
  ggplot2::theme(
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )
  
}


#' @title Specify legend position of ggplot output
#' @export
legendBottom <- purrr::partial(.f = ggplot2::theme, legend.position = "bottom")

#' @rdname legendBottom
#' @export
legendNone <- purrr::partial(.f = ggplot2::theme, legend.position = "none")

#' @rdname legendBottom
#' @export
legendRight <- purrr::partial(.f = ggplot2::theme, legend.position = "right")

#' @rdname legendBottom
#' @export
legendTop <- purrr::partial(.f = ggplot2::theme, legend.position = "top")
