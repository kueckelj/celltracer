

#' @title Rotate labels of ggplot output 
#' 
#' @description Allows to rotate the labels of the x-/y-axis 
#' of ggplot2 plots. Useful in case of overlap. 
#'
#' @param angle Numeric value. Denotes the angle with which 
#' the labels are to be rotated. 
#'
#' @return
#' @export
#'

rotateLabelsX <- function(angle = 90){
  
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = angle)
  )
  
}

#' @rdname rotateLabelsX
#' @export
rotateLabelsY <- function(angle = 90){
  
  ggplot2::theme(
    axis.text.y = ggplot2::element_text(angle = angle)
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
