% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-family.R
\name{plotAllTracks}
\alias{plotAllTracks}
\title{Plot cell migration}
\usage{
plotAllTracks(
  object,
  across = "condition",
  across_subset = NULL,
  time_subset = NULL,
  phase = "all",
  phase_cluster = "first_tmt",
  n_cells = 100,
  color_to = NULL,
  linetype = "solid",
  linesize = 0.75,
  clrp = "milo",
  ...,
  verbose = TRUE
)
}
\arguments{
\item{object}{A valid cell tracer object.}

\item{across}{Character value. Denotes the discrete variable in the data.frame
across which the variables of interest are to be analyzed or displayed. Valid input
options can be obtained via \code{getAcrossOptions()}.}

\item{across_subset}{Character vector. The groups of interest that the \code{across}-
variable contains. Valid input options can be obtained via \code{getVariableValues()}.}

\item{time_subset}{Numeric value. Refers to the time up to which the migration
is plotted. If set to NULL the entire timespan is used.}

\item{phase}{Character value. Refers to the phase of the experiment. Valid inputs are:

\describe{
\item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
\item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
\item{\emph{'entire'}}{Uses the complete data of the entire timespan.}
}

Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
started right from the beginning.}

\item{phase_cluster}{Character value. Refers to the phase of the experiment from which to take
the clustering variables. Valid inputs are:

\describe{
\item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
\item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
}

Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
started right from the beginning.}

\item{n_cells}{Numeric calue. Determines the number of cells that are randomly chosen from
every group to be displayed. Useful to keep plots insightful and aesthetically pleasing.}

\item{color_to}{Character value. The variable that is to be displayed or highlighted by the dots color.}

\item{linetype}{Character value. Valid options are \emph{'solid', 'twodash', 'longdash', 'dotted'}
and \emph{'dotdash'}.}

\item{linesize}{Numeric value. Denotes the size of the lines drawn.}

\item{verbose}{Logical. If set to TRUE informative messages regarding
the computational progress will be printed.

(Warning messages will always be printed.)}
}
\value{
A ggplot.
}
\description{

}
