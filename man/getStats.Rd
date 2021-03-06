% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get-family.R
\name{getStats}
\alias{getStats}
\alias{getStatsDf}
\title{Obtain stat data.frame}
\usage{
getStats(object, with_meta = TRUE, with_cluster = TRUE, phase = "first_tmt")

getStatsDf(object, with_meta = TRUE, with_cluster = TRUE, phase = "first_tmt")
}
\arguments{
\item{object}{A valid cell tracer object.}

\item{with_meta}{Logical. If set to TRUE the discrete variables of the meta data slot are added
to the output data.frame.}

\item{with_cluster}{Logical. If set to TRUE the discrete variables of the meta data slot are added
to the output data.frame.}

\item{phase}{Character value. Refers to the phase of the experiment. Valid inputs are:

\describe{
\item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
\item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
\item{\emph{'entire'}}{Uses the complete data of the entire timespan.}
}

Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
started right from the beginning.}
}
\value{
A data.frame with all numeric variables summarizing the measurements of
the track data.frame.
}
\description{
Obtain stat data.frame
}
