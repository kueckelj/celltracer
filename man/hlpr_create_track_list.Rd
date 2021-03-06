% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper-family.R
\name{hlpr_create_track_list}
\alias{hlpr_create_track_list}
\title{Process Track Data.frame}
\usage{
hlpr_create_track_list(phase, track_df)
}
\arguments{
\item{phase}{Character value. Refers to the phase of the experiment. Valid inputs are:

\describe{
\item{\emph{'before_tmt'}}{Uses the data that refers to the time before the treatment.}
\item{\emph{'first_tmt'}}{Uses the data that refers tot the time after the treatment.}
}

Note: This argument is ignored if the experiment set up did not include any treatment or the treatment
started right from the beginning.}

\item{track_df}{A data.frame in which each observation refers to a cell at a given frame and
that contains the following variables:

\describe{
\item{\emph{cell_id}}{Character. Refers to the cell id's.}
\item{\emph{x_coords}}{Numeric. Refers to the cell's x-coordinates.}
\item{\emph{y_coords}}{Numeric. Refers to the cell's y-coordinates.}
\item{\emph{dfo}}{Numeric. The distances from the first position tracked. (Distance from origin).}
\item{\emph{dfpl}}{Numeric. The distances from the last point.}
\item{\emph{speed}}{Numeric. Refers to the instantaneuous speed.}
\item{\emph{afo}}{Numeric. The angle from the position of origin.}
\item{\emph{aflp}}{Numeric. The angle from the last position.}
\item{\emph{frame_num}}{Numeric. The frame number the observation refers to.}
\item{\emph{frame_time}}{Numeric. The frame number multiplied with the interval between two frames}
\item{\emph{frame_itvl}}{Character. The frame time combined with the intervals unit}
}}
}
\description{
Filters the whole track data frame into it's phase-subparts
and recomputes the variable \emph{dfo (= Distance from origin)} if the
filtered part does not belong to the phase \code{'before_tmt'}.

To be used as input for \code{.f} in \code{purrr::map()}
inside the function \code{compileCto()}.
}
