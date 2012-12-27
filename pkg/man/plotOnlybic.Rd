\name{plotOnlybic}
\alias{plotOnlybic}

\title{ The plotOnlybic function}
\description{Supportive function for \code{\link{exploreBic}} used to plot summary statistics of biclusterd data.
}
\usage{
plotOnlybic(ball, fit = "all", gby)
}
\arguments{
  \item{ball}{ List of summary statistics('mean','median','variance','mad').}
  \item{fit}{fit a plot for 'mean', 'median', 'variance', 'mad', 'all', or 'quantile'.}
  \item{gby}{ grouped by 'genes' or 'conditions'.}
}
\details{A supportive function provides plots for the summary statistics of biclusterd data such as 'mean','median','variance', 'mad' or 'all'. if 'all' is selected then it will be displayed all the summary statistics together in single frame.}
\value{ Summary plot according to the user requirement
}
\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}

\seealso{exploreOnlybic}
\keyword{plotOnlybic}