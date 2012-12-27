\name{explorePlot}
\alias{explorePlot}

\title{ The explorePlot function

}
\description{
An internal function supports for \code{\link{exploreBic}} and Provides different types of plots for biclustered and clustered data simultaneously. 
}
\usage{
explorePlot(sbic, obic, pfor = c("all", "mean", "median",
"variance", "mad", "quant"))
}

\arguments{
  \item{sbic}{ biclusterd data}
  \item{obic}{clusterd data }
  \item{pfor}{plot for 'mean', 'median', 'variance', 'mad', 'all', or 'quantile'.}
}

\details{
It has an important role to plot the summary statistics of biclustered and clustered data. it helps the \code{\link{exploreBic}} function by exracting different types of plots for the summary statistics of biclusterd and clusterd data.
}
\value{
Extracts the required summary plot.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}

\seealso{
\code{\link{exploreBic}},\code{\link{exploreCalc}}
}
\keyword{ explorePlot }

