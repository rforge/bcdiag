\name{indexedBic}
\alias{indexedBic}
\title{ The indexedBic function.
}
\description{
Supportive function for \code{\link{profileBic}},  \code{\link{exploreBic}}, \code{\link{exploreOnlybic}} and \code{\link{anomedOnlybic}} used to extract biclusterd index from a dataset based on 'biclust','fabia' and 'isa2' algorithms.
}
\usage{
indexedBic(dset, bres, mname = c("fabia", "isa2", "biclust"), bnum)
}

\arguments{
  \item{dset}{ Data matrix }
  \item{bres}{ Bicluster result }
  \item{mname}{Method name; 'biclust', 'isa2' or 'fabia'}
  \item{bnum}{Existed biclusters; '1','2'...}
}
\details{
Its main purpose is for internal usage and extracts the index of biclusterd rows and columns from the dataset and returns to the main function. by knowing the indexed of the biclusterd data, we can easily identified and grouped the biculsterd data  from the clusterd ones.}
\value{ Indicies of biclustered data.
}
\author{
Mengsteab Aregay  \email{mycs.zab@gmail.com}
}
\seealso{
\code{\link{profileBic}}, \code{\link{exploreBic}}, \code{\link{exploreOnlybic}}, \code{\link{anomedOnlybic}}}
\keyword{indexedBic}

