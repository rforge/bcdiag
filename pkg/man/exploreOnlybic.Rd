\name{exploreOnlybic}
\alias{exploreOnlybic}
\title{ The exploreOnlybic function
}
\description{Provides exploratory plots only for biclusterd.}
\usage{
exploreOnlybic(dset, bres, fit= "all", gby= "genes", mname="biclust",bnum=1)
}
\arguments{
  \item{dset}{Data matrix.}
  \item{bres}{Bicluster result.}
  \item{gby}{group bicluster; 'genes' or 'conditions'.}
  \item{fit}{ fit a plot for 'mean', 'median', 'variance', 'mad', 'all', or 'quantile'.}
  \item{mname}{Method name; 'biclust', 'isa2' or 'fabia'}
  \item{bnum}{Existed biclusters; '1','2'...}
}
\details{The exploreOnlybic function has similar function with \code{\link{exploreBic}}. the only difference is, it provides exploratory plots only for biclusterd data. It provides summary plots for  'mean' 'median', 'variance', 'mad' and 'quantile plot'.
}
\value{
Summary plot will display only for biclusterd data.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}
\references{
  Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

  Hochreiter, S., Bodenhofer, U., Heusel, M.\emph{et al.} (2010).FABIA: factor analysis for bicluster acquisition. \emph{Bioinformatices}, 26, 1520-1527.
  
}

\seealso{
\code{\link{exploreBic}}}
\examples{
data(breastc)
# find bicluster using fabia algorithm
fab<- fabia(breastc)
# Plot the median of biclusterd data.
exploreOnlybic(dset=breastc,bres=fab,fit="all",gby="genes",mname="fabia",bnum=1)
}
\keyword{exploreOnlybic}
