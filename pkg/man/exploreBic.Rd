\name{exploreBic}
\alias{exploreBic}
\title{ The exploreBic function
}
\description{Provides exploratory plots for biclusterd and. clusterd data parallely.}
\usage{
exploreBic(dset, bres, gby ="genes", pfor ="mean", mname ="biclust", bnum =1)
}
\arguments{
  \item{dset}{Data matrix.}
  \item{bres}{Bicluster result.}
  \item{gby}{group bicluster; 'genes' or 'conditions'.}
  \item{pfor}{plot for 'mean', 'median', 'variance', 'mad', 'all', or 'quantile'.}
  \item{mname}{Method name; 'biclust', 'isa2' or 'fabia'}
  \item{bnum}{Existed biclusters; '1','2'...}
}
\details{The exploreBic function is mainly used for exploratory data analysis. It provides summary plots for  'mean' 'median', 'variance', 'mad' and 'quantile plot'.

The \code{\link{exploreBic}}function checked if the parameters are appropriately submitted and then it calls the \code{\link{indexedBic}}function to identify the biclust sub matrix and forwarded it to \code{\link{exploreCalc}} to be calculated its summary statistics. Finally, \code{\link{explorePlot}} will be invoked to display the required plot.
}
\value{
Summary plot will display according to the user specification.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}
\references{
  Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

  Hochreiter, S., Bodenhofer, U., Heusel, M.\emph{et al.} (2010).FABIA: factor analysis for bicluster acquisition. \emph{Bioinformatices}, 26, 1520-1527.
  
}

\seealso{\code{\link{exploreOnlybic}}}
\examples{
data(breastc)
# find bicluster using fabia algorithm
fab<- fabia(breastc)
# Plot the mean of biclusterd and clustered genes parallely.
exploreBic(dset=breastc,bres=fab,gby="conditions",pfor="mean",mname="fabia")
}
\keyword{exploreBic}
