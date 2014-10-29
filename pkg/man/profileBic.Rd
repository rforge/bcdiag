\name{profileBic}
\alias{profileBic}
\title{
The  profileBic function.
}
\description{Provides profile plots for biclustered and clustered data.
}
\usage{
profileBic(dset, bres, mname = c("fabia", "isa2", "biclust","bicare"), bplot = "all",
gby = "genes", bnum = 1, teta = 120, ph = 30)
}
\arguments{
  \item{dset}{data matrix.}
  \item{bres}{biclustering result.}
  \item{mname}{method name; 'biclust', 'isa2', 'fabia' or 'bicare'.}
  \item{bplot}{types of plots; 'all','lines', 'boxplot', 'histogram' or '3D'.}
  \item{gby}{grouped by; 'genes', or 'conditions'.}
  \item{bnum}{Existing biclusters; '1','2',...}
  \item{teta}{numerical value to rotate the 3D; 0, 90, 180,...}
  \item{ph}{numerical value to rotate the 3D; 0, 90, 180,...}
}
\details{
The profile.bic function checks if all parameters are correctly submitted and then identifies the biclustered and clustered data.

Note that the \code{"biclust"} option for \code{mname} will also accept results from the packages \pkg{iBBiG} and \pkg{rqubic}.}
\value{
profile.bic(dset, bres, mname="biclust", bplot="all", gby="genes", bnum=1, teta=120, ph=30)
}
\references{
Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

Kaiser S. and Leisch F. (2008). A Toolbox for Bicluster Analysis in R. \emph{Ludwigstrasse.} 33.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}

\examples{
# create the biclustering result
data(breastc)
library(biclust)
bic<- biclust(breastc, method=BCPlaid())
# 3 biclusters found

# 3D profile plot for biclustered and clustered data.
profileBic(dset=breastc,bres=bic,mname="biclust",
bplot="3D",gby="genes",teta=-30,ph=50,bnum=1)
}

\keyword{profileBic}

