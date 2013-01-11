\name{profileBic}
\alias{profileBic}
\title{
The  profileBic function.
}
\description{Provides profile plots for biclusterd and clusterd data parallely.
}
\usage{
profileBic(dset, bres, mname = c("fabia", "isa2", "biclust"), bplot = "all",
gby = "genes", bnum = 1, teta = 120, ph = 30)
}
\arguments{
  \item{dset}{Data matrix.}
  \item{bres}{Bicluster result.}
  \item{mname}{Method name; 'biclust', 'isa2' or 'fabia'}
  \item{bplot}{ types of plots;'all','lines','boxplot','histogram' or '3D'}
  \item{gby}{grouped by; 'genes', or 'conditions'}
  \item{bnum}{Existed biclusters; '1','2'...}
  \item{teta}{numerical value to rotate the 3D; 0,90,180,..}
  \item{ph}{numerical value to rotate the 3D; 0,90,180}
}
\details{
The profile.bic function checks all parameters are correctly submitted then it calls a function \code{\link{indexedBic}} to identify the biclusterd and clusterd data.}
\value{
profile.bic (dset, bres, mname="biclust", bplot="all", gby="genes", bnum=1, teta=120, ph=30)
}
\references{
Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

Kaiser S. and Leisch F. (2008). A Toolbox for Bicluster Analysis in R. \emph{Ludwigstrasse.} 33.
}

\author{Mengsteab Aregay \email{mycs.zab@gmail.com}}
\seealso{\code{\link{profileAll}}}
\examples{
#manupilate the biclust result
data(breastc)
bic<- biclust(breastc, method=BCPlaid())# 3 bicluster found.
#3D profile plot for biclusterd and clusterd data.
profileBic(dset=breastc,bres=bic,mname="biclust",
bplot="3D",gby="genes",teta=-30,ph=50,bnum=1)
}

\keyword{profileBic}

