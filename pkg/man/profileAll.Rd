\name{profileAll}
\alias{profileAll}
\title{The profileAll function}
\description{ Supportive function for profile.bic used to plot all profile plots in single frame for biclusterd and clusterd data.}
\usage{
profileAll(dset, indg, indc, grp, gby = "genes", teta = 120, ph = 30)
}
\arguments{
  \item{dset}{Data Matrix}
  \item{indg}{indexed genes(rows) obtained from \code{\link{indexedBic}}}
  \item{indc}{indexed conditions(columns) obtained from \code{\link{indexedBic}}}

  \item{grp}{group genes(rows)/conditions(columns) as '1' for biclusterd or '2' out side biclusterd }
  \item{gby}{ grouped by 'genes' or 'conditions'}
  \item{teta}{numerical value to rotate the 3D; 0,90,180,..}
  \item{ph}{numerical value to rotate the 3D; 0,90,180,..}
}
\details{\code{\link{profileAll}} is a function which supports \code{\link{profileBic}} and provides all profile plots; 'line','histogram',
'boxplot','3D' of the biclusterd and clusterd data grouped either by 'genes' or 'conditions' and displayed in a single frame.
}
\value{
Profile plots for all biclusterd and clustered genes and conditions.
}
\author{
Mengsteab Aregay \email{mycs.zab@gmail.com}}

\seealso{
\code{\link{profileBic}}
}
\keyword{ profileAll}
