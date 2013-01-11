\name{writeBic}
\alias{writeBic}
\title{The writeBic function} 

\description{Provides a summary output in a text format, extracted from 'biclust','isa2' and 'fabia' bicluster algorithms. }

\usage{
writeBic(dset, fileName, bicResult, bicname, 
mname = c("fabia", "isa2", "biclust"), append = TRUE, delimiter = " ")
}

\arguments{
  \item{dset}{Data matrix }
  \item{fileName}{ The name of the bicluster file to be saved.}
  \item{bicResult}{bicluster result obtained from 'biclust','isa2' or 'fabia'}
  \item{bicname}{ the tilte to be given for the biclusterd data.}
  \item{mname}{ method name; 'biclust', 'isa2' or 'fabia'}
  \item{append}{logical value; 'true' or 'false}
  \item{delimiter}{ default value is " ".}
}

\details{ The original function was developed in 'biclust' package by Kaiser \emph{et.al} (2008). we extend the function to be used for further bicluster algorithms, such as; 'isa2' and 'fabia'.}

\value{ Biclusterd text file with title, total number of biclusterd, dimention and name of the biclusterd genes(rows) or conditions(columns).}

\references{
Van't Veer, L.J., Dai, H., van de Vijver, M.J., He, Y.D., Hart, A.A. et al. (2002). Gene expression profiling predicts clinical outcome of breast cancer,\emph{Nature}, 415, 530-536.

Kaiser S. and Leisch F. (2008). A Toolbox for Bicluster Analysis in R. \emph{Ludwigstrasse.} 33.

Csardi G., Kutalik Z., and Bergmann S.(2010). Modular analysis of gene expression data with R. \emph{Bioinformatics,} 26, 1376-7
}

\author{Mengsteab Aregay 


\email{mycs.zab@gmail.com}}
\seealso{\pkg{biclust}}

\examples{
#manupilate the biclust result
data(breastc)
fab<- fabia(breastc)
#write the bicluster result in to a text
writeBic(dset=breastc,fileName="fabiabreast.txt",
bicResult=fab, bicname="Biclust results for fabia",
mname="fabia")
}

\keyword{writeBic}