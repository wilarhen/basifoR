\name{metrics2Vol}
\alias{metrics2Vol}
\title{Tree volumes in NFI data}
\description{This function computes over bark volumes (\code{dm3}) by deriving
tree metrics from NFI data and matching the metrics with volume
parameters established in 2nd NFI. To derive dendrometric
summaries use \code{\link{dendroMetrics}}.}
\usage{metrics2Vol(dbm, fc. = "freq", all.col = FALSE, ...)}
\arguments{
  \item{dbm}{\code{character} or \code{data.frame}.  URL/path to a
compressed file of the NFI (.zip) having data of either
.dbf or .mdb file extensions, or a data frame such as that
produced by \code{\link{nfiMetrics}}.}
  \item{fc.}{\code{character}. A Cubication form. Default
\code{'freq'} implements the most frequent form
matching the data.}
  \item{all.col}{\code{logical}. Maintain the columns used to
compute the volumes. Default \code{FALSE}.}
  \item{\dots}{Additional arguments in \code{\link{nfiMetrics}}}
}
\details{The volumes are computed deriving
metrics with \code{\link{nfiMetrics}} and
matching these to volume parameters
developed in second NFI. The data are
matched using two factors: the provincial
unit (\code{'pr'}) and the tree species
(\code{'Especie'}). The functions supports
parameters in two models: \code{'v ~ par1 +
par2 * (d^2) * h'}, and \code{v ~ par1 *
(d^par2) * (h^par3)}.}
\value{\code{data.frame}. Either short or expanded data, depending on the
\code{all.col} argument.  The short data contains the volumes
(\code{v}, \code{'dm3'}) plus the tree metrics defined in
\code{\link{nfiMetrics}}, see value of such a function to better
understand the metric units. The expanded data contains additional
columns used to compute the volumes.}

\author{Wilson Lara <wilarhen@gmail.com>, Felipe Bravo <fbravo@pvs.uva.es>, Cristobal Ordonez <a_cristo@pvs.uva.es>}




\examples{
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
mmad <- nfiMetrics(rmad)
vmad <- metrics2Vol(mmad)
head(vmad)
}
