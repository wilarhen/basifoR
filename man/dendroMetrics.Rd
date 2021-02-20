\name{dendroMetrics}
\alias{dendroMetrics}
\title{Summarize dendrometrics}
\description{This function can control most of the in-package routines. The
function computes and summarizes dendrometrics using data of the
Spanish National Forest Inventory (NFI). This also transforms the
metric units of the summaries, see details.}
\usage{dendroMetrics(mmd, summ.vr = "Estadillo", 
    cut.dt = "d == d", 
    report = FALSE, ...)}
\arguments{
  \item{mmd}{\code{character} or \code{data.frame}.  URL/path to a
compressed file of the NFI (.zip) having data of either
.dbf or .mdb file extensions; or data frame such as that
produced by \code{\link{nfiMetrics}}; or data frame such
as that produced by \code{\link{readNFI}}.}
  \item{summ.vr}{\code{character} or \code{NULL}. Name
of a Column used to summarize the
outputs. If \code{NULL} then output from
\code{\link{metrics2Vol}} is
returned. Default \code{'Estadillo'}}
  \item{cut.dt}{\code{character}. Logical condition used
to subset the output. Default \code{'d == d'}
avoids subsetting.}
  \item{report}{\code{logical}. Print a \code{report} of the output
in the current working directory.}
  \item{\dots}{Additional arguments in \code{\link{metrics2Vol}} or
\code{\link{nfiMetrics}} or \code{\link{readNFI}}.}
}
\details{Units of measurement of the
derived dendrometrics are transformed to
stand units and the data is summarized
using levels in
\code{summ.vr}. Consequently the metric
units of the summarized variables do not
correspond to the metric units of data
from \code{\link{metrics2Vol}}.  The
summaries contain following variables: 1)
three factor columns: the province
(\code{pr}), the tree species
(\code{'ESPECIE'} or \code{'Especie'},
depending on whether the data belongs to
2nd or 3rd NFIs), the sample units
(\code{'ESTADILLO'} or
\code{'Estadillo'}); 2) the tree basal
area (\code{ba}, \code{'m2 ha-1'}); 3)
the average diameter (\code{d},
\code{'cm'}); 4) the quadratic mean
diameter (\code{dg}, \code{'cm'}); 5) The
average tree height (\code{h},
\code{'m'}); 6) the number of trees by
hectare (\code{n}, dimensionless), and
the over bark volume (\code{v}, \code{'m3
ha-1'}). Subsets of the summaries are
computed using logical expressions in
\code{cut.dt}, see syntax in
\code{\link{Logic}}.}
\value{\code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
\code{\link{metrics2Vol}}, or a summary of the variables, see
Details section.}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## Local NFI Data from the province of Madrid
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
mmad <- nfiMetrics(rmad)
vmad <- metrics2Vol(mmad)
dmad <- dendroMetrics(vmad, cut.dt = 'h > 8')
head(dmad)


}
