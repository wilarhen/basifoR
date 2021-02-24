\name{nfiMetrics}
\alias{nfiMetrics}
\title{Tree metrics from NFI data}
\description{This function recursively implements \code{\link{dbhMetric}} on
NFI data, and derives tree metrics, according to units/parameters
of the Spanish National Forest Inventory (NFI). The metric units
are described in the Value section.  Use \code{\link{metrics2Vol}}
to directly derive over bark volumes.}
\usage{nfiMetrics(nfi, var = c("pr", 
    "d", "h", "ba", "n", 
    "Hd"), levels = c("esta", 
    "espe"))}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed file of the NFI (.zip) having data of either
.dbf or .mdb file extensions, or a data frame such as that
produced by \code{\link{readNFI}}.}
  \item{var}{\code{character}. Metrics. These
can be five: \code{(1)} The
provincial unit
\code{'pr'}; \code{(2)} the
mean diameter \code{'d'};
\code{(3)} the tree height
\code{'h'}; \code{(4)} the
number of trees per hectare
\code{'n'}; \code{(5)} the
basal areas \code{'ba'};
and \code{(6)} the dominant
height \code{'Hd'}, see
Details section in
\code{\link{dbhMetric}} for
better understanding of the
metrics \code{\link{units.}} Default
\code{c('pr','d','h','ba','n','Hd')}.
imp.un = c('','cm','m','m2','','m'), ##<<\code{character}. Input
##units.
out.un = c('','cm','dm','m2','','m'), ##<<\code{character}. Output
##units.}
  \item{levels}{\code{character}. \code{levels} at which
the metrics are computed. Pattern
matching is supported. Cases are
ignored. Default
\code{c('esta','espe')} matches both the
sample plot \code{'Estadillos'} and
tree species \code{'Especie'}.}
}

\value{\code{data.frame} containing columns matching \code{levels}, plus
the variables in \code{var}: the province \code{pr},
(\code{dimensionless}), the diameter \code{d} (\code{'mm'}), the
tree height \code{h} (\code{'dm'}), the basal area
\code{ba},(\code{'m2 tree-1'}), the number of trees by hectare
\code{n}, (\code{dimensionless}), and the tree dominant height (\code{'m'}).}

\author{Wilson Lara <wilarhen@gmail.com>, Cristobal Ordonez <a_cristo@pvs.uva.es>, Felipe Bravo <fbravo@pvs.uva.es>}




\examples{
## seconf NFI
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:10,]
head(rmad,3)
}
