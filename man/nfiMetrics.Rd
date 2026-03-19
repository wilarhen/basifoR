\name{nfiMetrics}
\alias{nfiMetrics}
\title{Tree metrics from NFI data}
\description{This function recursively implements \code{\link{dbhMetric}} on
data bases of the Spanish National Forest Inventory (NFI) to
derive a variety of tree metrics.  To compute all in-package
metrics, run function \code{\link{dendroMetrics}}.}
\usage{nfiMetrics(nfi, var = c("d", 
    "h", "ba", "n", "Hd"), 
    levels = c("esta", 
        "espe"), ...)}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed file of the NFI (.zip) having data of either
.dbf or .mdb file extensions, or a data frame such as that
produced by \code{\link{readNFI}}.}
  \item{var}{\code{character}. Metrics. These
can be five: \code{(1)} the mean
diameter \code{'d'}; \code{(2)}
the tree height \code{'h'};
\code{(3)} the basal area
\code{'ba'}; \code{(4)} the
number of trees per hectare
\code{'n'}; and \code{(5)} the
dominant height \code{'Hd'}, see
Details section in
\code{\link{dbhMetric}} for
better understanding of the
metrics units. Default
\code{c('pr','d','h','ba','n','Hd')}.}
  \item{levels}{\code{character}. \code{levels} at which
the metrics are computed. Pattern
matching is supported. Cases are
ignored. Default
\code{c('esta','espe')} matches both
the sample plot \code{'Estadillos'}
and tree species \code{'Especie'}.,}
  \item{\dots}{Additional arguments in \code{\link{readNFI}}.}
}

\value{\code{data.frame} containing columns which match the strings in
\code{levels}, plus the variables defined in \code{var}, including
the province \code{pr} (\code{dimensionless}), the diameter
\code{d} (\code{'mm'}), the tree height \code{h} (\code{'dm'}),
the basal area \code{ba} (\code{'m2 tree-1'}), the number of trees
by hectare \code{n} (\code{dimensionless}), and the tree dominant
height \code{Hd} (\code{'m'}).}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)

# Read and process the data (first 100 rows)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100,]

# Compute some metrics
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)

# see metric units
    attr(metrics_ifn4p45,'units')
}
