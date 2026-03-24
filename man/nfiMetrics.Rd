\name{nfiMetrics}
\alias{nfiMetrics}
<<<<<<< HEAD
\title{Tree metrics from NFI data}
\description{This function recursively implements \code{\link{dbhMetric}} on
data bases of the Spanish National Forest Inventory (NFI) to
derive a variety of tree metrics. Use \code{\link{metrics2Vol}} to
recursively derive the metrics plus over bark volumes.}
=======
\title{Tree-level metrics from Spanish NFI records}
\description{Compute tree-level metrics from Spanish National Forest Inventory
records by applying \code{\link{dbhMetric}}-style logic to NFI
tables or to files that \code{\link{readNFI}} can import.  The
function returns the requested metrics together with the grouping
columns selected through \code{levels}.  To derive the full set of
package metrics in one call, see \code{\link{dendroMetrics}}.}
>>>>>>> basifoR_0.7.1
\usage{nfiMetrics(nfi, var = c("d", 
    "h", "ba", "n", "Hd"), 
    levels = c("esta", 
        "espe"), design = snfi_design(), 
    ...)}
\arguments{
<<<<<<< HEAD
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed file of the NFI (.zip) having data of either
.dbf or .mdb file extensions, or a data frame such as that
produced by \code{\link{readNFI}}.}
  \item{var}{\code{character}. Metrics. These
can be five: \code{(1)} the
mean diameter \code{'d'};
\code{(2)} the tree height
\code{'h'}; \code{(3)} the
basal area \code{'ba'};
\code{(4)} the number of
trees per hectare
\code{'n'}; and \code{(5)}
the dominant height
\code{'Hd'}, see Details
section in
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
=======
  \item{nfi}{\code{character} or \code{data.frame}. Input NFI data.
Supply either \code{(1)} a path or URL to a compressed NFI
archive readable by \code{\link{readNFI}}, or \code{(2)} a
data frame already returned by \code{\link{readNFI}}.}
  \item{var}{\code{character}. Metrics to compute.
Supported values are \code{'d'}
(diameter), \code{'h'} (height),
\code{'ba'} (basal area), \code{'n'}
(trees per hectare), and \code{'Hd'}
(dominant height).  \code{'Hd'}
requires \code{'h'}, \code{'d'}, and
\code{'n'} to be present in
\code{var}.}
  \item{levels}{\code{character}. Grouping columns
kept in the output.  The function
supports partial matching, and
ignores case.  The default usually
keeps plot- and species-level
identifiers.}
  \item{design}{Sampling \code{design} used when computing
\code{'n'}.  Pass a
\code{\link{snfi_design}} object, another
\code{concentric_design}, or any \code{design}
supported by \code{trees_per_ha()}.}
  \item{\dots}{Additional arguments passed to \code{\link{readNFI}} when
\code{nfi} is not already a \code{readNFI} object.}
>>>>>>> basifoR_0.7.1
}

\value{\code{data.frame} with the grouping columns selected through
\code{levels} plus the requested metrics in \code{var}.  The output
inherits class \code{'nfiMetrics'}.  Inspect
\code{attr(x, 'units')} to see the units attached to each returned
variable.}

<<<<<<< HEAD
\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}
=======
\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}
>>>>>>> basifoR_0.7.1




\examples{
<<<<<<< HEAD
## seconf NFI
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:10,]
mmad <- nfiMetrics(rmad)
head(rmad,3)
## see metric units
attr(rmad,'units')
=======
## Example with bundled Toledo data
ifn4p45 <- system.file("Ifn4_Toledo.zip", package = "basifoR")

## Decompress the archive and read the first 100 records
fetch_ifn4p45 <- fetchNFI(ifn4p45)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100, ]

## Compute default metrics and inspect the reported units
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)
attr(metrics_ifn4p45, 'units')
>>>>>>> basifoR_0.7.1
}
