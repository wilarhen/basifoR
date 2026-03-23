\name{nfiMetrics}
\alias{nfiMetrics}
\title{Tree-level metrics from Spanish NFI records}
\description{Compute tree-level metrics from Spanish National Forest Inventory
records by applying \code{\link{dbhMetric}}-style logic to NFI
tables or to files that \code{\link{readNFI}} can import. \code{} The
function returns the requested metrics together with the grouping
columns selected through \code{levels}. \code{} To derive the full set of
package metrics in one call, see \code{\link{dendroMetrics}}.}
\usage{nfiMetrics(nfi, var = c("d", 
    "h", "ba", "n", "Hd"), 
    levels = c("esta", 
        "espe"), design = snfi_design(), 
    ...)}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}. Input NFI data.
Supply either \code{(1)} a path or URL to a compressed NFI
archive readable by \code{\link{readNFI}}, or \code{(2)} a
data frame already returned by \code{\link{readNFI}}.}
  \item{var}{\code{character}. Metrics to compute.
Supported values are \code{'d'}
(diameter), \code{'h'} (height),
\code{'ba'} (basal area), \code{'n'}
(trees per hectare), and \code{'Hd'}
(dominant height). \code{} \code{'Hd'}
requires \code{'h'}, \code{'d'}, and
\code{'n'} to be present in
\code{var}.}
  \item{levels}{\code{character}. Grouping columns kept in
the output. \code{} The function matches these
strings against column names returned by
\code{flev(\code{})}, supports partial matching,
and ignores case. \code{} The default usually
keeps plot- and species-level identifiers.}
  \item{design}{Sampling \code{design} used when computing
\code{'n'}. \code{} Pass a
\code{\link{snfi_design}} object, another
\code{concentric_design}, or any \code{design}
supported by \code{trees_per_ha(\code{})}.}
  \item{\dots}{Additional arguments passed to \code{\link{readNFI}} when
\code{nfi} is not already a \code{readNFI} object.}
}

\value{\code{data.frame} with the grouping columns selected through
\code{levels} plus the requested metrics in \code{var}. \code{} The output
inherits class \code{'nfiMetrics'}. \code{} Inspect
\code{attr(x, 'units')} to see the units attached to each returned
variable.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Example with bundled Toledo data
ifn4p45 <- system.file("Ifn4_Toledo.zip", package = "basifoR")

## Decompress the archive and read the first 100 records
fetch_ifn4p45 <- fetchNFI(ifn4p45)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100, ]

## Compute default metrics and inspect the reported units
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)
attr(metrics_ifn4p45, 'units')
}
