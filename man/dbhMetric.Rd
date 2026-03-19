\name{dbhMetric}
\alias{dbhMetric}
\title{DBH and height metrics}
\description{Format tree diameters at breast height and tree heights into common
inventory metrics. Depending on \code{met}, the function returns
mean diameter, basal area, trees per hectare, or height in
decimetres.

The sampling \code{design} is used only when \code{met = "n"}. For
\code{met = "d"}, \code{"ba"}, and \code{"h"}, the result does not
depend on \code{design}. This behavior matches the older internal
implementation in which the SNFI \code{design} thresholds and plot radii
were hard-coded inside a local function used only for expansion
factors.}
\usage{dbhMetric(dbh, met = "d", 
    design = snfi_design())}
\arguments{
  \item{dbh}{\code{numeric}. Diameter at breast height in
<< \code{mm}, or tree height in \code{m} when
<< \code{met = "h"}. Vectors are averaged after
<< replacing zeros with \code{NA}.}
  \item{met}{\code{character(1)}. Metric to compute:
<< mean diameter at breast height (\code{"d"}),
<< basal area (\code{"ba"}), trees per hectare
<< (\code{"n"}), or height (\code{"h"}).}
  \item{design}{design }
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
    dbhMetric(300, "ba")
    dbhMetric(130, "n")

dsg <- new_concentric_design(
    radii_m = c(4, 8, 12),
    min_dbh_cm = c(5, 15, 30),
    name = "3-subplot design"
)
trees_per_ha(dsg, 18)
dbhMetric(130, "n", design = dsg)
    
}
