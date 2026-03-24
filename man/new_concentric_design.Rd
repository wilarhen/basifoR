\name{new_concentric_design}
\alias{new_concentric_design}
\title{Constructor for concentric plot designs}
\description{Create a concentric forest inventory design from subplot radii and
minimum diameter thresholds. The function returns an object of class
\code{"concentric_design"} inheriting from \code{"inventory_design"}.
Expansion factors are computed as trees per hectare for each subplot.}
\usage{new_concentric_design(radii_m, 
    min_dbh_cm, name = "custom", 
    metadata = NULL)}
\arguments{
  \item{radii_m}{\code{numeric}. Subplot radii in metres.}
  \item{min_dbh_cm}{\code{numeric}. Minimum diameter at breast height
<< in \code{cm} required for a tree to be tallied in each
<< subplot.}
  \item{name}{\code{character(1)}. Design \code{name}.}
  \item{metadata}{Optional \code{list}. Additional design \code{metadata}.}
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_concentric_design(
    radii_m = c(5, 10, 15, 25),
    min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
    name = "SNFI"
)
dsg
}
