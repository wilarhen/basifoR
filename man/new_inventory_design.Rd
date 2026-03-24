\name{new_inventory_design}
\alias{new_inventory_design}
\title{Constructor for generic inventory designs}
\description{Create a generic forest inventory design from sampled areas and
minimum diameter thresholds. The function returns an object of class
\code{"inventory_design"}. Expansion factors are computed as trees
per hectare for each threshold tier. The design is shape-agnostic:
users can describe circular, square, strip, ring, or other layouts
through sampled area and optional \code{metadata}.}
\usage{new_inventory_design(sample_area_m2, 
    min_dbh_cm = 0, name = "custom", 
    metadata = NULL)}
\arguments{
  \item{sample_area_m2}{\code{numeric}. Sampled area in square metres for
<< each tally tier.}
  \item{min_dbh_cm}{\code{numeric}. Minimum diameter at breast height
<< in \code{cm} required for a tree to be tallied in
<< each tier.}
  \item{name}{\code{character(1)}. Design \code{name}.}
  \item{metadata}{Optional \code{list}. Additional design \code{metadata},
<< such as shape, radii, side length, strip width,
<< or field protocol notes.}
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
dsg <- new_inventory_design(
    sample_area_m2 = c(100, 400),
    min_dbh_cm = c(0, 20),
    name = "Square design",
    metadata = list(shape = "square", side_m = c(10, 20))
)
dsg
}
