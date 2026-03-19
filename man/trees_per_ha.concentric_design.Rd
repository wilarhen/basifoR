\name{trees_per_ha.concentric_design}
\alias{trees_per_ha.concentric_design}
\title{Trees per hectare for concentric designs}
\description{Compute the trees-per-hectare expansion factor for a tree measured
under a concentric subplot \code{design}. The returned value depends on the
subplot into which the tree falls according to its diameter.}
\usage{\method{trees_per_ha}{concentric_design}(design, 
    dbh_cm)}
\arguments{
  \item{design}{Object of class \code{"concentric_design"}.}
  \item{dbh_cm}{\code{numeric}. Diameter at breast height in \code{cm}.
<< If several values are supplied, the function uses their
<< mean after removing missing values.}
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
trees_per_ha(snfi_design(), 13)
}
