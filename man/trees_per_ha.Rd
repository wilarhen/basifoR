\name{trees_per_ha}
\alias{trees_per_ha}
\title{Trees per hectare from a sampling design}
\description{Generic for deriving the expansion factor, expressed as trees per
hectare, from a sampling \code{design} and a tree diameter.}
\usage{trees_per_ha(design, 
    dbh_cm)}
\arguments{
  \item{design}{Sampling \code{design} object.}
  \item{dbh_cm}{\code{numeric}. Diameter at breast height in \code{cm}.
<< If several values are supplied, the function uses their
<< mean after removing missing values.}
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





