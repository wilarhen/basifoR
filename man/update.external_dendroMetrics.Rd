\name{update.external_dendroMetrics}
\alias{update.external_dendroMetrics}
\title{Update a stored external dendroMetrics call
Update a stored external_dendroMetrics call}
\description{Modify the stored call in an \code{"external_dendroMetrics"} \code{object} and optionally re-evaluate it to produce an updated result.}
\usage{\method{update}{external_dendroMetrics}(object, 
    ..., evaluate = TRUE)}
\arguments{
  \item{object}{An \code{"external_dendroMetrics"} \code{object} returned by \code{\link{external_dendroMetrics}}.}
  \item{\dots}{Named arguments to replace in the stored call.}
  \item{evaluate}{Logical; if \code{TRUE}, \code{evaluate} the modified call and return the updated result. If \code{FALSE}, return the modified call without evaluating it.}
}

\value{Either an updated \code{"external_dendroMetrics"} \code{object} when \code{evaluate = TRUE}, or the modified call when \code{evaluate = FALSE}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





