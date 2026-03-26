\name{new_external_schema}
\alias{new_external_schema}
\title{Define a schema for external inventory workflows
Define a schema for external inventory workflows}
\description{Creates a reusable schema that stores column aliases, \code{units}, grouping \code{defaults}, and columns to keep across the external basifoR workflow. The schema can then be supplied to helper wrappers so repeated mappings do not need to be redefined in every call.
Create a reusable schema with column aliases, \code{units}, and default grouping settings for the external basifoR workflow.}
\usage{new_external_schema(colmap, 
    units = list(), levels = NULL, 
    keep_cols = NULL, 
    defaults = list())}
\arguments{
  \item{colmap}{colmap }
  \item{units}{units }
  \item{levels}{levels }
  \item{keep_cols}{keep_cols }
  \item{defaults}{defaults }
}

\value{An object of class \code{"external_schema"} containing normalized column mappings, unit declarations, grouping \code{defaults}, retained columns, and optional \code{defaults}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





