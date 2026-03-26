\name{snfi_volume_method_registry}
\alias{snfi_volume_method_registry}
\title{Build the active registry of SNFI volume methods}
\description{Return the active SNFI
volume-method registry.

Builds the final registry of SNFI volume methods used by
`metrics2Vol()` by combining the defaults with optional
user-defined overrides or additions. The returned registry
controls which parameters are available and how each requested
volume output is computed.}
\usage{snfi_volume_method_registry(equations = get0("snfi_volume_equations", 
    inherits = TRUE, 
    ifnotfound = NULL))}
\arguments{
  \item{equations}{equations }
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





