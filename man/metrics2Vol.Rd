\name{metrics2Vol}
\alias{metrics2Vol}
\title{Tree volumes in NFI data}
\description{This function computes over bark volumes (\code{'m3'}) processing
tree metrics from databases of the SNF data and using volume
equations established in 2nd NFI, see Details section. To compute
all in-package metrics, run function \code{\link{dendroMetrics}}.}
\usage{metrics2Vol(nfi, cub.met = "freq", 
    parametro = c("VCC"), 
    keep.var = TRUE, 
    keep.legacy = FALSE, 
    method_registry = snfi_volume_method_registry(), 
    track_provenance = FALSE, 
    ...)}
\arguments{
  \item{nfi}{Input accepted by `nfiMetrics(\code{})`, or a precomputed `"nfiMetrics"`
object with tree-level metrics.}
  \item{cub.met}{Cubication selector used when several coefficient rows match.}
  \item{parametro}{One or more volume outputs to compute. Typical values are `"V"`,
`"VCC"`, `"VSC"`, `"IAVC"`, and `"VLE"`.}
  \item{keep.var}{Keep auxiliary coefficient columns when available.}
  \item{keep.legacy}{Also return the legacy volume estimate for backward compatibility.}
  \item{method_registry}{Registry that maps each requested output to its equation function,
output column name, units, and fallback rule.}
  \item{track_provenance}{Add per-row provenance columns and a compact audit trail
attribute for computed volume outputs.}
  \item{\dots}{Passed to `nfiMetrics(\code{})` when `nfi` is not already an
`"nfiMetrics"` object.}
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{if (FALSE) {
  x <- nfiMetrics('toledo')
  y <- metrics2Vol(x, parametro = c('VCC', 'VSC'))
  head(y[c('nfi.nr', 'pr', 'especie', 'vcc', 'vsc')])
}}
