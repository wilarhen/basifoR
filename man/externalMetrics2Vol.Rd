\name{externalMetrics2Vol}
\alias{externalMetrics2Vol}
\title{externalMetrics2Vol}

\usage{externalMetrics2Vol(x, 
    parametro = c("V"), 
    parameter_table = NULL, 
    method_registry = external_volume_method_registry(), 
    colmap = NULL, selector = c("first", 
        "priority")[1], 
    track_provenance = FALSE, 
    compute_metrics_if_needed = TRUE, 
    design = NULL, var = NULL, 
    metric_var = NULL, 
    levels = NULL, metric_levels = NULL, 
    keep_cols = NULL, 
    metric_keep_cols = NULL, 
    metric_colmap = list(d = c("d", 
        "dbh", "diameter", 
        "diameter_mm"), 
        h = c("h", "height", 
            "height_m")), 
    d_unit = NULL, metric_d_unit = c("mm", 
        "cm")[1], h_unit = NULL, 
    metric_h_unit = c("m", 
        "dm", "cm")[1], 
    volume_colmap = list(d = c("d"), 
        h = c("h"), dnm = c("dnm", 
            "d_nm", "D.n.m."), 
        v = c("v"), species = c("species", 
            "spec", "especie"), 
        region = c("region", 
            "pr"), equation_set = c("equation_set", 
            "eqset", 
            "tariff", 
            "model_set")), 
    schema = NULL, ...)}
\arguments{
  \item{x}{x }
  \item{parametro}{parametro }
  \item{parameter_table}{parameter_table }
  \item{method_registry}{method_registry }
  \item{colmap}{colmap }
  \item{selector}{selector }
  \item{track_provenance}{track_provenance }
  \item{compute_metrics_if_needed}{compute_metrics_if_needed }
  \item{design}{design }
  \item{var}{var }
  \item{metric_var}{metric_var }
  \item{levels}{levels }
  \item{metric_levels}{metric_levels }
  \item{keep_cols}{keep_cols }
  \item{metric_keep_cols}{metric_keep_cols }
  \item{metric_colmap}{metric_colmap }
  \item{d_unit}{d_unit }
  \item{metric_d_unit}{metric_d_unit }
  \item{h_unit}{h_unit }
  \item{metric_h_unit}{metric_h_unit }
  \item{volume_colmap}{volume_colmap }
  \item{schema}{schema }
  \item{\dots}{\dots }
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





