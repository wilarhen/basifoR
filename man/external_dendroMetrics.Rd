\name{external_dendroMetrics}
\alias{external_dendroMetrics}
\title{external dendroMetrics}

\usage{external_dendroMetrics(x, 
    summ.vr = NULL, cut.dt = "d == d", 
    report = FALSE, mc.cores = getOption("mc.cores", 
        1L), var = c("d", 
        "h", "ba", "n", 
        "Hd"), parametro = NULL, 
    design = NULL, parameter_table = NULL, 
    method_registry = external_volume_method_registry(), 
    levels = NULL, metric_levels = NULL, 
    keep_cols = NULL, 
    metric_keep_cols = NULL, 
    colmap = NULL, metric_colmap = list(d = c("d", 
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
    selector = c("first", 
        "priority")[1], 
    track_provenance = FALSE, 
    compute_metrics_if_needed = TRUE, 
    schema = NULL, ...)}
\arguments{
  \item{x}{x }
  \item{summ.vr}{summ.vr }
  \item{cut.dt}{cut.dt }
  \item{report}{report }
  \item{mc.cores}{mc.cores }
  \item{var}{var }
  \item{parametro}{parametro }
  \item{design}{design }
  \item{parameter_table}{parameter_table }
  \item{method_registry}{method_registry }
  \item{levels}{levels }
  \item{metric_levels}{metric_levels }
  \item{keep_cols}{keep_cols }
  \item{metric_keep_cols}{metric_keep_cols }
  \item{colmap}{colmap }
  \item{metric_colmap}{metric_colmap }
  \item{d_unit}{d_unit }
  \item{metric_d_unit}{metric_d_unit }
  \item{h_unit}{h_unit }
  \item{metric_h_unit}{metric_h_unit }
  \item{volume_colmap}{volume_colmap }
  \item{selector}{selector }
  \item{track_provenance}{track_provenance }
  \item{compute_metrics_if_needed}{compute_metrics_if_needed }
  \item{schema}{schema }
  \item{\dots}{\dots }
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





