\name{externalMetrics2Vol}
\alias{externalMetrics2Vol}
\title{Compute tree-level volume outputs from external inventory data}
\description{Compute one or more tree-level volume outputs from external inventory data or from a precomputed \code{"externalMetrics"} object. The function resolves the required inputs for the selected methods, standardizes units, optionally derives missing tree metrics with \code{externalMetrics()}, evaluates registry-based volume methods, converts raw results to cubic metres, and can retain provenance metadata for each returned output.}
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
    ...)}
\arguments{
  \item{x}{A \code{data.frame} with external tree records, or a precomputed \code{"externalMetrics"} object. Standardized input columns such as \code{d}, \code{h}, \code{dnm}, or \code{v} must carry named unit metadata in \code{attr(\code{x}, "units")}.}
  \item{parametro}{Character vector of requested volume outputs. Values must match method names in \code{method_registry}; with the default registry these are typically \code{"V"}, \code{"VCC"}, and \code{"VSC"}.}
  \item{parameter_table}{Optional \code{data.frame} of coefficients or parameter rows used by the selected methods. When present, rows can be filtered by \code{parameter}, \code{parametro}, or \code{method}, and then matched by the fields declared in each method definition.}
  \item{method_registry}{Named list of method definitions, usually created with \code{external_volume_method_registry()} and \code{new_volume_method()}. Each entry defines the output column, raw units, conversion to \code{m3}, matching rules, and fallback behaviour.}
  \item{colmap}{Optional named list of column aliases used while resolving contextual inputs for the volume methods. If \code{NULL}, \code{volume_colmap} is used. Aliases for \code{d} and \code{h} also update \code{metric_colmap}.}
  \item{selector}{Rule used to choose one parameter row when several rows remain after filtering. \code{"first"} keeps the first surviving row; \code{"priority"} uses the highest numeric \code{priority} value when that column exists.}
  \item{track_provenance}{Logical; if \code{TRUE}, append per-output provenance columns such as \code{_source}, \code{_status}, \code{_raw_unit}, \code{_scale}, and \code{_model}, and store a \code{volume_meta} attribute.}
  \item{compute_metrics_if_needed}{Logical; if \code{TRUE}, call \code{externalMetrics()} when required standardized inputs are missing but can be derived from raw measurements.}
  \item{design}{Inventory \code{design} object passed to \code{externalMetrics()} when automatic metric derivation is needed. Supply an object inheriting from \code{"inventory_design"} when \code{x} does not already contain the standardized inputs required by the requested methods.}
  \item{var}{Legacy alias for \code{metric_var}. Use it only for backward compatibility.}
  \item{metric_var}{Character vector of metric variables to request from \code{externalMetrics()} when automatic metric derivation is needed. If \code{NULL}, the function derives the required metrics from \code{parametro} and from the legacy \code{var} argument.}
  \item{levels}{Legacy alias for \code{metric_levels}. Use it only for backward compatibility.}
  \item{metric_levels}{Character vector of grouping or identifier columns to preserve when \code{externalMetrics()} is called internally.}
  \item{keep_cols}{Legacy alias for \code{metric_keep_cols}. Use it only for backward compatibility.}
  \item{metric_keep_cols}{Extra columns to keep when \code{externalMetrics()} is called internally to standardize or derive missing metrics.}
  \item{metric_colmap}{metric_colmap }
  \item{d_unit}{Legacy alias for \code{metric_d_unit}. Use it only for backward compatibility.}
  \item{metric_d_unit}{Measurement unit of the raw diameter columns used by \code{externalMetrics()} when metrics must be derived. Accepted values are \code{"mm"} and \code{"cm"}.}
  \item{h_unit}{Legacy alias for \code{metric_h_unit}. Use it only for backward compatibility.}
  \item{metric_h_unit}{Measurement unit of the raw height columns used by \code{externalMetrics()} when metrics must be derived. Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.}
  \item{volume_colmap}{volume_colmap }
  \item{\dots}{Additional arguments passed only to \code{externalMetrics()} when automatic metric derivation is triggered. At present this is mainly useful for forwarding \code{domheight_fun}.}
}
\details{Each requested output is evaluated with the method definition stored in \code{method_registry}. A method can resolve its own parameter rows with \code{get_pars}, use embedded parameters in \code{pars}, or fall back to the shared \code{parameter_table}. Raw outputs are converted to cubic metres with \code{scale_to_m3}. When \code{track_provenance = TRUE}, the result stores both per-row provenance columns and a \code{volume_meta} audit attribute.}
\value{A \code{data.frame} containing the original columns in \code{x} plus the requested volume outputs. The result inherits from \code{"externalMetrics2Vol"} and \code{"metrics2vol"}, preserves the surviving named unit metadata in \code{attr(\code{x}, "units")}, and may also include \code{design_meta} and \code{volume_meta} attributes. When \code{track_provenance = TRUE}, extra columns describing the source, status, raw unit, scale factor, and selected model are added for each returned output.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{
x <- data.frame(
    species = c("sp1", "sp2"),
    d = c(120, 260),
    h = c(7.1, 13.2)
)
attr(x, "units") <- c(d = "mm", h = "m")

pars <- data.frame(
    parameter = "V",
    species = c("sp1", "sp2"),
    a = c(0.00002, 0.00003),
    b = c(2.30, 2.10),
    model = c("demo_sp1", "demo_sp2")
)

ext_methods <- external_volume_method_registry(list(
    V = new_volume_method(
        output = "v",
        fun = function(dbh_mm, h_m, pars) {
            dbh_cm <- dbh_mm / 10
            pars$a + pars$b * (dbh_cm^2) * h_m
        },
        raw_unit = "cm3",
        unit = "m3",
        scale_to_m3 = 1 / 1e6,
        build_args = function(ctx, pars, resolved) {
            list(dbh_mm = ctx$d_mm, h_m = ctx$h_m, pars = pars)
        },
        fallback = function(ctx, pars, resolved) NA_real_,
        match_by = "species",
        required_inputs = c("d", "h")
    )
))

out <- externalMetrics2Vol(
    x,
    parametro = "V",
    parameter_table = pars,
    method_registry = ext_methods,
    track_provenance = TRUE
)

out[, c("species", "v", "v_source", "v_status")]
}
