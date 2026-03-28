\name{external_dendroMetrics}
\alias{external_dendroMetrics}
\title{Summarize external inventory tree data and optional volume outputs}
\description{Compute standardized tree metrics from external inventory data and, when requested,}
\usage{"\\nexternal_dendroMetrics(x, summ.vr = NULL, cut.dt = \\"d == d\\", report = FALSE, mc.cores = getOption(\\"mc.cores\\", 1L), var = c(\\"d\\", \\"h\\", \\"ba\\", \\"n\\", \\"Hd\\"), parametro = NULL, design = NULL, parameter_table = NULL, method_registry = external_volume_method_registry(), levels = NULL, metric_levels = NULL, keep_cols = NULL, metric_keep_cols = NULL, colmap = NULL, metric_colmap = list(d = c(\\"d\\", \\"dbh\\", \\"diameter\\", \\"diameter_mm\\"), h = c(\\"h\\", \\"height\\", \\"height_m\\")), d_unit = NULL, metric_d_unit = c(\\"mm\\", \\"cm\\")[1], h_unit = NULL, metric_h_unit = c(\\"m\\", \\"dm\\", \\"cm\\")[1], tree_d_unit_out = NULL, tree_h_unit_out = NULL, volume_colmap = list(d = c(\\"d\\"), h = c(\\"h\\"), dnm = c(\\"dnm\\", \\"d_nm\\", \\"D.n.m.\\"), v = c(\\"v\\"), species = c(\\"species\\", \\"spec\\", \\"especie\\"), region = c(\\"region\\", \\"pr\\"), equation_set = c(\\"equation_set\\", \\"eqset\\", \\"tariff\\", \\"model_set\\")), selector = c(\\"first\\", \\"priority\\")[1], track_provenance = FALSE, compute_metrics_if_needed = TRUE, schema = NULL, domheight_fun = get0(\\"domheight_strict\\", mode = \\"function\\", inherits = TRUE) \\\%||\\\% get0(\\"domheight\\", mode = \\"function\\", inherits = TRUE), ...)\\n"}
\arguments{
  \item{x}{A \code{data.frame}, a processed external basifoR table, or a \code{list} of such objects. Raw inputs must contain enough columns to resolve the requested metrics and, if needed, the volume selectors.}
  \item{summ.vr}{Optional grouping variable or character vector of grouping variables. Leave \code{NULL} to keep tree-level output; supply one or more column names to obtain grouped summaries.}
  \item{cut.dt}{Character expression evaluated with \code{eval(parse(\code{...}))} on the final table. Use it to filter rows after metrics or summaries have been computed.}
  \item{report}{Logical. If \code{TRUE}, write the returned table to \file{report.csv} in the working directory.}
  \item{mc.cores}{Integer number of worker processes used when \code{x} is a list. Values smaller than 1 are reset to 1.}
  \item{var}{Character vector of requested variables. Typical metric requests are \code{"d"}, \code{"h"}, \code{"ba"}, \code{"n"}, and \code{"Hd"}; volume-like names can also trigger volume processing.}
  \item{parametro}{Optional character vector of volume-method codes, for example \code{"V"}. If \code{NULL}, the function tries to infer required methods from \code{var} and \code{method_registry}.}
  \item{design}{An object inheriting from \code{"inventory_design"}. Required when the function must compute missing metrics such as \code{n} from raw external data.}
  \item{parameter_table}{Optional parameter table passed to \code{externalMetrics2Vol()} for method-specific volume coefficients or selection metadata.}
  \item{method_registry}{Named list of volume methods, usually created by \code{external_volume_method_registry()} or a custom registry following the same structure.}
  \item{levels}{Optional grouping variables forwarded to \code{schema} resolution. When \code{schema} is not used, these serve as defaults for workflow grouping metadata.}
  \item{metric_levels}{Optional grouping variables used specifically while computing missing metrics internally with \code{externalMetrics()}.}
  \item{keep_cols}{Optional character vector of source columns to preserve in the output or \code{schema} defaults.}
  \item{metric_keep_cols}{Optional character vector of columns to preserve during the internal metric-computation stage.}
  \item{colmap}{Optional named list of column aliases that update the default mappings, especially when source names differ from the expected volume or metric names.}
  \item{metric_colmap}{metric_colmap }
  \item{d_unit}{Optional diameter unit override shared with \code{schema} resolution. Accepted values are \code{"mm"} and \code{"cm"}.}
  \item{metric_d_unit}{Diameter unit expected by \code{metric_colmap} when metrics must be computed internally.}
  \item{h_unit}{Optional height unit override shared with \code{schema} resolution. Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.}
  \item{metric_h_unit}{Height unit expected by \code{metric_colmap} when metrics must be computed internally.}
  \item{tree_d_unit_out}{Optional output unit for tree-level diameter values when \code{summ.vr = NULL}. Accepted values are \code{"mm"} and \code{"cm"}.}
  \item{tree_h_unit_out}{Optional output unit for tree-level height and dominant-height values when \code{summ.vr = NULL}. Accepted values are \code{"m"}, \code{"dm"}, and \code{"cm"}.}
  \item{volume_colmap}{volume_colmap }
  \item{selector}{Character string controlling how \code{externalMetrics2Vol()} resolves multiple candidate methods or rows.}
  \item{track_provenance}{Logical. If \code{TRUE}, keep provenance columns generated by the volume workflow before the final summary stage removes them from grouped output.}
  \item{compute_metrics_if_needed}{Logical. If \code{TRUE}, compute missing metrics from raw inputs when possible; if \code{FALSE}, stop when required standardized columns are absent.}
  \item{schema}{Optional \code{"external_schema"} object created by \code{new_external_schema()}. It centralizes column aliases, units, grouping defaults, and kept columns for repeated workflows.}
  \item{domheight_fun}{domheight_fun }
  \item{\dots}{Additional arguments passed to downstream helpers, mainly custom options for internal metric or volume-processing steps.}
}
\details{This wrapper mirrors the arguments and behaviour of \code{dendroMetrics()}.

When \code{summ.vr = NULL}, the function returns tree-level records after applying \code{cut.dt}. When \code{summ.vr} contains one or more grouping variables, the function aggregates by those groups, reporting weighted means for \code{d}, \code{h}, and \code{Hd}, arithmetic sums for \code{ba}, \code{n}, and volume variables, and quadratic mean diameter \code{dg} when diameter and trees-per-hectare are available.

The function can work from already standardized inputs or from raw external tables. If requested metrics are missing and \code{compute_metrics_if_needed = TRUE}, it calls \code{externalMetrics()} internally, so raw inputs usually need a valid \code{design} plus diameter and, when relevant, height mappings. When \code{schema} is supplied, it provides reusable defaults for column aliases, units, grouping variables, and retained columns, while explicit arguments supplied in the call override those defaults.

Volume outputs are optional. They are computed only when \code{parametro} is supplied explicitly or can be inferred from \code{var} and \code{method_registry}. This lets the same entry point handle metric-only workflows, mixed metric-plus-volume workflows, and repeated processing of several input tables with optional parallel execution through \code{mc.cores}.}
\value{A \code{data.frame} with class \code{c("external_dendroMetrics", "dendroMetrics", \code{...})}. With \code{summ.vr = NULL}, the returned rows represent tree-level records, optionally converted to \code{tree_d_unit_out} and \code{tree_h_unit_out}. With \code{summ.vr} supplied, the returned rows represent grouped summaries and include standardized summary units such as \code{"cm"}, \code{"m"}, \code{"m2 ha-1"}, \code{"ha-1"}, and \code{"m3 ha-1"} when those variables are present.

The returned object stores the matched call in \code{attr(\code{x}, "call")}. When available, it also preserves \code{"units"}, \code{"design_meta"}, and \code{"volume_meta"} attributes from upstream processing, which makes the result suitable for downstream inspection and update methods.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





