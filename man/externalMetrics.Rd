\name{externalMetrics}
\alias{externalMetrics}
\title{Compute tree-level metrics from external inventory data}
\description{Standardize external tree measurements into basifoR metric
units and compute requested tree-level outputs for external
inventory workflows. The function resolves diameter and height
columns from user-supplied aliases, optionally keeps grouping
columns in the output, and attaches \code{design} metadata when
expansion factors are needed downstream.}
\usage{"\\nexternalMetrics(x, var = c(\\"d\\", \\"h\\", \\"ba\\", \\"n\\", \\"Hd\\"), levels = NULL, design, colmap = list(d = c(\\"d\\", \\"dbh\\", \\"diameter\\", \\"diameter_mm\\"), h = c(\\"h\\", \\"height\\", \\"height_m\\")), d_unit = c(\\"mm\\", \\"cm\\")[1], h_unit = c(\\"m\\", \\"dm\\", \\"cm\\")[1], keep_cols = NULL, domheight_fun = get0(\\"domheight_strict\\", mode = \\"function\\", inherits = TRUE) \\\%||\\\% get0(\\"domheight\\", mode = \\"function\\", inherits = TRUE))\\n"}
\arguments{
  \item{x}{A \code{data.frame} with one row per tree or stem. The input
<< must contain at least the columns needed to resolve the
<< requested measurements through \code{colmap}.}
  \item{var}{Character vector of metrics to
<< return. Supported values are
<< \code{"d"} for diameter,
<< \code{"h"} for height,
<< \code{"ba"} for basal area,
<< \code{"n"} for trees per
<< hectare, and \code{"Hd"} for
<< dominant height. Requesting
<< \code{"Hd"} also requires
<< \code{"d"}, \code{"h"}, and
<< \code{"n"}.}
  \item{levels}{Optional character vector of grouping columns to
<< preserve in the output. When \code{"Hd"} is
<< requested, the function computes dominant height
<< within groups defined by the resolved columns in
<< \code{levels} together with \code{keep_cols}.}
  \item{design}{An object inheriting from \code{"inventory_design"}.
<< The function uses it to compute expansion factors for
<< \code{"n"} and to store \code{design} metadata in the result.}
  \item{colmap}{colmap }
  \item{d_unit}{Input unit for diameter columns named in
<< \code{colmap$d}. Returned diameter is
<< always standardized to millimetres.}
  \item{h_unit}{Input unit for height columns named
<< in \code{colmap$h}. Returned height
<< is always standardized to decimetres.}
  \item{keep_cols}{Optional character vector of additional columns to
<< carry into the output without modification. These
<< columns are also included in the grouping used for
<< \code{"Hd"} when dominant height is requested.}
  \item{domheight_fun}{domheight_fun }
}
\details{When \code{var} includes \code{"Hd"}, the function computes

dominant height after binding the requested output columns and

grouping columns. The dominant-height calculation is applied

separately within each resolved group.}
\value{Standardized output columns use these units:

\code{d} in millimetres, \code{h} in decimetres,

\code{ba} in square metres per tree, \code{n} in trees per

hectare, and \code{Hd} in decimetres. A named unit vector is

stored in \code{attr(\code{x}, "units")}. When \code{var} includes

\code{"n"} or \code{"Hd"}, the object also stores sampling

\code{design} information in \code{attr(\code{x}, "design_meta")}.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{

sq_0.1ha <- new_inventory_design(
    sample_area_m2 = 1000,
    min_dbh_cm = 7.5,
    name = "Square 0.1-ha plot",
    metadata = list(shape = "square", side_m = sqrt(1000))
)

x <- data.frame(
    plot = c("P1", "P1", "P2"),
    species = c("sp1", "sp1", "sp2"),
    diameter_mm = c(120, 185, 260),
    height_m = c(7.1, 9.4, 13.2),
    stringsAsFactors = FALSE
)

externalMetrics(
    x = x,
    var = c("d", "h", "ba", "n"),
    levels = c("plot", "species"),
    design = sq_0.1ha,
    colmap = list(d = "diameter_mm", h = "height_m"),
    d_unit = "mm",
    h_unit = "m"
)
}
