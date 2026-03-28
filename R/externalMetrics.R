externalMetrics <- structure(function # Compute tree-level metrics from external inventory data
### Standardize external tree measurements into basifoR metric units and
### compute requested tree-level outputs for external inventory workflows.
### The function resolves diameter and height columns from user-supplied
### aliases, optionally preserves grouping columns, and attaches design
### metadata when expansion factors are needed downstream.
(
    x,
### A \code{data.frame} with one row per tree or stem. The input must
### contain the columns needed to resolve the requested measurements
### through \code{colmap}.
    var = c("d", "h", "ba", "n", "Hd"),
### Character vector of metrics to return. Supported values are
### \code{"d"} for diameter, \code{"h"} for height, \code{"ba"}
### for basal area, \code{"n"} for trees per hectare, and \code{"Hd"}
### for dominant height. Requesting \code{"Hd"} also requires
### \code{"d"}, \code{"h"}, and \code{"n"}.
    levels = NULL,
### Optional character vector of grouping columns to preserve in the
### output. When \code{"Hd"} is requested, the function computes dominant
### height within groups defined by the resolved columns in
### \code{levels} together with \code{keep_cols}.
    design,
### An object inheriting from \code{"inventory_design"}. The function
### uses it to compute expansion factors for \code{"n"} and to store
### design metadata in the result.
    colmap = list(
        d = c("d", "dbh", "diameter", "diameter_mm"),
        h = c("h", "height", "height_m")
    ),
### Named list of candidate input column names. Element \code{d} lists
### aliases for diameter columns and element \code{h} lists aliases for
### height columns. The function resolves names case-insensitively and
### also matches numbered suffixes such as \code{diameter_1} and
### \code{diameter_2}.
    d_unit = c("mm", "cm")[1],
### Input unit for diameter columns named in \code{colmap$d}. Returned
### diameter is always standardized to millimetres.
    h_unit = c("m", "dm", "cm")[1],
### Input unit for height columns named in \code{colmap$h}. Returned
### height is always standardized to decimetres.
    keep_cols = NULL,
### Optional character vector of additional columns to carry into the
### output without modification. These columns are also included in the
### grouping used for \code{"Hd"} when dominant height is requested.
    domheight_fun = get0("domheight_strict", mode = "function", inherits = TRUE) %||%
        get0("domheight", mode = "function", inherits = TRUE)
### Function used to compute dominant height from \code{h}, \code{d},
### and \code{n}. This is only required when \code{"Hd"} is requested.
) {
    ##details<<
    ##details<< The function first resolves measurement columns from
    ##details<< \code{colmap}. Exact matches are preferred, then
    ##details<< case-insensitive pattern matches with optional numeric
    ##details<< suffixes are considered. If several repeated measurement
    ##details<< columns are found for the same variable, the function
    ##details<< averages non-missing values row-wise.
    ##details<<
    ##details<< Diameter is returned in millimetres, height in decimetres,
    ##details<< basal area in square metres per tree, dominant height in
    ##details<< decimetres, and \code{n} as trees per hectare. Zero values
    ##details<< in resolved diameter or height columns are treated as
    ##details<< missing before aggregation.
    ##details<<
    ##details<< When \code{var} includes \code{"n"}, the function uses
    ##details<< \code{design} to obtain tree expansion factors. For fixed-area
    ##details<< designs it calls \code{trees_per_ha()}, while for concentric
    ##details<< designs it selects the proper expansion factor according to
    ##details<< \code{design$min_dbh_cm}. The returned object stores these
    ##details<< settings in \code{attr(x, "design_meta")}.
    ##details<<
    ##details<< When \code{var} includes \code{"Hd"}, the function computes
    ##details<< dominant height after binding the requested output columns and
    ##details<< grouping columns. The dominant-height calculation is applied
    ##details<< separately within each resolved group.

    x0 <- x
    if (is.null(x0))
        return(x)

    if (!is.data.frame(x))
        stop("'x' must be a data.frame.", call. = FALSE)

    if (missing(design) || is.null(design))
        stop("'design' must be supplied.", call. = FALSE)

    if (!inherits(design, "inventory_design"))
        stop("'design' must inherit from 'inventory_design'.", call. = FALSE)

    d_unit <- match.arg(d_unit, c("mm", "cm"))
    h_unit <- match.arg(h_unit, c("m", "dm", "cm"))

    resolve_measure_cols <- function(dt, aliases) {
        if (is.null(aliases) || !length(aliases))
            return(character(0))

        nm0 <- names(dt)
        nml <- tolower(nm0)
        ali <- tolower(aliases)

        ii <- match(ali, nml)
        ii <- ii[!is.na(ii)]
        if (length(ii) > 0L)
            return(nm0[ii[1L]])

        hits <- integer(0)
        for (a in ali) {
            rx <- paste0("^", a, "([._]?[0-9]+)?$")
            hits <- c(hits, grep(rx, nml, perl = TRUE))
        }
        hits <- unique(hits)

        if (!length(hits))
            return(character(0))

        cols <- nm0[hits]
        base <- sub("([._]?[0-9]+)$", "", tolower(cols))

        if (length(unique(base)) > 1L) {
            warning(
                "Ambiguous measurement columns matched: ",
                paste(cols, collapse = ", "),
                ". Using ", cols[1L],
                call. = FALSE
            )
            return(cols[1L])
        }

        cols[order(cols)]
    }

    resolve_group_cols <- function(dt, cols) {
        if (is.null(cols) || !length(cols))
            return(character(0))

        nm0 <- names(dt)
        nml <- tolower(nm0)
        out <- character(0)

        for (cl in cols) {
            i <- match(tolower(cl), nml)

            if (!is.na(i)) {
                out <- c(out, nm0[i])
                next
            }

            j <- grep(tolower(cl), nml, fixed = TRUE)
            if (length(j) == 1L) {
                out <- c(out, nm0[j])
            } else if (length(j) > 1L) {
                warning(
                    "Ambiguous grouping column '", cl,
                    "'. Using ", nm0[j[1L]],
                    call. = FALSE
                )
                out <- c(out, nm0[j[1L]])
            }
        }

        unique(out)
    }

    get_numeric_matrix <- function(dt, cols) {
        if (!length(cols))
            return(NULL)

        y <- dt[, cols, drop = FALSE]
        y <- lapply(y, function(z) as.numeric(as.character(z)))
        y <- as.data.frame(y, check.names = FALSE, stringsAsFactors = FALSE)
        as.matrix(y)
    }

    convert_d_to_mm <- function(z, unit) {
        if (unit == "mm") return(z)
        if (unit == "cm") return(z * 10)
        stop("Unsupported diameter unit: ", unit, call. = FALSE)
    }

    convert_h_to_dm <- function(z, unit) {
        if (unit == "m")  return(z * 10)
        if (unit == "dm") return(z)
        if (unit == "cm") return(z / 10)
        stop("Unsupported height unit: ", unit, call. = FALSE)
    }

    mm_to_cm <- function(z) z / 10

    trees_per_ha_vec <- function(dbh_cm, design) {
        if (inherits(design, "concentric_design")) {
            out <- rep(NA_real_, length(dbh_cm))
            ok <- !is.na(dbh_cm) & dbh_cm >= design$min_dbh_cm[1]
            if (any(ok)) {
                idx <- findInterval(dbh_cm[ok], design$min_dbh_cm)
                out[ok] <- design$sf[idx]
            }
            return(out)
        }

        vapply(
            dbh_cm,
            function(v) trees_per_ha(design = design, dbh_cm = v),
            numeric(1)
        )
    }

    var0 <- unique(var)
    if ("Hd" %in% var0 && !all(c("d", "h", "n") %in% var0))
        stop("Hd requires var to include 'd', 'h', and 'n'.", call. = FALSE)

    var_base <- setdiff(var0, "Hd")

    diam_cols <- character(0)
    ht_cols   <- character(0)

    if (any(var_base %in% c("d", "ba", "n")))
        diam_cols <- resolve_measure_cols(x, colmap$d %||% character(0))

    if (any(var_base %in% c("h")))
        ht_cols <- resolve_measure_cols(x, colmap$h %||% character(0))

    if (any(var_base %in% c("d", "ba", "n")) && !length(diam_cols))
        stop("Could not resolve diameter column(s). Check 'colmap$d'.",
             call. = FALSE)

    if (any(var_base %in% c("h")) && !length(ht_cols))
        stop("Could not resolve height column(s). Check 'colmap$h'.",
             call. = FALSE)

    diam_mm <- NULL
    diam_cm <- NULL
    ht_dm   <- NULL
    trees_ha <- NULL

    if (length(diam_cols)) {
        diam_mat <- get_numeric_matrix(x, diam_cols)
        diam_mat[diam_mat == 0] <- NA_real_

        if (ncol(diam_mat) == 1L) {
            diam_raw <- diam_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(diam_mat))
            diam_raw <- rowMeans(diam_mat, na.rm = TRUE)
            diam_raw[nn == 0L] <- NA_real_
        }

        diam_mm <- convert_d_to_mm(diam_raw, d_unit)
        diam_cm <- mm_to_cm(diam_mm)

        if ("n" %in% var_base)
            trees_ha <- trees_per_ha_vec(diam_cm, design)
    }

    if (length(ht_cols)) {
        ht_mat <- get_numeric_matrix(x, ht_cols)
        ht_mat[ht_mat == 0] <- NA_real_

        if (ncol(ht_mat) == 1L) {
            ht_raw <- ht_mat[, 1L]
        } else {
            nn <- rowSums(!is.na(ht_mat))
            ht_raw <- rowMeans(ht_mat, na.rm = TRUE)
            ht_raw[nn == 0L] <- NA_real_
        }

        ht_dm <- convert_h_to_dm(ht_raw, h_unit)
    }

    out_metrics <- list()

    for (met in var_base) {
        if (met == "d")
            out_metrics[[met]] <- diam_mm

        if (met == "h")
            out_metrics[[met]] <- ht_dm

        if (met == "ba")
            out_metrics[[met]] <- pi * diam_cm^2 / 40000

        if (met == "n")
            out_metrics[[met]] <- trees_ha
    }

    out <- data.frame(out_metrics, check.names = FALSE, stringsAsFactors = FALSE)

    group_cols <- resolve_group_cols(x, unique(c(levels, keep_cols)))
    if (length(group_cols)) {
        out <- data.frame(
            x[, group_cols, drop = FALSE],
            out,
            check.names = FALSE,
            stringsAsFactors = FALSE
        )
    }

    if ("Hd" %in% var0) {
        if (is.null(domheight_fun))
            stop("Hd requested but no dominant-height function is available.",
                 call. = FALSE)

        grp <- if (length(group_cols)) {
            interaction(x[, group_cols, drop = FALSE], drop = TRUE, lex.order = TRUE)
        } else {
            factor(rep("all", nrow(x)))
        }

        spl <- split(out, grp, drop = TRUE)

        spl <- lapply(spl, function(y) {
            y$Hd <- tryCatch(
                domheight_fun(y$h, y$d, y$n),
                error = function(e) NA_real_
            )
            y
        })

        out <- do.call(rbind, spl)
        rownames(out) <- NULL
    }

    metric_units <- c(
        d  = "mm",
        h  = "dm",
        ba = "m2",
        n  = "",
        Hd = "dm"
    )
    attr(out, "units") <- metric_units[intersect(names(out), names(metric_units))]

    if (any(var0 %in% c("n", "Hd"))) {
        attr(out, "design_meta") <- list(
            name = design$name %||% NA_character_,
            class = class(design),
            min_dbh_cm = design$min_dbh_cm %||% NA_real_,
            sample_area_m2 = design$sample_area_m2 %||% NA_real_,
            expansion_factor = design$sf %||% NA_real_,
            metadata = design$metadata %||% list(),
            used_for = "n",
            returned_unit = "trees/ha"
        )
    }

    class(out) <- unique(c("externalMetrics", "nfiMetrics", class(out)))
    out
### A \code{data.frame} containing the requested tree-level metrics,
### optionally preceded by resolved grouping columns. The returned
### object inherits from \code{"externalMetrics"} and
### \code{"nfiMetrics"}.
###
### Standardized output columns use these units: \code{d} in
### millimetres, \code{h} in decimetres, \code{ba} in square metres
### per tree, \code{n} in trees per hectare, and \code{Hd} in
### decimetres. A named unit vector is stored in
### \code{attr(x, "units")}. When \code{var} includes \code{"n"}
### or \code{"Hd"}, the object also stores sampling design
### information in \code{attr(x, "design_meta")}.
}, ex = function() {

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
})
