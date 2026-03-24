## =========================================================
## Sampling-design classes and methods
## =========================================================

new_inventory_design <- structure(function#Constructor for generic inventory designs
### Create a generic forest inventory design from sampled areas and
### minimum diameter thresholds. The function returns an object of class
### \code{"inventory_design"}. Expansion factors are computed as trees
### per hectare for each threshold tier. The design is shape-agnostic:
### users can describe circular, square, strip, ring, or other layouts
### through sampled area and optional metadata.
(sample_area_m2, ##<< \code{numeric}. Sampled area in square metres for
                 ##<< each tally tier.
 min_dbh_cm = 0, ##<< \code{numeric}. Minimum diameter at breast height
                 ##<< in \code{cm} required for a tree to be tallied in
                 ##<< each tier.
 name = "custom",##<< \code{character(1)}. Design name.
 metadata = NULL ##<< Optional \code{list}. Additional design metadata,
                 ##<< such as shape, radii, side length, strip width,
                 ##<< or field protocol notes.
) {

    if (!is.numeric(sample_area_m2) || !is.numeric(min_dbh_cm))
        stop("'sample_area_m2' and 'min_dbh_cm' must be numeric.")

    if (length(sample_area_m2) != length(min_dbh_cm))
        stop("'sample_area_m2' and 'min_dbh_cm' must have the same length.")

    if (length(sample_area_m2) == 0)
        stop("Design vectors cannot be empty.")

    if (any(sample_area_m2 <= 0))
        stop("'sample_area_m2' must contain positive values.")

    o <- order(min_dbh_cm)

    if (is.null(metadata))
        metadata <- list()

    structure(
        list(
            name = name,
            min_dbh_cm = min_dbh_cm[o],
            sample_area_m2 = sample_area_m2[o],
            sf = 1e4 / sample_area_m2[o],
            metadata = metadata
        ),
        class = "inventory_design"
    )

}, ex = function() {
    dsg <- new_inventory_design(
        sample_area_m2 = c(100, 400),
        min_dbh_cm = c(0, 20),
        name = "Square design",
        metadata = list(shape = "square", side_m = c(10, 20))
    )
    dsg
})

new_concentric_design <- structure(function#Constructor for concentric plot designs
### Create a concentric forest inventory design from subplot radii and
### minimum diameter thresholds. The function returns an object of class
### \code{"concentric_design"} inheriting from \code{"inventory_design"}.
### Expansion factors are computed as trees per hectare for each subplot.
(radii_m,       ##<< \code{numeric}. Subplot radii in metres.
 min_dbh_cm,    ##<< \code{numeric}. Minimum diameter at breast height
                ##<< in \code{cm} required for a tree to be tallied in each
                ##<< subplot.
 name = "custom",##<< \code{character(1)}. Design name.
 metadata = NULL ##<< Optional \code{list}. Additional design metadata.
) {

    if (!is.numeric(radii_m) || !is.numeric(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must be numeric.")

    if (length(radii_m) != length(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must have the same length.")

    if (length(radii_m) == 0)
        stop("Design vectors cannot be empty.")

    o <- order(min_dbh_cm)

    if (is.null(metadata))
        metadata <- list()

    dsg <- new_inventory_design(
        sample_area_m2 = pi * radii_m[o]^2,
        min_dbh_cm = min_dbh_cm[o],
        name = name,
        metadata = utils::modifyList(
            list(shape = "circular", radii_m = radii_m[o]),
            metadata
        )
    )

    dsg$radii_m <- radii_m[o]
    class(dsg) <- c("concentric_design", "inventory_design")
    dsg

}, ex = function() {
    dsg <- new_concentric_design(
        radii_m = c(5, 10, 15, 25),
        min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
        name = "SNFI"
    )
    dsg
})

snfi_design <- structure(function#Spanish National Forest Inventory design
### Return the default concentric subplot design used by the Spanish
### National Forest Inventory.
() {
    new_concentric_design(
        radii_m = c(5, 10, 15, 25),
        min_dbh_cm = c(7.5, 12.5, 22.5, 42.5),
        name = "SNFI"
    )
}, ex = function() {
    snfi_design()
})

print.inventory_design <- structure(function#Print a generic inventory design
### Display the main components of an \code{"inventory_design"} object:
### design name, diameter thresholds, sampled areas, and expansion
### factors.
(x,   ##<< Object of class \code{"inventory_design"}.
 ...  ##<< Further arguments passed to methods. Currently unused.
) {
    cat("Inventory design:", x$name, "\n")
    cat("Minimum DBH (cm):", paste(x$min_dbh_cm, collapse = ", "), "\n")
    cat("Sample area (m2):", paste(x$sample_area_m2, collapse = ", "), "\n")
    cat("Expansion factors:", paste(round(x$sf, 2), collapse = ", "), "\n")
    invisible(x)
})

print.concentric_design <- structure(function#Print a concentric plot design
### Display the main components of a \code{"concentric_design"} object:
### design name, subplot radii, minimum diameters, and expansion factors.
(x,   ##<< Object of class \code{"concentric_design"}.
 ...  ##<< Further arguments passed to methods. Currently unused.
) {
    cat("Concentric plot design:", x$name, "\n")
    cat("Radii (m):", paste(x$radii_m, collapse = ", "), "\n")
    cat("Minimum DBH (cm):", paste(x$min_dbh_cm, collapse = ", "), "\n")
    cat("Expansion factors:", paste(round(x$sf, 2), collapse = ", "), "\n")
    invisible(x)
})

trees_per_ha <- structure(function#Trees per hectare from a sampling design
### Generic for deriving the expansion factor, expressed as trees per
### hectare, from a sampling design and a tree diameter.
(design, ##<< Sampling design object.
 dbh_cm  ##<< \code{numeric}. Diameter at breast height in \code{cm}.
         ##<< If several values are supplied, the function uses their
         ##<< mean after removing missing values.
) {
    UseMethod("trees_per_ha")
})

trees_per_ha.inventory_design <- structure(function#Trees per hectare for generic designs
### Compute the trees-per-hectare expansion factor for a tree measured
### under a generic inventory design. The returned value depends on the
### tally tier into which the tree falls according to its diameter.
(design, ##<< Object of class \code{"inventory_design"}.
 dbh_cm  ##<< \code{numeric}. Diameter at breast height in \code{cm}.
         ##<< If several values are supplied, the function uses their
         ##<< mean after removing missing values.
) {

    dbh_cm <- as.numeric(dbh_cm)

    if (length(dbh_cm) > 1)
        dbh_cm <- mean(dbh_cm, na.rm = TRUE)

    if (all(is.na(dbh_cm)))
        return(NA_real_)

    if (is.na(dbh_cm) || dbh_cm < design$min_dbh_cm[1])
        return(NA_real_)

    idx <- findInterval(dbh_cm, design$min_dbh_cm)
    design$sf[idx]

}, ex = function() {
    trees_per_ha(new_inventory_design(c(100, 400), c(0, 20)), 13)
})

trees_per_ha.concentric_design <- structure(function#Trees per hectare for concentric designs
### Compute the trees-per-hectare expansion factor for a tree measured
### under a concentric subplot design. The returned value depends on the
### subplot into which the tree falls according to its diameter.
(design, ##<< Object of class \code{"concentric_design"}.
 dbh_cm  ##<< \code{numeric}. Diameter at breast height in \code{cm}.
         ##<< If several values are supplied, the function uses their
         ##<< mean after removing missing values.
) {

    dbh_cm <- as.numeric(dbh_cm)

    if (length(dbh_cm) > 1)
        dbh_cm <- mean(dbh_cm, na.rm = TRUE)

    if (all(is.na(dbh_cm)))
        return(NA_real_)

    if (is.na(dbh_cm) || dbh_cm < design$min_dbh_cm[1])
        return(NA_real_)

    idx <- findInterval(dbh_cm, design$min_dbh_cm)
    design$sf[idx]

}, ex = function() {
    trees_per_ha(snfi_design(), 13)
})

## =========================================================
## DBH metrics
## =========================================================

dbhMetric <- structure(function#DBH and height metrics
### Format tree diameters at breast height and tree heights into common
### inventory metrics. Depending on \code{met}, the function returns
### mean diameter, basal area, trees per hectare, or height in
### decimetres.
###
### The sampling design is used only when \code{met = "n"}. For
### \code{met = "d"}, \code{"ba"}, and \code{"h"}, the result does not
### depend on \code{design}. This behavior matches the older internal
### implementation in which the SNFI design thresholds and plot radii
### were hard-coded inside a local function used only for expansion
### factors.
(dbh,          ##<< \code{numeric}. Diameter at breast height in
               ##<< \code{mm}, or tree height in \code{m} when
               ##<< \code{met = "h"}. Vectors are averaged after
               ##<< replacing zeros with \code{NA}.
 met = "d",    ##<< \code{character(1)}. Metric to compute:
               ##<< mean diameter at breast height (\code{"d"}),
               ##<< basal area (\code{"ba"}), trees per hectare
               ##<< (\code{"n"}), or height (\code{"h"}).
 design = snfi_design()
               ##<< Sampling design object used only when
               ##<< \code{met = "n"}. The default is the Spanish
               ##<< National Forest Inventory concentric subplot design.
) {

    if (!is.numeric(dbh))
        dbh <- as.numeric(as.character(dbh))

    dbh[dbh == 0] <- NA_real_

    if (length(dbh) > 1)
        dbh <- mean(dbh, na.rm = TRUE)

    if (all(is.na(dbh)))
        return(NA_real_)

    if (!met %in% c("d", "ba", "n", "h"))
        stop("'met' must be one of 'd', 'ba', 'n', or 'h'.")

    if (met %in% "d")
        return(dbh)

    if (met %in% c("ba", "n"))
        dbh <- conv_unit(dbh, from = "mm", to = "cm")

    if (met %in% "ba")
        return(pi * dbh^2 * (4 * 1E4)^-1)

    if (met %in% "n")
        return(trees_per_ha(design = design, dbh_cm = dbh))

    if (met %in% "h")
        return(conv_unit(dbh, from = "m", to = "dm"))

}, ex = function() {
    dbhMetric(300, "ba")
    dbhMetric(130, "n")

dsg <- new_concentric_design(
    radii_m = c(4, 8, 12),
    min_dbh_cm = c(5, 15, 30),
    name = "3-subplot design"
)
trees_per_ha(dsg, 18)
dbhMetric(130, "n", design = dsg)
    
})
