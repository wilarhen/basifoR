<<<<<<< HEAD
dbhMetric <- structure(function#DBH metrics
###This function can format tree diameters at breast height and tree
###heights according to the sampling design of the Spanish National
###Forest Inventory (SNFI). The function is used by other routines of
###\code{basifoR} to derive tree metrics, see Details
###section. Implementation of this function using data sets of the
###SNFI can be burdensome. Use \code{\link{dendroMetrics}} instead to
###recursively derive tree metrics.
                       ##details<< Replicates of tree diameter
                       ##\code{'d'} are averaged. The tree heights
                       ##\code{'h'} are formatted from \code{mm} to
                       ##\code{dm} for further evaluation of volume
                       ##equations. The basal areas are computed
                       ##transforming the diameters from \code{mm} to
                       ##\code{cm} and using the formula: \code{ba (m2
                       ##tree-1 ha-1) = pi * d(cm)^2 * (4 *
                       ##1E4)^-1}. The number of trees per hectare
                       ##\code{'n'} are calculated considering the
                       ##sample design of the NFI: each plot consists
                       ##of four concentric subplots with radii
                       ##\code{5, 10, 15,} and \code{25 m}. The
                       ##minimum diameters recorded in the subplots
                       ##are \code{7.5, 12.5, 22.5,} and \code{42.5
                       ##cm} respectively. Considering these, any of
                       ##four estimates is printed: \code{127.32,
                       ##31.83, 14.15}, or \code{5.09}.
(
    dbh,  ##<<\code{numeric}. Either diameters at breast height
          ##(\code{mm}) or tree heights (\code{m}). Vectors are
          ##averaged. Zero values are formatted to \code{NA}.
    met = 'd' ##<<\code{character}. Any of five metrics: mean diameter
              ##at breast height (\code{'d'}), basal area
              ##(\code{'ba'}), number of trees (\code{'n'}), or tree
              ##height (\code{'h'}). Default \code{'d'}.
    
=======
## =========================================================
## Sampling-design classes and methods
## =========================================================

new_concentric_design <- structure(function#Constructor for concentric plot designs
### Create a concentric forest inventory design from subplot radii and
### minimum diameter thresholds. The function returns an object of class
### \code{"concentric_design"} inheriting from \code{"inventory_design"}.
### Expansion factors are computed as trees per hectare for each subplot.
(radii_m,       ##<< \code{numeric}. Subplot radii in metres.
 min_dbh_cm,    ##<< \code{numeric}. Minimum diameter at breast height
                ##<< in \code{cm} required for a tree to be tallied in each
                ##<< subplot.
 name = "custom"##<< \code{character(1)}. Design name.
>>>>>>> basifoR_0.7.1
) {

    if (!is.numeric(radii_m) || !is.numeric(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must be numeric.")

    if (length(radii_m) != length(min_dbh_cm))
        stop("'radii_m' and 'min_dbh_cm' must have the same length.")

    if (length(radii_m) == 0)
        stop("Design vectors cannot be empty.")

    o <- order(min_dbh_cm)

    structure(
        list(
            name = name,
            radii_m = radii_m[o],
            min_dbh_cm = min_dbh_cm[o],
            sf = 1e4 / (pi * radii_m[o]^2)
        ),
        class = c("concentric_design", "inventory_design")
    )

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
