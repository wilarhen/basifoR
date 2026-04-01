# basifoR

`basifoR` provides reproducible workflows for Spanish National Forest Inventory (SNFI) data in R.

The package helps users move from raw inventory archives to analysis-ready outputs. It supports archive retrieval and ingestion, design-aware dendrometric computation, tree-level volume estimation, and stand-level summarization. It also exposes extensible components for custom sampling designs, schemas, and volume methods, so users can adapt the same processing logic to external inventory data under explicit assumptions.

This repository contains the version used in the manuscript:

> **R Package basifoR: Reproducible Workflows for the Spanish National Forest Inventory**

## Main features

- Retrieve and import SNFI archives distributed as compressed files
- Read legacy SNFI tables stored as `.dbf`, `.mdb`, and `.accdb`
- Compute tree-level metrics such as diameter, height, basal area, and expansion factors
- Estimate tree-level volume with native SNFI volume equations
- Summarize stand-level outputs such as `n`, `d`, `dg`, `h`, `ba`, and volume
- Run the workflow stepwise or through a single entry point with `inventoryMetrics()`
- Adapt the workflow to external inventories with explicit designs, schemas, and volume methods

## Installation

### Install the manuscript version from GitHub

```r
install.packages("remotes")
remotes::install_github("wilarhen/basifoR@v0.7.7")
```

### Load the package

```r
library(basifoR)
```

## System requirements

Most package functionality runs directly in R once you install the package. Some workflows may require extra system components to read legacy database formats used by SNFI archives.

### Windows

Workflows that import `.mdb` or `.accdb` files may require Microsoft Access connectivity.

The recommended option is the **Microsoft 365 Access Runtime**. Install the version that matches your local Office installation and then restart R.

### Linux

Legacy Access connectivity is commonly provided through `mdbtools`.

On Debian-based systems, install it with:

```bash
sudo apt update
sudo apt install mdbtools
```

### macOS

Install `mdbtools` with Homebrew:

```bash
brew install mdbtools
```

### Practical note

These external components affect only archive import from legacy database files. Once data are in R, the downstream workflow runs normally.

If import fails after installation, first check that:
- the external database engine matches your local system configuration
- you restarted the R session

## Quick start

### One-call workflow

`inventoryMetrics()` provides the main user-facing entry point.

```r
library(basifoR)

dtol <- inventoryMetrics("toledo", nfi.nr = 4, cut.dt = "h >= 8")
head(dtol)
attr(dtol, "units")
```

This call retrieves the SNFI data for Toledo, filters trees with height greater than or equal to 8 m, and returns stand-level summaries.

### Stepwise workflow

You can also run the workflow explicitly:

```r
library(basifoR)

z <- system.file("ifn3p28.zip", package = "basifoR")

x <- readNFI(z)
m <- nfiMetrics(x)
v <- metrics2Vol(m)
s <- inventoryMetrics(v)

head(s)
attr(s, "units")
```

This route makes each processing step explicit:

```text
readNFI() -> nfiMetrics() -> metrics2Vol() -> inventoryMetrics()
```

## Minimal examples

### Import a local archive shipped with the package

```r
library(basifoR)

z <- system.file("ifn3p28.zip", package = "basifoR")
x <- readNFI(z)

is.data.frame(x)
dim(x)
```

### Compute tree-level metrics

```r
library(basifoR)

dbh_mm <- c(80, 130, 230, 430)

dbhMetric(dbh_mm, met = "ba")
dbhMetric(dbh_mm, met = "n")
```

### Summarize by plot

```r
library(basifoR)

z <- system.file("ifn3p28.zip", package = "basifoR")
s <- inventoryMetrics(z, summ.vr = "Estadillo")

head(s)
attr(s, "units")
```

## External inventory support

`basifoR` can also process external inventory data when you define:

- the sampling design
- the column mapping
- the volume method

Core helpers for this workflow include:

- `new_concentric_design()`
- `new_external_schema()`
- `new_volume_method()`
- `external_volume_method_registry()`
- `external_dendroMetrics()`
- `inventoryMetrics(..., backend = "external")`

This extension is useful when you want to reuse the same design-aware processing logic beyond the SNFI under explicit assumptions.

## Main functions

### Data access and import

- `getNFI()`
- `fetchNFI()`
- `readNFI()`

### Tree-level computation

- `dbhMetric()`
- `nfiMetrics()`
- `metrics2Vol()`

### Stand-level summarization

- `inventoryMetrics()`

### External inventory extensions

- `new_concentric_design()`
- `new_external_schema()`
- `new_volume_method()`
- `external_volume_method_registry()`
- `external_dendroMetrics()`

## Data sources

Example Spanish National Forest Inventory archives are distributed with the package and are also available from the official website of the Ministry for the Ecological Transition and the Demographic Challenge of Spain.

The external example used in the manuscript relies on the official IGN export resource for the French National Forest Inventory.

## Development status

This GitHub repository provides the public development and review version of `basifoR`.

The exact version used in the manuscript is available as GitHub release `v0.7.7`.

## License

MIT License

## Citation

If you use `basifoR`, please cite the package and the associated manuscript when available.

## Contact

Wilson Lara  
wilson.lara@uva.es
