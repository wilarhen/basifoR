# basifoR

`basifoR` provides reproducible, design-aware workflows for Spanish National Forest Inventory (SNFI) data in R.

The package helps users move from raw inventory archives to analysis-ready outputs. It supports archive retrieval and ingestion, tree-level metric computation, tree-level volume estimation, and stand-level summarization. It also exposes extensible components for custom sampling designs, schemas, and volume methods, so the same workflow can be adapted to external inventory data under explicit assumptions.

This repository contains the version used in the manuscript:

> **R Package basifoR: Reproducible Workflows for the Spanish National Forest Inventory**

## Quick example

The main user-facing entry point is `inventoryMetrics()`. It accepts one inventory input or several inputs and returns grouped stand-level summaries.

```r
library(basifoR)

dm <- inventoryMetrics(
  list("toledo", "palencia", "madrid"),
  nfi.nr = list(4, 3, 2),
  mc.cores = 3,
  summ.vr = c("pr", "estadillo")
)

str(dm)
attr(dm, "units")
```

This example processes three Spanish provinces from three SNFI stages in one call. `inventoryMetrics()` resolves each input, dispatches the required SNFI workflow internally, computes tree-level metrics and volume where needed, and returns stand-level summaries grouped by province (`pr`) and plot (`estadillo`). With `mc.cores = 3`, the inputs can be processed in parallel when your system supports it. The returned object is a data frame with standard summary variables such as basal area, mean diameter, quadratic mean diameter, mean height, trees per hectare, and volume, together with unit metadata.

## Main features

- Retrieve and import SNFI archives distributed as compressed files
- Read SNFI tables stored as `.dbf`, `.mdb`, and `.accdb`
- Compute design-aware tree-level metrics such as diameter, height, basal area, and expansion factors
- Estimate tree-level volume with the native SNFI volume equations
- Summarize stand-level outputs such as `n`, `d`, `dg`, `h`, `ba`, and volume
- Run the workflow stepwise or through a single dispatcher with `inventoryMetrics()`
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

## SNFI workflow

`basifoR` supports both a stepwise workflow and a one-call workflow.

### One-call workflow

```r
library(basifoR)

dtol <- inventoryMetrics("toledo", nfi.nr = 4, cut.dt = "h >= 8")
head(dtol)
attr(dtol, "units")
```

### Stepwise workflow

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

## External inventory support

`basifoR` can also process external inventory data through the same design-aware logic used for the SNFI. External workflows make the required assumptions explicit: you define the sampling design, declare how source columns map to diameter and height, and supply a volume method or parameter table when tree volume must be computed.

The external workflow can start from raw tree tables or from already standardized metric tables. In a typical workflow:

1. define the inventory design with `new_inventory_design()` or `new_concentric_design()`
2. define reusable column mappings and units with `new_external_schema()`
3. compute tree-level metrics with `externalMetrics()` when needed
4. compute tree-level volume outputs with `externalMetrics2Vol()` and a registry built with `external_volume_method_registry()` and `new_volume_method()`
5. summarize tree-level or grouped outputs with `external_dendroMetrics()` or through `inventoryMetrics(..., backend = "external")`

These helpers let you standardize measurements, compute design-aware expansion factors, derive optional dominant height and volume outputs, and return either tree-level records or grouped stand summaries. The external backend supports repeated workflows, grouped summaries, optional provenance tracking, and custom volume definitions.

A minimal set of core helpers is:

- `new_inventory_design()` or `new_concentric_design()`
- `new_external_schema()`
- `externalMetrics()`
- `new_volume_method()`
- `external_volume_method_registry()`
- `externalMetrics2Vol()`
- `external_dendroMetrics()`
- `inventoryMetrics(..., backend = "external")`

## Main functions

### Workflow dispatcher

- `inventoryMetrics()` — unified entry point that dispatches complete workflows for SNFI and external inventories

### SNFI archive access and import

- `getNFI()` — resolve a province identifier or archive input and fetch matching SNFI files
- `fetchNFI()` — download and extract files from local or remote compressed archives
- `readNFI()` — import extracted SNFI tables into R

### Tree-level metric computation

- `dbhMetric()` — compute compact tree-level metrics such as diameter, height, basal area, and trees per hectare
- `nfiMetrics()` — standardize SNFI measurements and compute tree-level dendrometric variables

### Tree-level volume computation

- `metrics2Vol()` — compute tree-level SNFI volume outputs from standardized metrics
- `snfi_volume_method_registry()` — inspect the active SNFI volume-method registry
- `default_snfi_volume_equations()` — return the bundled SNFI equation definitions

### Stand-level summarization

- `dendroMetrics()` — summarize SNFI tree data into grouped stand-level outputs

### External inventory design and schemas

- `new_inventory_design()` — define custom fixed-area sampling designs
- `new_concentric_design()` — define custom concentric sampling designs
- `new_external_schema()` — store reusable external column mappings, units, and defaults

### External tree-level processing

- `externalMetrics()` — standardize external tree measurements into basifoR metric units
- `externalMetrics2Vol()` — compute external tree-level volume outputs from standardized metrics or raw inputs
- `external_volume_method_registry()` — build the active registry of external volume methods
- `new_volume_method()` — define one custom external volume method

### External summarization

- `external_dendroMetrics()` — process external tree tables and return tree-level or grouped stand-level outputs

## Data sources

Example Spanish National Forest Inventory archives are distributed with the package and are also available from the official website of the Ministry for the Ecological Transition and the Demographic Challenge of Spain.

The external example used in the manuscript relies on the official IGN export resource for the French National Forest Inventory.

## Development status

This GitHub repository provides the public development and review version of `basifoR`.

The exact version used in the manuscript is available as GitHub release `v0.7.7`.

## License

GPL-3 License

## Citation

If you use `basifoR`, please cite the package and the associated manuscript when available.

## Contact

Wilson Lara  
wilarhen@gmail.com
