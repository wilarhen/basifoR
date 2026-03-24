\name{dendroMetrics}
\alias{dendroMetrics}
\title{Summarize dendrometrics}
<<<<<<< HEAD
\description{This function can summarize dendrometric data of the Spanish
National Forest Inventory (SNFI). It can also control most other
functions of the package. Dendrometric variables in the outputs are
transformed into stand units, see the Details section.}
=======
\description{This function summarizes dendrometric data from the Spanish
National Forest Inventory (SNF). It primarily accepts a province
name or number, a local compressed SNF file, or a URL to a
compressed SNF file hosted by \code{www.miteco.gob.es}. It can
also process data frames previously returned by
\code{\link{readNFI}}, \code{\link{nfiMetrics}}, or
\code{\link{metrics2Vol}}. Dendrometric variables in the output
are transformed into stand units, see Details section.}
>>>>>>> basifoR_0.7.1
\usage{dendroMetrics(nfi, summ.vr = "Estadillo", 
    cut.dt = "d == d", 
    report = FALSE, mc.cores = getOption("mc.cores", 
        1L), .parallel = TRUE, 
    ...)}
\arguments{
<<<<<<< HEAD
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed SNFI file (.zip) having data of either
.dbf or .mdb file extensions; or data frame such as that
produced by \code{\link{nfiMetrics}}; or data frame such
as that produced by \code{\link{readNFI}}.}
=======
  \item{nfi}{\code{character}, \code{data.frame}, or \code{list}. A
province name or province number used to locate SNF data;
a local path or URL to a compressed SNF file
(\code{.zip}), including ZIP files hosted by
\code{www.miteco.gob.es}; a data frame such as that
returned by \code{\link{readNFI}},
\code{\link{nfiMetrics}}, or
\code{\link{metrics2Vol}}; or a list of such objects.}
>>>>>>> basifoR_0.7.1
  \item{summ.vr}{\code{character} or \code{NULL}. Name
of a Categorical variables in the SNFI
data used to summarize the outputs. If
\code{NULL} then output from
\code{\link{metrics2Vol}} is
returned. Default \code{'Estadillo'}
processes sample plots.}
  \item{cut.dt}{\code{character}. Logical condition used
to subset the output. Default \code{'d == d'}
avoids subsetting.}
  \item{report}{\code{logical}. Print a \code{report} of the output
in the current working directory.}
  \item{mc.cores}{\code{integer}. Number
of cores used when several
inputs are processed.}
  \item{.parallel}{\code{logical}. If \code{TRUE}, allow
parallel processing when several inputs are
supplied.}
  \item{\dots}{Additional arguments passed to \code{\link{readNFI}},
\code{\link{nfiMetrics}}, or \code{\link{metrics2Vol}},
including \code{nfi.nr} when required.}
}
\details{Dendrometric variables are
summarized according to the levels of
argument \code{summ.vr}. Summary outputs
include the categorical columns defined
by \code{summ.vr} together with the
quantitative variables available after
processing with \code{\link{nfiMetrics}}
and \code{\link{metrics2Vol}}.

These variables may include tree basal
area \code{ba} (\code{'m2 ha-1'}), mean
diameter at breast height \code{d}
(\code{'cm'}), quadratic mean diameter
\code{dg} (\code{'cm'}), mean tree
height \code{h} (\code{'m'}), number of
trees per hectare \code{n}
(\code{'dimensionless'}), and over-bark
volume \code{v} (\code{'m3 ha-1'}).

When \code{summ.vr = NULL}, the function
returns tree-level outputs from
\code{\link{metrics2Vol}} after applying
the filter defined in \code{cut.dt}.

When \code{summ.vr} is not \code{NULL},
the function converts supported
variables to stand units, splits the data
by the requested grouping variable, and
computes summaries by group. Extensive
variables are multiplied by \code{n}
before summation. Variables \code{d},
\code{h}, and \code{Hd}, when present,
are returned as averages weighted by
\code{n}.

If both \code{ba} and \code{n} are
available, the function also derives the
quadratic mean diameter \code{dg}.

Output subsets are extracted using the
logical expression supplied in
\code{cut.dt}, see syntax in
\code{\link{Logic}}.

The function accepts one input object or
several inputs. When several inputs are
supplied, each one is processed
independently and the results are merged
into a single output data frame.
Parallel processing is available through
argument \code{mc.cores} and can be
disabled with \code{.parallel = FALSE}.}
\value{\code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
\code{\link{metrics2Vol}}, or a summary of the variables, see
Details section.}

<<<<<<< HEAD
\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}
=======
\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}
>>>>>>> basifoR_0.7.1




\examples{
## SNFI Data from the province of Madrid
madridNFI <- system.file("ifn3p28_tcm30-293962.zip", package="basifoR")
rmad <- readNFI(madridNFI)[1:100,]
mmad <- nfiMetrics(rmad)
vmad <- metrics2Vol(mmad)
dmad <- dendroMetrics(vmad, cut.dt = 'h > 8')
head(dmad)
## see metric units
attr(dmad,'units')

## Retrieval of SNFI data in 'www.miteco.gob.es' and computation
## of the corresponding dendrometric summary:

## donttest{
## path <- '/es/biodiversidad/servicios/banco-datos-naturaleza/090471228013cbbd_tcm30-278511.zip'
## url2 <- httr::modify_url("https://www.miteco.gob.es", path = path)
## dmad <- dendroMetrics(url2, cut.dt = 'h >= 11')
## head(dmad)
## attr(dmad, 'units')
## }

<<<<<<< HEAD
=======
# Compute some metrics
metrics_ifn4p45 <- nfiMetrics(get_ifn4p45)

# Calculate volume metrics 
vol_ifn4p45 <- metrics2Vol(metrics_ifn4p45)

# Compute all metrics (dendrometrics) for trees with height > 8
dendromet_ifn4p45 <- dendroMetrics(vol_ifn4p45, cut.dt='h > 8')

# Display structure of dendrometric data
str(dendromet_ifn4p45)

# Check units of metrics
attr(dendromet_ifn4p45,'units')

## Alternatively, download data from 'www.miteco.gob.es'
## Specify province name/number to compute dendrometrics:

## donttest{
### Compute dendrometrics for Toledo (code 45) for NFI 4, height >= 8
## dendromet_ifn4p45 <- dendroMetrics(provincia=45,nfi=4,cut.dt='h >= 8')
### Display first few rows
## head(dendromet_ifn4p45)
### Check units of metrics
## attr(dendromet_ifn4p45,'units')
## }
    
>>>>>>> basifoR_0.7.1
}
