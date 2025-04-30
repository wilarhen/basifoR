\name{dendroMetrics}
\alias{dendroMetrics}
\title{Summarize dendrometrics}
\description{This function can summarize dendrometric data of the Spanish
National Forest Inventory (SNF). It can also control most other
functions of the package. Dendrometric variables in the outputs are
transformed into stand units, see the Details section.}
\usage{dendroMetrics(nfi, summ.vr = "Estadillo", 
    cut.dt = "d == d", 
    report = FALSE, ...)}
\arguments{
  \item{nfi}{\code{character} or \code{data.frame}.  URL/path to a
compressed SNF file (.zip) having data of either
.dbf or .mdb file extensions; or data frame such as that
produced by \code{\link{nfiMetrics}}; or data frame such
as that produced by \code{\link{readNFI}}.}
  \item{summ.vr}{\code{character} or \code{NULL}. Name
of a Categorical variables in the SNF
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
  \item{\dots}{Additional arguments in \code{\link{metrics2Vol}} or
\code{\link{nfiMetrics}} or \code{\link{readNFI}}.}
}
\details{Dendrometric variables are
summarized according to the levels of
the argument \code{summ.vr}. The summary
outputs include the categorical columns
formulated in \code{summ.vr} and the
variables defined using
arguments/defaults in
\code{\link{nfiMetrics}}. These
variables include the tree basal area
\code{ba} (\code{'m2 ha-1'}), the
average diameter at breast height
\code{d} (\code{'cm'}), the quadratic
mean diameter \code{dg} (\code{'cm'}),
the average tree height \code{h}
(\code{'m'}), the number of trees by
hectare \code{n} ('dimensionless'), and
the over bark volume \code{v} (\code{'m3
ha-1'}). Subsets of the output summary
are extracted using logical expressions
in argument \code{'cut.dt'}, see syntax
in \code{\link{Logic}}.}
\value{\code{data.frame}. Depending on \code{summ.vr = NULL}, an output from
\code{\link{metrics2Vol}}, or a summary of the variables, see
Details section.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (<https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (<https://orcid.org/0000-0001-7348-6695>)}




\examples{
## Process SNF data for Toledo stored locally
# Path to Toledo data file in 'basifoR' package
ifn4p45 <- system.file("Ifn4_Toledo.zip", package="basifoR")

# Decompress SNF data from the specified file path or URL
fetch_ifn4p45 <- fetchNFI(ifn4p45)

# Read and process the data (first 100 rows)
get_ifn4p45 <- getNFI(fetch_ifn4p45)[1:100,]

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

\donttest{
#Compute dendrometrics for Toledo (code 45) for NFI 4, height >= 8
dendromet_ifn4p45 <- dendroMetrics(provincia=45,nfi=4,cut.dt='h >= 8')
#Display first few rows
head(dendromet_ifn4p45)
#Check units of metrics
attr(dendromet_ifn4p45,'units')
}
    
}
