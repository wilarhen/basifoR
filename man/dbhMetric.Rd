\name{dbhMetric}
\alias{dbhMetric}
\title{dbhMetric}

\usage{dbhMetric(dbh, met = "d", 
    design = snfi_design())}
\arguments{
  \item{dbh}{\code{numeric}. Either diameters at breast height
<< (\code{mm}) or tree heights (\code{m}). Vectors are
<< averaged. Zero values are formatted to \code{NA}.}
  \item{met}{\code{character}. Any of four metrics: mean diameter
<< at breast height (\code{'d'}), basal area
<< (\code{'ba'}), number of trees (\code{'n'}), or tree
<< height (\code{'h'}). Default \code{'d'}.}
  \item{design}{design }
}



\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <a_cristo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Felipe Bravo <fbravo@pvs.uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}




\examples{

## default SNFI design
dbhMetric(300, "ba")   # basal area for 300 mm DBH
dbhMetric(130, "n")    # trees per ha under SNFI design

## custom concentric design
dsg <- concentric_design_3()
dbhMetric(130, "n", design = dsg)

}
