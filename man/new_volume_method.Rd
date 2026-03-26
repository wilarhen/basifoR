\name{new_volume_method}
\alias{new_volume_method}
\title{Create a volume-method definition
Create a volume-method definition}
\description{Create a method specification used by the external volume registry.}
\usage{new_volume_method(output, 
    fun = NULL, fun_name = NULL, 
    unit = "m3", raw_unit = unit, 
    scale_to_m3 = 1, 
    build_args = function(ctx, 
        pars, resolved) list(), 
    fallback = function(ctx, 
        pars, resolved) null_or(resolved$preexisting_v_m3, 
        NA_real_), match_by = character(0), 
    get_pars = NULL, 
    pars = NULL, filter_pars = NULL, 
    required_inputs = NULL)}
\arguments{
  \item{output}{Name of the returned \code{output} column.}
  \item{fun}{Function used to compute the \code{output}.}
  \item{fun_name}{Name of a function to look up at run time.}
  \item{unit}{Returned \code{unit}.}
  \item{raw_unit}{Unit returned by the raw method before scaling.}
  \item{scale_to_m3}{Multiplier used to convert raw \code{output} to cubic metres.}
  \item{build_args}{Function that builds arguments for \code{fun}.}
  \item{fallback}{fallback }
  \item{match_by}{Columns used to match parameter rows.}
  \item{get_pars}{Optional function that resolves parameter rows.}
  \item{pars}{Optional embedded parameter table.}
  \item{filter_pars}{Optional function that filters candidate parameter rows.}
  \item{required_inputs}{Inputs required by the method.}
}

\value{A named list describing one external volume-computation method.}

\author{Wilson Lara <wilarhen@gmail.com> [aut, cre] (ORCID: <https://orcid.org/0000-0003-3527-1380>), Cristobal Ordonez <angelcristobal.ordonez@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-5354-3760>), Aitor Vázquez-Veloso <aitor.vazquez.veloso@uva.es> [aut] (ORCID: <https://orcid.org/0000-0003-0227-506X>), Felipe Bravo <felipe.bravo@uva.es> [aut] (ORCID: <https://orcid.org/0000-0001-7348-6695>)}





