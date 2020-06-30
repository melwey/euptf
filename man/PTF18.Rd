\name{PTF18}
\alias{PTF18}
\docType{data}
\title{
PTF18
}
\description{
PTF18 is a class PTF giving Mualem-van Genuchten parameters for combinations of modified FAO texture classes and topsoil/subsoil distinction.}
\usage{data(PTF18)}
\format{
  a data frame with 12 observations on the following 9 variables.
  \describe{
    \item{\code{thr}}{a numeric vector giving residual water content in Mualem-van Genuchten model}
    \item{\code{ths}}{a numeric vector giving saturated water content in Mualem-van Genuchten model}
    \item{\code{alp}}{a numeric vector giving parameter alpha  (cm^(-1)) in Mualem-van Genuchten model}
    \item{\code{n}}{a numeric vector giving parameter n in in Mualem-van Genuchten model}
    \item{\code{m}}{a numeric vector giving parameter m=1-1/n in Mualem-van Genuchten model}
    \item{\code{K0}}{a numeric vector giving parameter K0 (cm d^(-1)) in Mualem-van Genuchten model}
    \item{\code{L}}{a numeric vector giving parameter L in Mualem-van Genuchten model}
    \item{\code{TOPSOIL}}{a character vector with values either "top" or "sub"}
    \item{\code{TEXT_FAO_MOD}}{a character vector with values "coarse", "medium", "medium fine", "fine", "very fine", "organic"}
  }
}

\seealso{
\code{\link{predict.ptf}}, \code{\link{ChoosePTF}, \code{\link{ptf.data}}}
}

\references{
Tóth Brigitta, Weynants Mélanie, Nemes Attila, Makó Andras, Bilas George and Tóth Gergely. 2014. New generation of hydraulic pedotransfer functions for Europe. European Journal of Soil Science. in Press.
}

\keyword{datasets}
