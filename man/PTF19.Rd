\name{PTF19}
\alias{PTF19}
\docType{data}
\title{
PTF19}
\description{
PTF19 is a class PTF giving Mualem-van Genuchten parameters for combinations of USDA texture classes and topsoil/subsoil distinction.}
\usage{data(PTF19)}
\format{
  A data frame with 12 observations on the following 9 variables.
  \describe{
    \item{\code{thr}}{a numeric vector giving residual water content in Mualem-van Genuchten model}
    \item{\code{ths}}{a numeric vector giving saturated water content in Mualem-van Genuchten model}
    \item{\code{alp}}{a numeric vector giving parameter alpha  (cm^(-1)) in Mualem-van Genuchten model}
    \item{\code{n}}{a numeric vector giving parameter n in in Mualem-van Genuchten model}
    \item{\code{m}}{a numeric vector giving parameter m=1-1/n in Mualem-van Genuchten model}
    \item{\code{K0}}{a numeric vector giving parameter K0 (cm d^(-1)) in Mualem-van Genuchten model}
    \item{\code{L}}{a numeric vector giving parameter L in Mualem-van Genuchten model}
    \item{\code{TOPSOIL}}{a character vector with values either "top" or "sub"}
    \item{\code{TEXT_US}}{a character vector with values any of "C" (clay),"CL" (clay loam),"L" (loam),"LS" (loam Sand),"O"(organic),"S"(sand),"SC" (sand clay),"SCL" (sand clay loam),"Si" (silt),"SiC" (silt clay),"SiCL" (silt clay loam),"SiL" (silt loam),"SL" (sand loam)}
	}
  }
\seealso{
\code{\link{predict.ptf}}, \code{\link{ChoosePTF}, \code{\link{ptf.data}}}
}
\references{
Tóth Brigitta, Weynants Mélanie, Nemes Attila, Makó Andras, Bilas George and Tóth Gergely. 2014. New generation of hydraulic pedotransfer functions for Europe. European Journal of Soil Science. in Press.
}
\keyword{datasets}
