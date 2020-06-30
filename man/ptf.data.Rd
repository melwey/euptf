\name{ptf.data}
\alias{ptf.data}
\docType{data}
\title{
Soil properties example data}
\description{
Example data of soil properties formatted to run the PTFs from package euptf.}
\usage{data(ptf.data)}
\format{
  A data frame with 60 observations on the following 10 variables.
  \describe{
    \item{\code{SAMPLE_ID}}{a numeric vector}
    \item{\code{TOPSOIL}}{a factor with levels \code{sub} \code{top}}
    \item{\code{USSAND}}{a numeric vector of sand content in mass percent. Particles of diameter between 0.05 and 2 mm.}
    \item{\code{USSILT}}{a numeric vector of silt content in mass percent. Particles of diameter between 0.002 and 0.05 mm.}
    \item{\code{USCLAY}}{a numeric vector of clay content in mass percent. Particles of diameter less than 0.002 mm.}
    \item{\code{OC}}{a numeric vector of organic carbon content in mass percent. Determined by dry combustion after removal of carbonates or harmonised to that method.}
    \item{\code{BD}}{a numeric vector of bulk density measurements (grams cm^(-3))}
    \item{\code{PH_H2O}}{a numeric vector of soil pH in 1:2.5 soil:water solution.}
    \item{\code{CEC}}{a numeric vector of cation exchange capacity (meq  (100 g)^(-1))}
    \item{\code{CACO3}}{a numeric vector of calcium carbonates content in mass percent.}
  }
}
\references{
Weynants M. et al. 2013. European Hydropedological Data Inventory (EU-HYDI). EUR Scientific and Technical Research Series. Luxembourg: Publication Office of the European Commission. 168p. doi: 10.2788/5936. }
\seealso{\code{\link{predict.ptf}}}
\examples{
data(ptf.data)
str(ptf.data)
}
\keyword{datasets}
