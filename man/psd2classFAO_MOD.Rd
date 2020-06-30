\name{psd2classFAO_MOD}
\alias{psd2classFAO_MOD}
\title{
Generate modified FAO texture class}
\description{
Generate modified FAO soil texture class from sand, silt, clay content, and organic carbon content to distinguish organic soils.}
\usage{
psd2classFAO_MOD(sand = 0, silt = 0, clay = 0, OC = 0, option = FALSE, tol=1)
}
\arguments{
  \item{sand}{
numeric vector of N observations of sand content in mass percent. Particles of diameter between 0.05 and 2 mm.}
  \item{silt}{
numeric vector of N observations of silt content in mass percent. Particles of diameter between 0.002 and 0.05 mm.}
  \item{clay}{
numeric vector of N observations of clay content in mass percent. Particles of diameter less than 0.002 mm.}
  \item{OC}{
numeric vector of N observations of organic carbon content in mass percent. Determined by dry combustion after removal of carbonates or harmonised to that method.}
  \item{option}{
logical. If FALSE (default), the output will be returned as a numeric vector. If TRUE, the output will be returned as a character vector.}
	\item{tol}{numeric setting the tolerance on the sum of sand, silt, clay. Default is 1: sum(sand,silt,clay) > 101 or < 99 will return NA.}
}
\details{The texture classes are defined as:
"coarse": 1, "medium": 2, "medium fine": 3, "fine": 4, "very fine": 5, "organic": 9.
Organic soils are defined as soils having clay content >= 60 \% clay and organic carbon >= 18 \% , or having clay content < 60 \% and organic carbon >= (12+clay*0.1).
}
\value{
A numeric or character vector of length N with FAO texture class for each observation.
}
\references{
CEC (1985). Soil Map of the European Communities, 1:1,000,000. CEC Luxembourg, 124pp. 7 maps.

IUSS Working Group WRB. 2006. World reference base for soil resources 2006. World Soil Resources Reports No. 103. FAO, Rome
}
\author{
M. Weynants
}
\seealso{
\code{\link{psd2classUS}},\code{\link[soiltexture:TT.points.in.classes]{TT.points.in.classes}}
}
\examples{
data(ptf.data)
ptf.data$TEXT_FAO_MOD <- psd2classFAO_MOD(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=TRUE)
}
