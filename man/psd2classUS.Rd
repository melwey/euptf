\name{psd2classUS}
\alias{psd2classUS}
\title{
Generate USDA texture class}
\description{
Generate USDA soil texture class from sand, silt, clay content, and organic carbon content to distinguish organic soils.}
\usage{
psd2classUS(sa, si, cl, oc = NULL, option = FALSE, tol=1)
}
\arguments{
  \item{sa}{numeric vector of N observations of sand content in mass percent. Particles of diameter between 0.05 and 2 mm.}
  \item{si}{
numeric vector of N observations of silt content in mass percent. Particles of diameter between 0.002 and 0.05 mm.}
  \item{cl}{
numeric vector of N observations of clay content in mass percent. Particles of diameter less than 0.002 mm.}
  \item{oc}{
optional numeric vector of N observations of organic carbon content in mass percent. Determined by dry combustion after removal of carbonates or harmonised to that method.}
  \item{option}{
logical. If FALSE (default), the output will be returned as a numeric vector. If TRUE, the output will be returned as a character vector.}
  \item{tol}{numeric setting the tolerance on the sum of sand, silt, clay. Default is 1: sum(sand,silt,clay) > 101 or < 99 will return NA.}
}
\details{
Codes used for the textural classification:
\tabular{lll}{
1=C (Clay) \tab 5=CL (Clay Loam) \tab 9=L (Loam)\cr
2=SiC (Silt clay) \tab 6=SCL (Sand Clay Loam) \tab 10=SL (Sand Loam)\cr
3=SC (Sand Clay) \tab 7=Si (silt) \tab 11=LS (Loam Sand)\cr
4=SiCL (Silt Clay Loam) \tab 8=SiL (silt Loam) \tab 12=S (Sand)\cr
13=O (organic) \tab  \tab \cr}
Organic soils are defined as soils having clay content >= 60 \% clay and organic carbon >= 18 \% , or having clay content < 60 \% and organic carbon >= (12+clay*0.1).
}
\value{
A numeric or character vector of length N with USDA texture class for each observation.
}
\references{
E. Benham, R.J. Ahrens, W.D. Nettleton. MO5 Soil Technical Note-16. March 19, 2009. Subject: SOI – Clarification of Textural Class Boundaries. National Soil Survey Center, USDA-NRCS, Lincoln, Nebraska 

IUSS Working Group WRB. 2006. World reference base for soil resources 2006. World Soil Resources Reports No. 103. FAO, Rome
}
\author{
M. Weynants
}
\seealso{
\code{\link{psd2classFAO_MOD}}, \code{\link[soiltexture:TT.points.in.classes]{TT.points.in.classes}}
}
\examples{
data(ptf.data)
ptf.data$TEXT_US <- psd2classUS(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=TRUE)
}