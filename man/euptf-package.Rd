\name{euptf-package}
\alias{euptf-package}
\alias{euptf}
\docType{package}
\title{
Hydraulic pedotransfer functions for Europe
}
\description{
euptf is a set of hydraulic pedotransfer functions trained on the European HYdropedological Data Inventory. It allows point estimation of soil water content at saturation (THS), field capacity (FC, pF 2.5) and wilting point (WP, pF 4.2) and of saturated hydraulic conductivity (KS). It can also return parameter estimations of the (Mualem-)van Genuchten model describing the soil water retention and unsaturated hydraulic conductivity curves (MRC and HCC).}
\details{
\tabular{ll}{
Package: \tab euptf\cr
Type: \tab Package\cr
Version: \tab 1.4\cr
Date: \tab 2016-03-09\cr
License: \tab GPL-3\cr
}
euptf contains 15 PTFs predicting soil hydraulic properties  from a variety of input data, in an hierarchical way. The PTFs can all be run through a unique interface: \code{\link{predict.ptf}}. The PTFs can be accessed with function \code{data}. See their names in the table below.

Other features include utilities to choose the right PTF according to available data (\code{\link{ChoosePTF}}), obtain texture classes (\code{\link{psd2classFAO_MOD}}, \code{\link{psd2classUS}}), run the (Mualem-)van Genuchten model (\code{\link{MV}}) and an example dataset (\code{\link{ptf.data}}).

\tabular{llll}{
PTF name \tab Input \tab Method \tab Output\cr
\code{\link{PTF01}}: \tab FAO_MOD+T/S \tab RT \tab THS\cr
\code{\link{PTF02}}: \tab FAO_MOD+T/S+OC \tab RT \tab THS\cr
\code{\link{PTF03}}: \tab USDA+T/S \tab RT \tab THS\cr
\code{\link{PTF04}}: \tab PSD+T/S+OC \tab RT \tab THS\cr
\code{\link{PTF05}}: \tab PSD+T/S+OC+BD \tab LRt \tab THS\cr
\code{\link{PTF06}}: \tab PSD+T/S+BD+pH \tab LRt \tab THS\cr
\code{\link{PTF07}}: \tab FAO_MOD+T/S \tab RT \tab FC\cr
\code{\link{PTF08}}: \tab USDA+T/S \tab RT \tab FC\cr
\code{\link{PTF09}}: \tab PSD+OC \tab LRt \tab FC\cr
\code{\link{PTF10}}: \tab FAO_MOD+T/S \tab RT \tab WP\cr
\code{\link{PTF11}}: \tab USDA+T/S \tab RT \tab WP\cr
\code{\link{PTF12}}: \tab PSD+OC \tab LRt \tab WP\cr
\code{\link{PTF13}}: \tab FAO_MOD+T/S \tab RT \tab log10(KS)\cr
\code{\link{PTF14}}: \tab FAO_MOD+T/S+OC \tab RT \tab log10(KS)\cr
\code{\link{PTF15}}: \tab USDA+T/S \tab RT \tab log10(KS)\cr
\code{\link{PTF16}}: \tab PSD+T/S+OC \tab RT \tab log10(KS)\cr
\code{\link{PTF17}}: \tab PSD+T/S+pH+CEC \tab LR \tab log10(KS)\cr
\code{\link{PTF18}}: \tab FAO_MOD+T/S \tab cPTF \tab MRC+HCC\cr
\code{\link{PTF19}}: \tab USDA+T/S \tab cPTF \tab MRC+HCC\cr
\code{\link{PTF20}}: \tab PSD+OC+pH+CEC \tab LRt \tab MRC\cr
\code{\link{PTF21}}: \tab PSD+T/S+OC+BD \tab LR \tab MRC\cr
\code{\link{PTF22}}: \tab PSD+T/S+OC+BD+pH \tab LRt2 \tab MRC\cr
}
T/S: topsoil/subsoil distinction, FAO_MOD: modified FAO texture classes (5 classes for mineral soils and an organic class), USDA: USDA-SCS texture classes and an organic class, PSD: particle size distribution (sand, silt, clay \%), OC: organic carbon (\%), BD: bulk density (g cm^(-3)), pH: soil pH in water, CEC: cation exchange capacity (meq (100 g)^(-1)).

cPTF: class PTF, RT: regression tree, LR: linear regression, LRt: linear regression with transformed variables and interactions, LRt2: linear regression with transformed variables.

MRC: moisture retention curve (parameters of van Genuchten model), HCC: hydraulic conductivity curve (parameters of Mualem-van Genuchten model), THS: saturated water content, FC: water content at field capacity (pF 2.5 or h=330 cm), WP: water content at wilting point (pF 4.2 or h=15848 cm), log10(KS): base 10 logarithm of saturated hydraulic conductivity.
}
\author{
Melanie Weynants, Brigitta Tóth}

\references{
Tóth Brigitta, Weynants Mélanie, Nemes Attila, Makó Andras, Bilas George and Tóth Gergely. 2014. New generation of hydraulic pedotransfer functions for Europe. European Journal of Soil Science. doi:10.1111/ejss.12192.

Weynants M. et al. 2013. European Hydropedological Data Inventory (EU-HYDI). EUR Scientific and Technical Research Series. Luxembourg: Publication Office of the European Commission. 168p. doi: 10.2788/5936.

Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal, 44, 89-98.
}
\keyword{ package }
\seealso{
\code{\link[http://soilwater.r-forge.r-project.org/]{soilwaterptf}}
}
\section{Warning }{The pedotransfer functions in this package use the following units:
\tabular{ll}{
Length: \tab centimeter\cr
Time: \tab day\cr
Mass: \tab gram}
}
\examples{
# load example data
data(ptf.data)
# generate texture classes
ptf.data$TEXT_FAO_MOD <- psd2classFAO_MOD(ptf.data$USSAND, ptf.data$USSILT, ptf.data$USCLAY,
  ptf.data$OC, option=TRUE)
ptf.data$TEXT_US <- psd2classUS(ptf.data$USSAND, ptf.data$USSILT, ptf.data$USCLAY,
  ptf.data$OC, option=TRUE)
# predict saturated water content with PTF18 (modified FAO texture class and top/sub-soil distinction)
ths_fao <- predict.ptf(ptf.data,"PTF18")[,"ths"]
# predict field capacity with PTF19 (USDA texture class and top/sub-soil distinction)
fc_usda <- predict.ptf(ptf.data,"PTF19",h=330)[,"theta",]
# predict wilting point with PTF12 (particle size distribution and organic carbon content)
wp_psd_OC <- predict.ptf(ptf.data,"PTF12")

}
