\name{predict.ptf}
\alias{predict.ptf}
\title{
Run pedotransfer functions}
\description{
Wrapper for running any of the European hydraulic PTFs based on the European HYdropedological Data Inventory enclosed in package euptf.}
\usage{
predict.ptf(newdata, ptf, h = NULL, ...)
}
\arguments{
  \item{newdata}{
Data frame containing the values at which predictions are required. The predictors referred to in ptf must be present by name in newdata. To get the correct names, follow example: data(ptf.data)}
  \item{ptf}{
Name of PTF to be run, as a character. Any of ("PTF01","PTF02",...,"PTF15").}
  \item{h}{
Optional vector of suction head value(s) at which the (Mualem-)van Genuchten model must be evaluated.}
  \item{\dots}{
Arguments to be passed to constitutive functions.}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
For \code{\link{PTF01}}, \code{\link{PTF02}}, \code{\link{PTF03}}, \code{\link{PTF04}}, \code{\link{PTF05}}, \code{\link{PTF06}}, \code{\link{PTF07}}, \code{\link{PTF08}}, \code{\link{PTF09}}, \code{\link{PTF10}}, \code{\link{PTF11}}, \code{\link{PTF12}}, \code{\link{PTF13}}, \code{\link{PTF14}}, \code{\link{PTF15}}, \code{\link{PTF16}}, \code{\link{PTF17}}, predict.ptf returns a vector of length N, the number of observations in \code{newdata}.

For \code{\link{PTF18}}, \code{\link{PTF19}}, \code{\link{PTF20}}, \code{\link{PTF21}}, \code{\link{PTF22}}, the function returns a Nx4 or Nx7 matrix of (Mualem-)van Genuchten parameters, with N the number of rows in \code{newdata}.
If \code{h} is given, then it returns a Mx4xN array, with M the length of \code{h}, N the number of rows in \code{newdata}. The second dimension of the array gives h, theta, Se and K (the outputs of function \code{\link{MV}}).
}
\references{
Tóth Brigitta, Weynants Mélanie, Nemes Attila, Makó Andras, Bilas George and Tóth Gergely. 2014. New generation of hydraulic pedotransfer functions for Europe. European Journal of Soil Science. in Press.
}
\author{
M. Weynants}
\section{Warning }{The pedotransfer functions in this package use the following units:
\tabular{ll}{
Length: \tab centimeter\cr
Time: \tab day\cr
Mass: \tab gram}
}

\seealso{
\code{\link{ptf.data}, \code{\link{ChoosePTF}}, \code{\link{MV}}}
}
\examples{
# load example data
data(ptf.data)
# set modified FAO texture class
ptf.data$TEXT_FAO_MOD <- psd2classFAO_MOD(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=TRUE)
# set USDA texture class
ptf.data$TEXT_US <- psd2classUS(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=TRUE)
# examples of usage of predict.ptf
ths_fao <- predict.ptf(ptf.data,"PTF01")
fc_usda <- predict.ptf(ptf.data,"PTF07")
wp_psd_OC <- predict.ptf(ptf.data,"PTF10")
ks_psd_ph_cec <- 10^(predict.ptf(ptf.data,"PTF17"))
ks_psd_oc <- 10^(predict.ptf(ptf.data,"PTF16"))

### Parameters estimation and evaluation of Mualem-van Genuchten model
# define a vector h of 24 suction head values
h <- c(0,10^(seq(0,4.5,by=0.2)))
## if only MRC is needed
# get van Genuchten parameters
vg_psd_oc_bd_ph <- predict.ptf(ptf.data,"PTF22")
# evaluate van Genuchten model at h
thk_psd_oc_bd_ph1 <- predict.ptf(ptf.data,"PTF22",h)
## if both MRC and HCC are needed
# get Mualem-van Genuchten parameters
mv_psd_oc_bd_ph <- predict.ptf(ptf.data,"PTF19")
# evaluate Mualem-van Genuchten model at h
thk_psd_oc_bd_ph2 <- predict.ptf(ptf.data,"PTF19",h)
## compare obtained MRC for the PTF22 and PTF19
plot(thk_psd_oc_bd_ph1[,2,],thk_psd_oc_bd_ph2[,2,],xlab="MRC predicted with PTF22",ylab="MRC predicted with PTF19")
abline(0,1,col="grey")
# The two PTFs produce different results: when only retention data are needed, PTF15 is recommended, but if conductivity data are also required, PTF02 should be use.

## example to save predicted values to xlsx file:
require("xlsx")
# if not already installed
## NOT RUN
# install.packages("xlsx")
# require("xlsx")
# write.xlsx(x = mv_psd_oc_bd_ph, file = "pred_MVG.xlsx", sheetName = "pred_MVG", row.names = FALSE)
##
}
