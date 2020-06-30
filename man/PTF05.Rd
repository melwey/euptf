\name{PTF05}
\alias{PTF05}
\docType{data}
\title{
PTF05}
\description{
PTF05 is a linear regression based PTF predicting the saturated water content (THS) from PSD T/S, OC and BD.
}
\usage{data(PTF05)}
\format{
	A list of 8:
	\describe{
	\item{t}{Rxp numeric matrix with the results of running \code{PTF05$R} times \code{lm} on \code{PTF05$formula}.}
	\item{R}{a scalar giving the number of resamplings}
	\item{n.train}{a scalar giving the number of observations in the training set}
	\item{n.total}{a scalar giving the total number of observations used in the crossvalidation}
	\item{call}{call}
	\item{meth}{character. Dataset on which the PTF was trained.}
	\item{formula}{Class 'formula'. Here: THS ~ 1 + I(1/(OC + 1)) + I(1/(OC + 1)):I(BD^2) + I(BD^2) + TOPSOIL + TOPSOIL:I(BD^2) +      USCLAY + USCLAY:TOPSOIL + USSILT + USSILT:USCLAY + USSILT:I(BD^2) + USCLAY:I(BD^2)}
	\item{median_model}{Named numeric vector of length p with the median values of columns of \code{PTF05$t}.}
	}
}
\details{
PTF05 was obtained by running \code{PTF05$R} times \code{lm} on \code{PTF05$formula}. The formula was built based on cross-validated stepwise selection.}
\seealso{
\code{\link{predict.ptf}}, \code{\link{ChoosePTF}, \code{\link{ptf.data}}}
}
\references{
Tóth Brigitta, Weynants Mélanie, Nemes Attila, Makó Andras, Bilas George and Tóth Gergely. 2014. New generation of hydraulic pedotransfer functions for Europe. European Journal of Soil Science. in Press.
}
\keyword{datasets}
