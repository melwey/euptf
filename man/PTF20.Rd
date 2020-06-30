\name{PTF20}
\alias{PTF20}
\docType{data}
\title{
PTF20}
\description{
PTF20 is a linear regression based PTF predicting the parameters of van Genuchten model from PSD, OC, pH and CEC. }
\usage{data(PTF20)}
\format{
  The format is a list of 4.
	\describe{
	\item{thr}{rpart object predicting parameter thr. see \code{\link{PTF03}}.}
	\item{ths}{linear regression PTF predicting parameter thr. see \code{\link{PTF04}}.}
	\item{alp}{linear regression PTF predicting parameter log10(alp). see \code{\link{PTF05}}.}
	\item{n}{linear regression PTF predicting parameter log10(n-1). see \code{\link{PTF05}}.}
	}
}
\seealso{
\code{\link{predict.ptf}}, \code{\link{ChoosePTF}, \code{\link{ptf.data}}}
}
\source{
Tóth Brigitta, Weynants Mélanie, Nemes Attila, Makó Andras, Bilas George and Tóth Gergely. 2014. New generation of hydraulic pedotransfer functions for Europe. European Journal of Soil Science. in Press.
}
\keyword{datasets}
