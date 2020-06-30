\name{predict.lrptf}
\alias{predict.lrptf}
\title{
Evaluates linear regression based PTF}
\description{
Evaluates linear regression based PTF on new data.}
\usage{
predict.lrptf(lrptf, newdata, na.action = na.pass, ensemble = FALSE)
}
\arguments{
  \item{lrptf}{
linear regression based PTF. Any of \code{\link{PTF04}}, \code{\link{PTF05}}, \code{\link{PTF06}}, \code{\link{PTF07}}, \code{\link{PTF12}}, \code{\link{PTF13}}, \code{\link{PTF14}}, \code{\link{PTF15}}.}
  \item{newdata}{
data frame containing the values at which the PTF has to be evaluated. The predictors referred to in \code{lrptf} must be present by name in \code{newdata}. To get the correct names, follow example: \code{data(ptf.data)}.}
  \item{na.action}{
how to handle missing values. Default is \code{na.pass}}
  \item{ensemble}{
logical. If TRUE, an ensemble of PTFs are evaluated. If FALSE, the median model is used.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
a vector of length \code{nrow(newdata)}.}

\author{
M. Weynants
}
\note{
This function should preferably be executed through \code{\link{predict.ptf}}.}

\seealso{
\code{\link{predict.ptf}}
}
\examples{
data(ptf.data)
data(PTF05)
THS_psd_ts_oc_bd <- predict.lrptf(PTF05,ptf.data)
data(PTF06)
THS_psd_ts_bd_ph <- predict.lrptf(PTF06,ptf.data)
plot(THS_psd_ts_oc_bd,THS_psd_ts_bd_ph)
abline(0,1,col="grey")
}