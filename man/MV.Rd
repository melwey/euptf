\name{MV}
\alias{MV}
\title{
Mualem van Genuchten model}
\description{
Evaluates Mualem van Genuchten model describing the soil moisture retention and hydraulic conductivity curve as a function of the suction head h (L) or the water content theta (L^3 L^-3)
}
\usage{
MV(par, h = NULL, theta = NULL)
}
\arguments{
  \item{par}{
vector of parameters of the (Mualem-)van Genuchten model: \code{c(thr,ths,alp,n,m,K0,L)}, with thr the residual water content,ths the saturated water content, alp (L^(-1)), n (-) and m (-) shape parameters. K0 (L T^(-1)) is the hydraulic conductivity at h=0 and L a shape parameter (-). If only the retention curve is needed, K0 and L can be omitted. If the conductivity is needed, m should be equal to 1-1/n.}
  \item{h}{
vector of suction heads at which the model is to be evaluated. If NULL, \code{theta} must be provided.}
  \item{theta}{
vector of water content at which the model is to be evaluated. If NULL, \code{h}  must be provided.
}
}
\details{
The van Genuchten model is defined as: 
\deqn{Se = \frac{\theta - \theta_r}{\theta_s - \theta_r} = (\alpha h^n)^{-m}}{%
	Se = (theta - thr)/(ths - thr) = (alp * h^n)^(-m)}

Provided that m=1-1/n, it can be coupled with the Mualem model as: 
\deqn{K = K_0 * (Se)^L * (1-(1-Se^{1/m})^m)^2}{
	K = K0 * (Se)^L * (1-(1-Se^(1/m))^m)^2}
}
\value{
A data frame with \code{length(h)} rows with columns:
  \item{h}{provided as arguments or calculated as a function of \code{theta}}
  \item{theta}{provided as argument or calculated as a function of \code{h}}
  \item{Se}{relative saturation}
  \item{K}{soil hydraulic conductivity (LT^(-1)}
}
\references{
Van Genuchten, M. T. (1980). A closed-form equation for predicting the hydraulic conductivity of unsaturated soils. Soil Science Society of America Journal, 44, 89-98.}
\author{
M. Weynants}

\section{Warning }{The pedotransfer functions in this package use the following units:
\tabular{ll}{
Length: \tab centimeter\cr
Time: \tab day\cr
Mass: \tab gram}
}

\seealso{
\code{\link{predict.ptf}}
}
\examples{
mvg <- MV(par=c(0.02,0.45,0.02,3.5,1-1/3.5,150,0.5),h=10^(seq(0,5,by=0.1)))
par(mfrow=c(2,1))
plot(theta ~ h, data = mvg, log = "x")
plot(K ~ h, data = mvg, log = "xy")
}