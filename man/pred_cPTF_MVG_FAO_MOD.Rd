\name{pred_cPTF_MVG_FAO_MOD}
\alias{pred_cPTF_MVG_FAO_MOD}
\alias{pred_cPTF_MVG_US}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
class PTF predicting parameters of Mualem van Genuchten model}
\description{
class PTF predicting the parameters of Mualem van Genuchten model describing the soil water retention and hydraulic conductivity curves based on texture class (modified FAO or USDA) and topsoil/subsoil distinction (T/S)}
\usage{
pred_cPTF_MVG_FAO_MOD(newdata, nms = c("SAMPLE_ID", "TOPSOIL", "TEXT_FAO_MOD"))

pred_cPTF_MVG_US(newdata, nms = c("SAMPLE_ID", "TOPSOIL", "TEXT_US"))
}
\arguments{
  \item{newdata}{
data frame containing at least columns in nms: sample identifier, T/S and texture class}
  \item{nms}{
character vector of the column names for the sample identifier, T/S (TRUE/FALSE or "top"/"sub"), and the modified FAO texture class ("coarse","medium","medium fine", "fine","very fine","organic") or the USDA texture class ("C","CL","L","LS","O","S","SC","SCL","Si","SiC","SiCL","SiL","SL"). Default value is c("SAMPLE_ID", "TOPSOIL", "TEXT_FAO_MOD") for \code{pred_cPTF_MVG_FAO_MOD} and c("SAMPLE_ID", "TOPSOIL", "TEXT_US") for \code{pred_cPTF_MVG_US}.}
}
\details{
\code{pred_cPTF_MVG_FAO_MOD} loads \code{\link{PTF18}} and merges it to \code{newdata} by T/S and FAO texture class.

\code{pred_cPTF_MVG_US} loads \code{\link{PTF19}} and merges it to \code{newdata} by T/S and USDA texture class. The "O" class has been added to the standard USDA classes to account for organic soils.
}

\value{
nx7 matrix with parameters thr,ths,alp,n,m,Ks and L for each row in \code{newdata}.}
\references{
%% ~put references to the literature/web site here ~
}
\author{
M. Weynants}
\note{
These functions can be executed through \code{\link{predict.ptf}}.}


\seealso{
\code{\link{psd2classFAO_MOD}},\code{\link{psd2classUS}},\code{\link{predict.ptf}},\code{\link{MV}}
}
\examples{
data(ptf.data)
# generate texture classes
ptf.data$TEXT_FAO_MOD <- psd2classFAO_MOD(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC*1.724,option=TRUE)
ptf.data$TEXT_US <- psd2classUS(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=TRUE)
# predict MRC and HCC (MVG parameters) based on modified FAO and T/S
mvgFAO_MOD <- pred_cPTF_MVG_FAO_MOD(ptf.data)
# predict MRC and HCC (MVG parameters) based on USDA and T/S
mvgUS <- pred_cPTF_MVG_US(ptf.data)
# plot predicted MRC and HCC for observation i in ptf.data
# set i
i=5
h <- 10^(seq(0,5,by=0.2))
par(mfrow=c(1,2))
plot(log10(h),MV(mvgFAO_MOD[i,],h)$theta,col="red",type="l",ylab="theta")
lines(log10(h),MV(mvgUS[i,],h)$theta,col="blue")
plot(log10(h),log10(MV(mvgFAO_MOD[i,],h)$K),col="red",type="l",ylab="log10(K)")
lines(log10(h),log10(MV(mvgUS[i,],h)$K),col="blue")
legend("topright",legend=c("PTF01","PTF03"),pch=NA,lty=1,col=c("red","blue"))
}