\name{predict.ptf.raster}
\alias{predict.ptf.raster}
\title{
Run pedotransfer functions on a stack of raster layers}
\description{
Wrapper for running any of the European hydraulic PTFs based on the European HYdropedological Data Inventory enclosed in package euptf on a stack of raster layers (RasterStack.)}
\usage{
predict.ptf(dataStack, PTF, filename, format="GTiff", overwrite = !file.exists(filename), transfo=NULL)
}
\arguments{
  \item{dataStack}{
RasterStack containing layers with the predictors. The predictors referred to in PTF must be present as named layers in dataStack. To get the correct names, follow example: data(ptf.data). //
Layers containing the categorical variables TEXT_FAO_MOD, TEXT_US, TOPSOIL should use the  numbered coding as given in \code{\link{psd2classFAO_MOD}}, \code{\link{psd2classUS}} and TOPSOIL=1 for topsoil and TOPSOIL=2 for subsoil.}
  \item{PTF}{
Name of the PTF to be run, as a character. Any of ("PTF01","PTF02",...,"PTF15").}
  \item{filename}{
Output filename. Character}
  \item{format}{
Output file format. Character. See \code{\link{raster::writeFormats}}. Defaults to "GTiff".}
  \item{overwrite}{
Should existing "filename" be overwritten? Logical. Defaults to FALSE.}
  \item{transfo}{
List of transformations to operate on the predictors before running the PTFs, e.g. list(PH_H2O='/10',BD='/1000'). For details on the units required by the PTFs, see \code{\link{euptf-package}}.}
}
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
A \code{\link[raster:raster]{RasterLayer}} or \code{\link[raster:brick]{RasterBrick}} object.}

\author{
M. Weynants}
\section{Warning }{The pedotransfer functions in this package use the following units:
\tabular{ll}{
Length: \tab centimeter\cr
Time: \tab day\cr
Mass: \tab gram}
}

\seealso{
\code{\link{ptf.data}, \code{\link{ChoosePTF}}, \code{\link[raster:raster]{raster:raster}}, \code{\link[raster:stack]{raster:RasterStack}}}
}
\examples{
# load example data
data(ptf.data)
# set modified FAO texture class
ptf.data$TEXT_FAO_MOD <- psd2classFAO_MOD(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=FALSE)
# set USDA texture class
ptf.data$TEXT_US <- psd2classUS(ptf.data$USSAND,ptf.data$USSILT,ptf.data$USCLAY,ptf.data$OC,option=FALSE)
# reset TOPSOIL to numeric
ptf.data$TOPSOIL <- as.numeric(ptf.data$TOPSOIL)-1;ptf.data$TOPSOIL[ptf.data$TOPSOIL==0]<-2
# create a RasterStack
s <- brick(ncol=10,nrow=10,nl=ncol(ptf.data))
ind <- sort(sample(1:100,60))
m <- matrix(NA,ncol=ncol(ptf.data),nrow=ncell(s))
m[ind,] <-  as.matrix(ptf.data,ncol=ncol(ptf.data))
s <- stack(setValues(s, as.numeric(m)))
names(s) <- names(ptf.data)
# examples of usage of predict.ptf.raster
ths_fao <- predict.ptf.raster(s,"PTF01",filename='tmp1.tif')
fc_usda <- predict.ptf.raster(s,"PTF07",filename='tmp2.tif')
wp_psd_OC <- predict.ptf.raster(s,"PTF10",filename='tmp3.tif')

### Parameters estimation of Mualem-van Genuchten model
## if only MRC is needed
# get van Genuchten parameters
vg_psd_oc_bd_ph <- predict.ptf.raster(s,"PTF22",filename='tmp4.tif')
## if both MRC and HCC are needed
# get Mualem-van Genuchten parameters
mv_psd_oc_bd_ph <- predict.ptf.raster(s,"PTF19",filename='tmp5.tif')
}
