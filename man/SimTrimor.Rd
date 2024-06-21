\name{SimTrimor}
\alias{SimTrimor}
\title{Simulate data of stylar dimorphic populations}

\description{
Produces simulated datasets of heterostylous (trimorphic) individuals with as many rows as number of sex organs defined. One stigmas and two anthers are simulated per individual.
}

\usage{
SimTrimor(NIDL=30,NIDM=30,NIDS=30,
LUPmeanX=0, LUPsdX=1, LBWmeanX=0, LBWsdX=1, LLWmeanX=0, LLWsdX=1,
MUPmeanX=0, MUPsdX=1, MBWmeanX=0, MBWsdX=1, MLWmeanX=0, MLWsdX=1,
SUPmeanX=0, SUPsdX=1, SBWmeanX=0, SBWsdX=1, SLWmeanX=0, SLWsdX=1,

LUPmeanY=0, LUPsdY=1, LBWmeanY=0, LBWsdY=1, LLWmeanY=0, LLWsdY=1,
MUPmeanY=0, MUPsdY=1, MBWmeanY=0, MBWsdY=1, MLWmeanY=0, MLWsdY=1,
SUPmeanY=0, SUPsdY=1, SBWmeanY=0, SBWsdY=1, SLWmeanY=0, SLWsdY=1,

LUPmeanZ=0, LUPsdZ=1, LBWmeanZ=0, LBWsdZ=1, LLWmeanZ=0, LLWsdZ=1,
MUPmeanZ=0, MUPsdZ=1, MBWmeanZ=0, MBWsdZ=1, MLWmeanZ=0, MLWsdZ=1,
SUPmeanZ=0, SUPsdZ=1, SBWmeanZ=0, SBWsdZ=1, SLWmeanZ=0, SLWsdZ=1,

pop_code="test1")
}

\arguments{
  \item{NIDL}{
Number of large-morph individuals to be simulated
}
  \item{NIDM}{
Number of medium-morph individuals to be simulated
}
  \item{NIDS}{
Number of short-morph individuals to be simulated
}
\item{LUPmeanX}{Mean value of the simulated long morph for stigmas in the UP level (X axis).}

\item{LUPsdX}{Standard deviation of the simulated long morph for stigmas in the UP level (X axis).}

\item{LBWmeanX}{Mean value of the simulated long morph for stigmas in the BETWEEN level (X axis).}

\item{LBWsdX}{Standard deviation of the simulated long morph for stigmas in the BETWEEN level (X axis).}

\item{LLWmeanX}{Mean value of the simulated long morph for stigmas in the low (LW) level (X axis).}

\item{LLWsdX}{Standard deviation of the simulated long morph for stigmas in the low (LW) level (X axis).}

\item{MUPmeanX}{Mean value of the simulated medium morph for stigmas in the UP level (X axis).}

\item{MUPsdX}{Standard deviation of the simulated medium morph for stigmas in the UP level (X axis).}

\item{MBWmeanX}{Mean value of the simulated medium morph for stigmas in the BETWEEN level (X axis).}

\item{MBWsdX}{Standard deviation of the simulated medium morph for stigmas in the BETWEEN level (X axis).}

\item{MLWmeanX}{Mean value of the simulated medium morph for stigmas in the low (LW) level (X axis).}

\item{MLWsdX}{Standard deviation of the simulated medium morph for stigmas in the low (LW) level (X axis).}

\item{SUPmeanX}{Mean value of the simulated short morph for stigmas in the UP level (X axis).}

\item{SUPsdX}{Standard deviation of the simulated short morph for stigmas in the UP level (X axis).}

\item{SBWmeanX}{Mean value of the simulated short morph for stigmas in the BETWEEN level (X axis).}

\item{SBWsdX}{Standard deviation of the simulated short morph for stigmas in the BETWEEN level (X axis).}

\item{SLWmeanX}{Mean value of the simulated short morph for stigmas in the low (LOW) level (X axis).}

\item{SLWsdX}{Standard deviation of the simulated short morph for stigmas in the low (LOW) level (X axis).}

\item{LUPmeanY}{Mean value of the simulated long morph for stigmas in the UP level (Y axis).}

\item{LUPsdY}{Standard deviation of the simulated long morph for stigmas in the UP level (Y axis).}

\item{LBWmeanY}{Mean value of the simulated long morph for stigmas in the BETWEEN level (Y axis).}

\item{LBWsdY}{Standard deviation of the simulated long morph for stigmas in the BETWEEN level (Y axis).}

\item{LLWmeanY}{Mean value of the simulated long morph for stigmas in the low (LW) level (Y axis).}

\item{LLWsdY}{Standard deviation of the simulated long morph for stigmas in the low (LW) level (Y axis).}

\item{MUPmeanY}{Mean value of the simulated medium morph for stigmas in the UP level (Y axis).}

\item{MUPsdY}{Standard deviation of the simulated medium morph for stigmas in the UP level (Y axis).}

\item{MBWmeanY}{Mean value of the simulated medium morph for stigmas in the BETWEEN level (Y axis)}

\item{MBWsdY}{Standard deviation of the simulated medium morph for stigmas in the BETWEEN level (Y axis).}

\item{MLWmeanY}{Mean value of the simulated medium morph for stigmas in the low (LW) level (Y axis).}

\item{MLWsdY}{Standard deviation of the simulated medium morph for stigmas in the low (LW) level (Y axis).}

\item{SUPmeanY}{Mean value of the simulated short morph for stigmas in the UP level (Y axis).}

\item{SUPsdY}{Standard deviation of the simulated short morph for stigmas in the UP level (Y axis).}

\item{SBWmeanY}{Mean value of the simulated short morph for stigmas in the BETWEEN level (Y axis).}

\item{SBWsdY}{Standard deviation of the simulated short morph for stigmas in the BETWEEN level (Y axis).}

\item{SLWmeanY}{Mean value of the simulated short morph for stigmas in the low (LOW) level (Y axis).}

\item{SLWsdY}{Standard deviation of the simulated short morph for stigmas in the low (LOW) level (Y axis).}

\item{LUPmeanZ}{Mean value of the simulated long morph for stigmas in the UP level (Z axis).}

\item{LUPsdZ}{Standard deviation of the simulated long morph for stigmas in the UP level (Z axis).}

\item{LBWmeanZ}{Mean value of the simulated long morph for stigmas in the BETWEEN level (Z axis).}

\item{LBWsdZ}{Standard deviation of the simulated long morph for stigmas in the BETWEEN level (Z axis).}

\item{LLWmeanZ}{Mean value of the simulated long morph for stigmas in the low (LW) level (Z axis).}

\item{LLWsdZ}{Standard deviation of the simulated long morph for stigmas in the low (LW) level (Z axis).}

\item{MUPmeanZ}{Mean value of the simulated medium morph for stigmas in the UP level (Z axis).}

\item{MUPsdZ}{Standard deviation of the simulated medium morph for stigmas in the UP level (Z axis).}

\item{MBWmeanZ}{Mean value of the simulated medium morph for stigmas in the BETWEEN level (Z axis).}

\item{MBWsdZ}{Standard deviation of the simulated medium morph for stigmas in the BETWEEN level (Z axis).}

\item{MLWmeanZ}{Mean value of the simulated medium morph for stigmas in the low (LW) level (Z axis).}

\item{MLWsdZ}{Standard deviation of the simulated medium morph for stigmas in the low (LW) level (Z axis).}

\item{SUPmeanZ}{Mean value of the simulated short morph for stigmas in the UP level (Z axis).}

\item{SUPsdZ}{Standard deviation of the simulated short morph for stigmas in the UP level (Z axis).}

\item{SBWmeanZ}{Mean value of the simulated short morph for stigmas in the BETWEEN level (Z axis).}

\item{SBWsdZ}{Standard deviation of the simulated short morph for stigmas in the BETWEEN level (Z axis).}

\item{SLWmeanZ}{Mean value of the simulated short morph for stigmas in the low (LOW) level (Z axis).}

\item{SLWsdZ}{Standard deviation of the simulated short morph for stigmas in the low (LOW) level (Z axis).}

\item{pop_code}{A string, the population code of the simulated data}
}

\details{
Values for sex organs are randomly taken from normal distributions with the defined mean and standard deviation.
}

\value{
A data frame with NID x Nst x Nan rows (one per simulated sex organ) and 8 columns (containing population code, morph, individual ID, sex organ ID, sex organ type, and coordinates x, y and z).
}

\author{
A. J. Muñoz-Pajares and V. I. Simón-Porcar, 
}

\examples{

## Getting a tiny dataset

SimTrimor(2,2,2)

## Getting a largest dataset and performing the analysis
sim.data.tri<-SimTrimor(20,20,20)
inaccuracy(sim.data.tri)

}
