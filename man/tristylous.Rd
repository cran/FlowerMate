\name{tristylous}
\alias{tristylous}
\title{1D tristylous population example dataset}
\description{Simulated data for one tristylous population with 1D data}
\usage{
data(tristylous)}
\details{
The dataset contains information from 180 sex organs from 60 individuals. For every sex organ, the dataset contains eight columns with the population code, morph, individual ID, sex organ ID, sex organ type, and coordinates x, y and z. Because the dataset contains information in only one dimension, x and z columns values are 0. This dataset has been obtained using the SimTrimor function as shown in the example section.
}
\value{
A data frame with 180 rows and 8 columns.
}

\author{
A.J. Muñoz-Pajares, V.I. Simón-Porcar
}
\examples{

## This dataset has been generated using:


set.seed(4444)
SimTrimor(NIDL=20,NIDM=20,NIDS=20,
LUPmeanX=0, LUPsdX=0, LBWmeanX=0 ,LBWsdX=0, LLWmeanX=0, LLWsdX=0,
MUPmeanX=0, MUPsdX=0, MBWmeanX=0 ,MBWsdX=0, MLWmeanX=0, MLWsdX=0,
SUPmeanX=0, SUPsdX=0, SBWmeanX=0 ,SBWsdX=0, SLWmeanX=0, SLWsdX=0,
LUPmeanY=12, LUPsdY=2, LBWmeanY=8 ,LBWsdY=2, LLWmeanY=4, LLWsdY=2,
MUPmeanY=12, MUPsdY=2, MBWmeanY=8 ,MBWsdY=2, MLWmeanY=4, MLWsdY=2,
SUPmeanY=12, SUPsdY=2, SBWmeanY=8 ,SBWsdY=2, SLWmeanY=4, SLWsdY=2,
LUPmeanZ=0, LUPsdZ=0, LBWmeanZ=0 ,LBWsdZ=0, LLWmeanZ=0, LLWsdZ=0,
MUPmeanZ=0, MUPsdZ=0, MBWmeanZ=0 ,MBWsdZ=0, MLWmeanZ=0, MLWsdZ=0,
SUPmeanZ=0, SUPsdZ=0, SBWmeanZ=0 ,SBWsdZ=0, SLWmeanZ=0, SLWsdZ=0,
pop_code="tristylous")




## To load and test the inaccuracy function:

data(tristylous)
# inaccuracy(tristylous)

}
