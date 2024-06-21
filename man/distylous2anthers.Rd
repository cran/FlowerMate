\name{distylous2anthers}
\alias{distylous2anthers}
\title{1D and 2 anthers Distylous population example dataset}
\description{Simulated data for one 2-anthers distylous population with 1D data}
\usage{
data(distylous2anthers)}
\details{
The dataset contains information from 120 sex organs from 40 individuals. For every sex organ, the dataset contains eight columns with the population code, morph, individual ID, sex organ ID, sex organ type, and coordinates x, y and z. Because the dataset contains information in only one dimension, x and z columns values are 0. This dataset has been obtained using the SimDimor function as shown in the example section.
}
\value{
A data frame with 120 rows and 8 columns.
}

\author{
A.J. Muñoz-Pajares, V.I. Simón-Porcar
}
\examples{

## This dataset has been generated using:

set.seed(3333)
SimDimor (NIDL=20, NIDS=20,Nst=1,Nan=2,Norg.st=1,Norg.an=1,
LSTmeanX=0, LSTsdX=0, LANmeanX=c(0,0),LANsdX=c(0,0),
SSTmeanX=0, SSTsdX=0, SANmeanX=c(0,0), SANsdX=c(0,0),
LSTmeanY=8, LSTsdY=1, LANmeanY=c(6,5) ,LANsdY=c(1,1),
SSTmeanY=4, SSTsdY=1, SANmeanY=c(6,5), SANsdY=c(1,1),
LSTmeanZ=0, LSTsdZ=0, LANmeanZ=c(0,0) ,LANsdZ=c(0,0),
SSTmeanZ=0, SSTsdZ=0, SANmeanZ=c(0,0), SANsdZ=c(0,0),
pop_code="styledimorphic")


## To load and test the inaccuracy function:

data(distylous2anthers)
inaccuracy(distylous2anthers)

}
