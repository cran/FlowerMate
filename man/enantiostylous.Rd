\name{enantiostylous}
\alias{enantiostylous}
\title{3D enantiostylous population example dataset}
\description{Simulated data for one enantiostylous population with 3D data}
\usage{
data(enantiostylous)}
\details{
The dataset contains information from 160 sex organs from 40 individuals. For every sex organ, the dataset contains eight columns with the population code, morph, individual ID, sex organ ID, sex organ type, and coordinates x, y and z. This dataset has been obtained using the SimDimor function as shown in the example section.
}
\value{
A data frame with 160 rows and 8 columns.
}

\author{
A.J. Muñoz-Pajares, V.I. Simón-Porcar
}
\examples{

## This dataset has been generated using:

set.seed(1111)
SimDimor (NIDL=20, NIDS=20,Nst=1,Nan=3,Norg.st=1,Norg.an=1,
LSTmeanX=-10, LSTsdX=2, LANmeanX=c(9,7,-7),LANsdX=c(2,2,2),
SSTmeanX=9, SSTsdX=2, SANmeanX=c(-9,-6,6), SANsdX=c(2,2,2),
LSTmeanY=9, LSTsdY=2, LANmeanY=c(7,3,3) ,LANsdY=c(2,2,2),
SSTmeanY=8, SSTsdY=2, SANmeanY=c(6,2,3), SANsdY=c(2,2,2),
LSTmeanZ=15, LSTsdZ=2, LANmeanZ=c(16,15,15) ,LANsdZ=c(2,2,2),
SSTmeanZ=16, SSTsdZ=2, SANmeanZ=c(15,15,15), SANsdZ=c(2,2,2),
pop_code="enantiostylous")



## To load and test the inaccuracy function:

data(enantiostylous)
#  inaccuracy(enantiostylous)

}
