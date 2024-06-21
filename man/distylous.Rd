\name{distylous}
\alias{distylous}
\title{1D Distylous population example dataset}
\description{Simulated data for one distylous population with 1D data}
\usage{
data(distylous)}
\details{
The dataset contains information from 80 sex organs from 40 individuals. For every sex organ, the dataset contains eight columns with the population code, morph, individual ID, sex organ ID, sex organ type, and coordinates x, y and z. Because the dataset contains information in only one dimension, x and z columns values are 0. This dataset has been obtained using the SimDimor function as shown in the example section.
}
\value{
A data frame with 80 rows and 8 columns.
}

\author{
A.J. Muñoz-Pajares, V.I. Simón-Porcar
}
\examples{

## This dataset has been generated using:

set.seed(2222)
SimDimor (NIDL=20, NIDS=20,Nst=1,Nan=1,Norg.st=1,Norg.an=1,
LSTmeanX=0, LSTsdX=0, LANmeanX=0,LANsdX=0,
SSTmeanX=0, SSTsdX=0, SANmeanX=0, SANsdX=0,
LSTmeanY=8, LSTsdY=1, LANmeanY=4 ,LANsdY=1,
SSTmeanY=5, SSTsdY=1, SANmeanY=9, SANsdY=1,
LSTmeanZ=0, LSTsdZ=0, LANmeanZ=0 ,LANsdZ=0,
SSTmeanZ=0, SSTsdZ=0, SANmeanZ=0, SANsdZ=0,
pop_code="distylous")

## To load and test the inaccuracy function:

data(distylous)
inaccuracy(distylous)

}
