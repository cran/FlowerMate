\name{SimDimor}
\alias{SimDimor}
\title{Simulate data of heterostylous individuals}

\description{
Produces simulated datasets of heterostylous individuals with as many rows as number of sexual organs defined.
}
\usage{
SimDimor(NIDL=30, NIDS=30,Nst=1,Nan=1,Norg.st=1,Norg.an=1,
LSTmeanX=20, LSTsdX=0.5, LANmeanX=8,LANsdX=0.5,
SSTmeanX=8, SSTsdX=0.5, SANmeanX=20, SANsdX=0.5,
LSTmeanY=20, LSTsdY=0.5, LANmeanY=8 ,LANsdY=0.5,
SSTmeanY=8, SSTsdY=0.5, SANmeanY=20, SANsdY=0.5,
LSTmeanZ=20, LSTsdZ=0.5, LANmeanZ=8 ,LANsdZ=0.5,
SSTmeanZ=8, SSTsdZ=0.5, SANmeanZ=20, SANsdZ=0.5, pop_code="test1")
}

\arguments{
  \item{NIDL}{
Number of large-morph individuals to be simulated
}
  \item{NIDS}{
Number of short-morph individuals to be simulated
}
  \item{Nst}{
Number of stigma levels per individual
}
  \item{Nan}{
Number of anther levels per individual
}
  \item{Norg.st}{
Number of stigmas per level in every individual
}
  \item{Norg.an}{
Number of anthers per level in every individual
}
  \item{LSTmeanX}{
Mean value of the simulated long stigmas in the X axis
}
  \item{LSTsdX}{
Standard deviation of the simulated long stigmas in the X axis
}
  \item{LANmeanX}{
Mean value of the simulated long anthers in the X axis
}
  \item{LANsdX}{
Standard deviation of the simulated long anthers in the X axis
}
  \item{SSTmeanX}{
Mean value of the simulated short stigmas in the X axis
}
  \item{SSTsdX}{
Standard deviation of the simulated shor stigmas in the X axis
}
  \item{SANmeanX}{
Mean value of the simulated short anthers in the X axis
}
  \item{SANsdX}{
Standard deviation of the simulated short anthers in the X axis
}
  \item{LSTmeanY}{
Mean value of the simulated long stigmas in the Y axis
}
  \item{LSTsdY}{
Standard deviation of the simulated long stigmas in the Y axis
}
  \item{LANmeanY}{
Mean value of the simulated long anthers in the Y axis
}
  \item{LANsdY}{
Standard deviation of the simulated long anthers in the Y axis
}
  \item{SSTmeanY}{
Mean value of the simulated short stigmas in the Y axis
}
  \item{SSTsdY}{
Standard deviation of the simulated shor stigmas in the Y axis
}
  \item{SANmeanY}{
Mean value of the simulated short anthers in the Y axis
}
  \item{SANsdY}{
Standard deviation of the simulated short anthers in the Y axis
}
  \item{LSTmeanZ}{
Mean value of the simulated long stigmas in the Z axis
}
  \item{LSTsdZ}{
Standard deviation of the simulated long stigmas in the Z axis
}
  \item{LANmeanZ}{
Mean value of the simulated long anthers in the Z axis
}
  \item{LANsdZ}{
Standard deviation of the simulated long anthers in the Z axis
}
  \item{SSTmeanZ}{
Mean value of the simulated short stigmas in the Z axis
}
  \item{SSTsdZ}{
Standard deviation of the simulated shor stigmas in the Z axis
}
  \item{SANmeanZ}{
Mean value of the simulated short anthers in the Z axis
}
  \item{SANsdZ}{
Standard deviation of the simulated short anthers in the Z axis
}
  \item{pop_code}{
A string, the population code of the simulated data
}
}

\details{
Values for sexual organs are randomly taken from a normal distribution with the defined mean and standard deviation.
}

\value{
A data frame with NID x Nst x Nan rows (one per simulated sexual organ) and 8 columns (containing population code, morph, individual ID, sexual organ ID, sexual organ type, and coordinates x, y and z).
}

\author{
A. J. Muñoz-Pajares and V. I. Simón-Porcar
}

\examples{

## Getting a tiny dataset
SimDimor(NIDL=2,NIDS=2)

## Getting a largest dataset and performing the analysis
sim.data<-SimDimor(NIDL=30,NIDS=30)
inaccuracy(sim.data)

}
