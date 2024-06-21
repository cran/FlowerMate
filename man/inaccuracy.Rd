\name{inaccuracy}
\alias{inaccuracy}
\title{Estimation of Inaccuracy and Reciprocity indexes}
\description{Computation of the Inaccuracy Index of Armbruster et al. (2017) and the Reciprocity Index of Sánchez et al. (2008; 2013), or their multidimensional variants (Simón-Porcar et al., 2023), for populations of stylar-polymorphic species.}
\usage{
inaccuracy(input = NULL, useonly.dim = c("x", "y", "z"),
useonly.vert = "all", verbose = FALSE, na.rm = FALSE,intramorph = FALSE)
}
\arguments{
  \item{input}{
A dataframe containing information on the position of sex organs for different individuals. See details for further information on the specific format required.}
  \item{useonly.dim}{
Dimensions to be considered for the analysis. By default "x", "y", and/or "z" columns will be used if they contain numeric values different from 0.
}
  \item{useonly.vert}{
A vector of characters defining the verticile considered for the analysis. By default all verticiles are used, but this argument make it possible to, for example, analyse only the subset of stiles labeled as "ST1".}
  \item{verbose}{
A logical to obtain a complete list of indices estimated during the computation.}
  \item{na.rm}{
A logical to perform the analysis without considering missing data (excluding these values only). Accurate estimates of reciprocity and inaccuracy should rely on a dataset with no missing data. An uneven distribution of missing data across dimensions or floral morphs may incur on biased estimates for both parameters. In fact, the original Microsoft Excel Macro of Sánchez et al. (2008, 2013) does not allow the computation of datasets with missing data. A warning will be provided if set to TRUE.
}
  \item{intramorph}{
A logical to perform the computation using only intramorph comparisons.
  }
}
\details{
The input dataframe must contain one row per measured sex organ and at least \bold{eight} columns with the following colnames: \bold{pop_code} (population names), \bold{morph} ("L" or "S" to identify long- and short-styled morphs, respectively, for distylous population datasets and "L", "M", "S" to identify long-, mid- and short-styled morphs, respectively, for tristylous plants; this nomenclature should be used also for any other stylar dimorphism or trimorphism analysed; we suggest that in enantiostylous populations, for instance, “L” and “S” are used for left-handed and right-handed morphs, respectively); \bold{ID_indiv} (individual names); \bold{ID_sexorg} (a numeric ID for every sex organ measured in the population),	\bold{sexorg} ("ST" for stamen and "AN" for anther. Categories within sex organs can be defined adding a number, such as "ST1", "ST2",...), \bold{x} (position of the sex organ within the x dimension), \bold{y} (position of the sex organ within the y dimension), \bold{z} (position of the sex organ within the x dimension). See data(distylous) to get an example.}

\value{
If verbose is set to FALSE, this function provides a numeric vector with the following values per population:

\bold{nL, nS}:  number of individuals from each morph in dimorphic plants (named as L and S). In the case of trimorphic populations the number of individuals of the third morph (named as M) is also provided (\bold{nM}).

\bold{Inac.u.y}: Armbruster Inaccuracy value obtained for the “upper” level sex organs (generally, for the stigmas of the L morph and the opposite morph/s anthers) in the Y axis. \bold{Inac.u.xy} or \bold{Inac.u.xyz} will be provided if the input file contains information for any of these axes. Inaccuracy is calculated as (Armbruster et al. 2017; Simón-Porcar et al., 2023 for multidimensional variants).

\bold{Inac.b.y}: Armbruster Inacuracy value obtained for the “between” level sex organs of trimorphic populations (generally, for the stigmas of the M morph and the opposite morphs anthers) in the Y axis. \bold{Inac.b.xy} or \bold{Inac.b.xyz} will be provided if the input file contains information for any of these axes.

\bold{Inac.perc.u.y}: Armbruster Percentage of Inaccuracy value obtained for the “upper” level sex organs (generally, for the stigmas of the L morph and the opposite morph/s anthers) in the Y axis.

\bold{Inac.perc.u.xy} or \bold{Inac.perc.u.xyz} will be provided if the input file contains information for any of these axes. The Percentage of Inaccuracy values are calculated as (Armbruster et al., 2017).

\bold{Inac.perc.d.y}: Armbruster Percentage of Inaccuracy value obtained for the “down” level sex organs (generally, for the stigmas of the S morph and the opposite morph/s anthers) in the Y axis.

\bold{Inac.perc.d.xy} or \bold{Inac.perc.d.xyz} will be provided if the input file contains information for any of these axes.

\bold{Inac.perc.b.y}: Armbruster Percentage of Inaccuracy value obtained for the “between” level sex organs (generally, for the stigmas of the M morph and the opposite morphs anthers in trimorphic populations) in the Y axis. 

\bold{Inac.perc.b.xy} or \bold{Inac.perc.b.xyz} will be provided if the input file contains information for any of these axes.

\bold{TotalInac.y}: Armbruster Total inacuracy value in the Y axis (calculated as ; Armbruster et al. 2017).

\bold{TotalInac.xy} or \bold{TotalInac.xyz} will be provided if the input file contains information for any of these axes.

\bold{Mean2Stand.TotalInac.y}: Armbruster Mean 2 -standardized total inaccuracy in the Y axis
(calculated as . \bold{Mean2Stand.TotalInac.xy} or \bold{Mean2Stand.TotalInac.xyz} will be
provided if the input file contains information for any of these axes.

\bold{r}: Sánchez r calculated as for the one, two or three dimensions computed (see Sánchez et
al. 2008, 2013; Simón-Porcar et al. 2023 for details).

\bold{R}: Sánchez R calculated as for the one, two or three dimensions computed (see Sánchez et
al. 2008, 2013; Simón-Porcar et al. 2023 for details).

If verbose is set to TRUE, this function provides a list with three elements:

\bold{Results} a numeric vector identical to the verbose=FALSE output

\bold{Armbruster} a numeric vector with all the parameters estimated during the computation of the Armbruster Inaccuracy Index.

\bold{Sanchez} a numeric vector with all the indices estimated during the computation of the Sanchez Reciprocity Index
}
\references{
Armbruster, W. S., Bolstad, G. H., Hansen, T. F., Keller, B., Conti, E., & Pélabon, C. (2017). The measure and mismeasure of reciprocity in heterostylous flowers. New Phytologist, 215(2), 906-917.

Sánchez JM, Ferrero V, Navarro L. 2008. A new approach to the quantification of degree of reciprocity in distylous (sensu lato) plant populations. Annals of Botany 102: 463–472.

Sánchez JM, Ferrero V, Navarro L. 2013. Quantifying reciprocity in distylous and tristylous plant populations. Plant Biology 15: 616–620.

Simón-Porcar VI, Muñoz-Pajares AJ, Arroyo J, Johnson SD (2023). Multidimensional reciprocity and inaccuracy indexes in style-polymorphic plant populations. Applications in Plant Sciences in Press
}
\author{
A. J. Muñoz-Pajares and V. I. Simón-Porcar
}

\examples{

data(distylous)
inaccuracy(distylous)


### Generating data with multiple populations and
### performing the analysis:

data(distylous2anthers)
inaccuracy(distylous2anthers,useonly.vert=c("ST","AN2"))

}
