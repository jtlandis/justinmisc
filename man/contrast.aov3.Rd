% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dfanalysis.R
\name{contrast.aov3}
\alias{contrast.aov3}
\title{Three factor contrast anova}
\usage{
contrast.aov3(df, response, factor3)
}
\arguments{
\item{df}{data frame passed containing value vector and factors}

\item{response}{positive integer that describes the index of the response vector}

\item{factor3}{positive integer vector of length three that describes the index
of three factor vectors within the passed df.}
}
\value{
return dataframe of anova test statistic
}
\description{
Generate the anova test statistics for three factors
}
