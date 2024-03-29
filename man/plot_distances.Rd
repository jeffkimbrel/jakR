% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotDistances.R
\name{plot_distances}
\alias{plot_distances}
\title{Boxplot of Beta-diversity Distances}
\usage{
plot_distances(
  p = GlobalPatterns,
  m = "wunifrac",
  s = "X.SampleID",
  d = "SampleType"
)
}
\arguments{
\item{p}{A phyloseq object}

\item{m}{Distance metric}

\item{s}{Column name with sample IDs}

\item{d}{Grouping for comparisons}
}
\description{
Takes a `phyloseq` object with samples grouped in the sample data, calculates
all pairwise beta-diversity metrics, and plots the distances in facets. Now
it returns a list where the df can be accessed with $df, and the plot can be
accessed with $plot.
}
