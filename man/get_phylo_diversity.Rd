% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getPhyloDiversity.R
\name{get_phylo_diversity}
\alias{get_phylo_diversity}
\title{Phylogenetic Diversity Measures}
\usage{
get_phylo_diversity(p, runs = 999, null.model = "frequency", minOccurrence = 1)
}
\arguments{
\item{p}{A phyloseq object}

\item{runs}{Number of bootstraps to run}

\item{null.model}{The null.model for the SES calculations}

\item{minOccurrence}{Species must be found at least this many times in at least one sample to be kept}
}
\description{
Phylogenetic Diversity Measures
}
