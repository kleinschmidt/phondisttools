% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classification.R
\name{unlist_models}
\alias{unlist_models}
\title{Convert named list of models to data frame}
\usage{
unlist_models(l, names_col, model_col = "model")
}
\arguments{
\item{l}{named list of models}

\item{names_col}{name of column generated for names}

\item{model_col}{='model' name of column generated for models}
}
\value{
a data frame with names in names_col and models in model_col
}
\description{
Undoes \code{\link{list_models}}
}
