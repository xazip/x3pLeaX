% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x3p_raster_relabel.R
\name{x3p_raster_relabel_replace}
\alias{x3p_raster_relabel_replace}
\title{Replace mask label with a new label}
\usage{
x3p_raster_relabel_replace(df, mask_1, mask_2, multiple = FALSE, select_col)
}
\arguments{
\item{df}{object of class 'x3p' or a tibble style dataframe containing 'x3p' objects}

\item{mask_1}{old mask label the user wishes to replace, must be a color code #8digits}

\item{mask_2}{new mask label user wishes to replace with old mask label, must be a color code #8digits}

\item{multiple}{clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe}

\item{select_col}{if multiple = TRUE, column name containing the x3p objects}
}
\description{
Replace mask label with a new label
}
