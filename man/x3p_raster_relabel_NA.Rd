% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/x3p_raster_relabel.R
\name{x3p_raster_relabel_NA}
\alias{x3p_raster_relabel_NA}
\title{Relabel mask of an 'x3p' object for inclusion of missing values
This function allows a user to relabel a mask of an 'x3p' object
The user is also allowed to specify a new mask name and mask color.}
\usage{
x3p_raster_relabel_NA(
  df,
  mask_1,
  mask_2,
  color_new,
  annotation_new,
  multiple = FALSE,
  select_col
)
}
\arguments{
\item{df}{object of class 'x3p' or a tibble style dataframe containing 'x3p' objects}

\item{mask_1}{old mask label the user wishes to replace, must be a color code #8digits}

\item{mask_2}{new mask label user wishes to replace with old mask label, must be a color code #8digits}

\item{color_new}{Optional color specification for new mask label}

\item{annotation_new}{Rename label associated with the new mask}

\item{multiple}{clarify whether user is working with a single 'x3p' object or multiple in a tibble style dataframe}

\item{select_col}{if multiple = TRUE, location of the x3p objects}
}
\value{
'x3p' object with altered mask
}
\description{
Relabel mask of an 'x3p' object for inclusion of missing values
This function allows a user to relabel a mask of an 'x3p' object
The user is also allowed to specify a new mask name and mask color.
}
