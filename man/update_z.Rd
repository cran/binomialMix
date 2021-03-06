% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/update_z.R
\name{update_z}
\alias{update_z}
\title{M-step : Update the matrix of working variables Z from beta iterative equation}
\usage{
update_z(df, col_id = "id", target, beta_up, df_id, matrix_id)
}
\arguments{
\item{df}{A dataframe}

\item{col_id}{A character value corresponding to id column name}

\item{target}{A character value corresponding to the target variable}

\item{beta_up}{A matrix of estimated beta in a specific cluster k}

\item{df_id}{A list of dataframe filter by id levels}

\item{matrix_id}{A list of design matrices filter by id levels}
}
\value{
work_z An up-to-date matrix of working variables Z
}
\description{
M-step : Update the matrix of working variables Z from beta iterative equation
}
