% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recodeCheck.R
\name{recodeCheck}
\alias{recodeCheck}
\title{Change labeling of \code{checkbox} variables}
\usage{
recodeCheck(
  df,
  vars,
  old = c("Unchecked", "Checked"),
  new = c("No", "Yes"),
  reverse = FALSE
)
}
\arguments{
\item{df}{A data frame, presumably retrieved from REDCap, though not a
strict requirement.}

\item{vars}{Optional character vector of variables to convert.  If left
missing, all of the variables in \code{df} that are identified as
\code{checkbox} variables are relabeled.  See 'Details' for more about
identifying \code{checkbox} variables.}

\item{old}{A character vector to be passed to \code{factor}.
This indicates the levels to be replaced and their order.}

\item{new}{A character vector of labels to replace the values in
\code{levels}.  The first value becomes the reference value.}

\item{reverse}{For convenience, if the user would prefer to reverse the
order of the elements in \code{levels} and \code{labels},
simply set this to \code{TRUE}.}
}
\description{
Rewrites the labeling of \code{checkbox} variables from
Checked/Unchecked to Yes/No (or some other user specified labeling).
}
\details{
\code{checkbox} variables are not identified using the metadata
from the REDCap database.  Instead, variables are scanned, and those
variables in which every value is in \code{levels} are assumed to be
\code{checkbox} variables.

Realistically, this could be used to relabel any set of factors with
identical labels, regardless of the data source.  The number of labels is
not limited, but \code{levels} and \code{labels} should have the same length.

The actual code to perform this is not particularly difficult
(\code{df[checkbox] <- lapply(df[checkbox], factor, levels=levels, labels=labels)}),
but \code{checkbox} variables are common enough in REDCap
(and the Checked/Unchecked scheme so unpalatable) that a quick way to
replace the labels was highly desirable
}
