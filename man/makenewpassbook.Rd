% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/passbook.R
\name{makenewpassbook}
\alias{makenewpassbook}
\title{construct empty passbook}
\usage{
makenewpassbook(year, ploc = inhome("/current/", year, ".passbook"),
  qOverWrite = FALSE)
}
\arguments{
\item{year}{basis for dates in the passbook and default year for passbook name}

\item{ploc}{full path where passbook is to be written}

\item{qOverWrite}{should an existing passbook be overwritten if needed}
}
\description{
construct empty passbook
}
