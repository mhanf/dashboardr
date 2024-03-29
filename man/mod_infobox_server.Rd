% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_infobox_server.R
\name{mod_infobox_server}
\alias{mod_infobox_server}
\title{infobox server part}
\usage{
mod_infobox_server(
  id,
  df_graph,
  r = NULL,
  theme_var,
  default_pattern = "^\%r\%"
)
}
\arguments{
\item{id}{module id}

\item{df_graph}{A graph dashboarder dataframe}

\item{r}{r internal list (advanced use)}

\item{theme_var}{bslib theme variables}

\item{default_pattern}{default pattern to identify r code in dashboardr dataframe}
}
\value{
an infobox server
}
\description{
infobox server part
}
