% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Theme_ggplot2.R
\name{EZ_theme}
\alias{EZ_theme}
\title{ggplot theme}
\usage{
EZ_theme(
  legend = c("TRUE", "bot"),
  supress = c("none", "x", "y", "both"),
  rotate = c("none", "x", "y", "both"),
  angle = 90,
  ...
)
}
\arguments{
\item{legend}{permet de specifier si on veut la presence ou non de la legende, si bot alors la legende est en bas}

\item{supress}{permet de specifier si on veut supprimer la mise en forme pour un des axes x,y ou les deux.}

\item{...}{other}
}
\value{
a ggplot object
}
\description{
ggplot theme
}
