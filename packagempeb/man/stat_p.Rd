% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistiques.R
\name{stat_p}
\alias{stat_p}
\title{Renvoie les stats p de toutes les variables}
\usage{
stat_p(reg, var)
}
\arguments{
\item{reg}{objet régression de R qui provient de la fonction model}

\item{var}{variable qui vaut 0 si le calcul du modèle n'a pas été fait, 1 sinon}
}
\value{
un dataframe avec toutes les p-values de toutes les variables du modèle
}
\description{
Renvoie les stats p de toutes les variables
}
