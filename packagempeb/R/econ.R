#' Calcul l'économie estimée en fin d'année
#'
#' @param dd jeu de données avec a minima une colonne date, une colonne énergie et une colonne prédiction
#' @param date_deb_ref date de début de la période de référence
#' @param date_fin_ref date de fin de la période de référence
#'
#' @return un dataframe avec la date et le pourcentage d'économie
#' @export
#'

economie<-function(dd,date_deb_ref,date_fin_ref){
  dd.suiv=subset(dd,date<date_deb_ref|date>date_fin_ref)
  dd.ref=subset(dd,date>=date_deb_ref&date<=date_fin_ref)
  economie_d=dd.suiv$prediction-dd.suiv$energie
  economie_cum=matrix(ncol=nrow(dd.suiv))
  ponder=matrix(ncol=nrow(dd.suiv))
  taille=nrow(dd.ref)
  taille2=nrow(dd.suiv)
  j=floor(taille2/taille)+1
  for (i in seq(from=1, to=taille2)){
    if (i>2 & i%%taille==0) {
      economie_cum[i]=sum(economie_d[1:i])
      ponder[i]=sum(dd.suiv$prediction[(1+taille*(floor((i-1)/taille))):i])
    }else{
      economie_cum[i]=sum(economie_d[1:i])
      ponder[i]=sum(dd.suiv$prediction[(1+taille*(floor((i-1)/taille))):i])+sum(dd.ref[(i-taille*(floor(i/nrow(dd.ref)))):nrow(dd.ref),2])
    }
  }
  eco_pourcent2=t(economie_cum/ponder*100)
  return(data.frame(date=dd.suiv$date,economie=eco_pourcent2))
}
