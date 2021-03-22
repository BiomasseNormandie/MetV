#' Fonction qui retourne le dataset avec une colonne supplémentaire d'occupation/inoccupation
#'
#' @param dd jeu de données avec au moins une colonne date, une variable température et une colonne énergie
#'
#' @return le jeu de données d'entrée avec en plus une colonne occup qui vaut 0 ou 1 en fonction de l'occupation prédite par le modèle
#' @import lubridate
#' @export
#'
occupation<-function(dd){
  eload_col <- dd$energie
  temp_col <- dd$Temperature
  dd$ferie <- 0

  minute_of_week <- (lubridate::wday(dd$date) - 1) * 24 * 60 +
    lubridate::hour(dd$date) * 60 + lubridate::minute(dd$date)

  interval_of_week <- 1 + floor(minute_of_week / 10)
  ftow=factor(interval_of_week)
  uniq_time_of_week <- unique(interval_of_week)
  time_of_week_rows <- length(uniq_time_of_week)

  temp_col_18 <- temp_col - 18
  temp_col_18[temp_col > 18] <- 0
  temp_col_23 <- temp_col - 23
  temp_col_23[temp_col < 23] <- 0

  #régression avec deux températures de coupures : <18°C (chauff) et >23°C (clim)
  amod <- stats::lm(eload_col ~ temp_col_18 + temp_col_23, na.action = stats::na.exclude)

  #on regarde si le nombre de résidus positif d'un pas de temps (surprédiction) est supérieur à 65% du temps
  ok_occ <- rep(0, time_of_week_rows)
  for (row_index in 1 : time_of_week_rows) {
    ok_time_of_week <- interval_of_week == uniq_time_of_week[row_index]
    if (!is.na(sum(stats::residuals(amod)[ok_time_of_week] > 0, na.rm = TRUE) >
         0.55 * sum(ok_time_of_week))){
    if (sum(stats::residuals(amod)[ok_time_of_week] > 0, na.rm = TRUE) >
        0.55 * sum(ok_time_of_week)) {
      ok_occ[row_index] <- 1
    }}
  }
  dd$occup=ok_occ[match(interval_of_week,uniq_time_of_week)]

  dd_j <- aggregation(dd,"Jour")
  dd_j <- dd_j[dd_j$occup>0,]
  #on fait une régression linéaire et si celle si surprédit de 65% la consommation réelle alors c'est fermé
  if (nrow(dd_j)>0){
  test<-stats::lm(data=dd_j,energie~Temperature)
  dd_j$occj=ifelse(stats::residuals(test)< -0.65*dd_j$energie,0,1)
  dd_j=dd_j[dd_j$occj==0,]
  dd[which(!is.na(match(lubridate::floor_date(dd$date,"day"),dd_j$date))),"occup"]=0
  dd[which(!is.na(match(lubridate::floor_date(dd$date,"day"),dd_j$date))),"ferie"]=1
  }


  return(dd)

}
