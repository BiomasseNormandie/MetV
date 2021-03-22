#' Fonction qui permet d'aggréger un jeu de données avec une colonne date, en un jeu de donnée avec un pas de temps plus grand
#'
#' @param dd dataset a aggréger
#' @param form format pour l'aggrégation qui doit être parmis : "Jeu de données", "Heure", "Jour", "Mois"
#'
#' @return le dataset aggrégé
#' @export
#' @import dplyr
#' @import lubridate
#'

aggregation<-function(dd,form){
  var=0
  if (length(which(colnames(dd)=="Temperature"))==0){
    dd$Temperature=0
    var=1
  }
  if (form=="Jeu de donnees"){
    dd2=dd
  }else{
    if (form=="Heure"){
    dd2 = dd %>%
      dplyr::group_by(date_1=lubridate::floor_date(date,"hour"))%>%
      dplyr::summarise_at(vars(-c(1,which(colnames(dd)=="Temperature"))),sum)%>%
      merge(dd %>%dplyr::group_by(date_1=lubridate::floor_date(date,"hour"))%>%dplyr::summarise_at(vars("Temperature"),mean))
  }else{
    if (form=="Jour"){
    dd2 = dd %>%
      dplyr::group_by(date_1=lubridate::floor_date(date,"day"))%>%
      dplyr::summarise_at(vars(-c(1,which(colnames(dd)=="Temperature"))),sum)%>%
      merge(dd %>%dplyr::group_by(date_1=lubridate::floor_date(date,"day"))%>%dplyr::summarise_at(vars("Temperature"),mean))
  }else{
    dd2 = dd %>%
      dplyr::group_by(date_1=lubridate::floor_date(date,"month"))%>%
      dplyr::summarise_at(vars(-c(1,which(colnames(dd)=="Temperature"))),sum)%>%
      merge(dd %>%dplyr::group_by(date_1=lubridate::floor_date(date,"month"))%>%dplyr::summarise_at(vars("Temperature"),mean))
  }}}
  colnames(dd2)[1]="date"
  if (var==1){
    dd2=subset(dd2,select=-Temperature)
  }
  return(dd2)
}

