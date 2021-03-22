#' Modélise la consommation énergétique
#'
#'
#' @param dd jeu de données avec au moins une colonne date et une colonne énergie
#' @param model_type modèle statistique à appliquer au choix entre "Regression simple", "Regression par morceaux" et "TOWT"
#' @param date_deb_ref date de début de la période de référence
#' @param date_fin_ref date de fin de la période de référence
#' @param econ économie attendue avec la réalisation de l'AAPE
#'
#' @return une liste comprenant le jeu de données d'entrée auquel on a rajouté une colonne prédiction et un objet correspondant à la régression qui a été réalisée
#' @export
#'
#' @import lubridate
#'
modelisation<-function(dd,model_type,date_deb_ref,date_fin_ref,econ) {
  energie_temp=dd$energie
  dd$energie=dd$energie*(1-econ/100)
  dd.res=dd
  resultat=list()

  if (model_type=="Regression simple"){
    regression<-stats::lm(energie~.,subset(dd,date>as.POSIXct(date_deb_ref)&date<=as.POSIXct(date_fin_ref),select=-date))
    dd.res$prediction<-stats::predict(regression,dd)
  }else{
    trame=matrix(0,nrow=length(dd$Temperature),ncol=6)
    temp_knots=c(-100,10,15,20,25,30,100)
    colnames(trame)=levels(cut(dd$Temperature,breaks=temp_knots))
    Temp_mat=cbind(dd$Temperature,cut(dd$Temperature,breaks=temp_knots,labels=FALSE),trame)
    for (i in c(3:8)){
      Temp_mat[,i]=ifelse(Temp_mat[,2]+2<i,0,ifelse(Temp_mat[,2]+2==i,Temp_mat[,1]-temp_knots[i-2],temp_knots[3]-temp_knots[2]))
    }
    Temp_mat<-as.data.frame(Temp_mat[,c(3:8)])
    Temp_mat$date=as.POSIXct(dd$date)
    dd=data.frame(dd,Temp=Temp_mat[,c(1:6)])
    if (model_type=="Regression par morceaux") {
      regression<-stats::lm(energie~.,subset(dd,date>as.POSIXct(date_deb_ref)&date<=as.POSIXct(date_fin_ref),select=-c(date,Temperature)))
      dd.res$prediction<-stats::predict(regression,dd)
    } else {
      dd_j=aggregation(dd,"Jour")
      minute_of_week <- (lubridate::wday(dd$date) - 1) * 24 * 60 +
        lubridate::hour(dd$date) * 60 + lubridate::minute(dd$date)
      interv=lubridate::minute(lubridate::minutes(dd$date[2])-lubridate::minutes(dd$date[1]))
      interval_of_week <- 1 + floor(minute_of_week / 10)
      ftow=factor(interval_of_week)
      uniq_time_of_week <- unique(interval_of_week)
      time_of_week_rows <- length(uniq_time_of_week)
      dd$ftow=as.numeric(ftow)
      ###gestion des feriés et weekend
      dd[dd$ferie>0,"ftow"]=rep(seq(from=10000,to=10000+which(dd$date==date(dd$date[1])%m+%days(1))-2),nrow(dd_j[dd_j$ferie>0,]))
      ###regressions
      dd.inocc=subset(dd,occup==0,select=c(colnames(dd[-c((ncol(dd)-6):ncol(dd)-1)])))
      dd.inocc$ftow=as.factor(dd.inocc$ftow)

      dd.occ=subset(dd,occup>0,select=-c(Temperature,occup))
      dd.occ$ftow=as.factor(dd.occ$ftow)

      dd.res$prediction=0

      if (nrow(dd.inocc)!=0){
        regression=stats::lm(energie~.,subset(dd.inocc,date>as.POSIXct(date_deb_ref)&date<=as.POSIXct(date_fin_ref),select=-date))
        dd.res$prediction[dd.res$occup==0]=stats::predict(regression,dd.inocc)
      }
      regression_2=NULL
      if (nrow(dd.occ)!=0){
        regression_2=stats::lm(energie~.,subset(dd.occ,date>as.POSIXct(date_deb_ref)&date<=as.POSIXct(date_fin_ref),select=-date))
        dd.res$prediction[dd.res$occup>0]=stats::predict(regression_2,dd.occ)
      }
      ###doit gérer les jours feriés
      resultat$regression2=regression_2

    }
  }
  dd.res$energie=energie_temp
  resultat$df=dd.res
  resultat$regression=regression
  if (model_type=="TOWT"){
    resultat$ftow=dd$ftow
  }else{resultat$ftow=NULL}

  return(resultat)
}
