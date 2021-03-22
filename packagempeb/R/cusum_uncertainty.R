#' Calcul l'incertitude type du modèle
#'
#' @param dd jeu de données avec a minima une variable, une colonne energie, une colonne prédiction et une variable indépendante
#' @param date_deb_ref date de début de la période de référence
#' @param date_fin_ref date de fin de la période de référence
#' @param econ économie attendue en pourcentage
#'
#' @return l'incertitude selon la méthode hat matrix écrite dans la FDX30-148
#' @export
#'
incertitude <- function(dd,date_deb_ref,date_fin_ref,econ){
  dd.suiv=subset(dd,date<date_deb_ref|date>date_fin_ref)
  dd.ref=subset(dd,date>=date_deb_ref&date<=date_fin_ref)
  for (i in c(ncol(dd.ref):3)){
    if (sum(dd.ref[,i])==0){
      dd.ref=dd.ref[,-i]
      dd.suiv=dd.suiv[,-i]
    }
  }

  dd.ref$energie=dd.ref$energie*(1-econ/100)
  col_rem=which(colnames(dd.ref)=="prediction")
  design_matrix_ref=as.matrix(cbind(c(rep(1, nrow(dd.ref))),dd.ref[,-c(1,2,col_rem)]))
  design_matrix_suiv=as.matrix(cbind(c(rep(1, nrow(dd.suiv))),dd.suiv[,-c(1,2,col_rem)]))

  hat_matrix=design_matrix_suiv%*%solve(t(design_matrix_ref)%*%design_matrix_ref)
  hat_matrix2=design_matrix_ref%*%solve(t(design_matrix_ref)%*%design_matrix_ref)
  RMSE=sqrt(sum((dd.ref$prediction-dd.ref$energie)^2)/(nrow(dd.suiv)-ncol(dd.suiv)+2))

  # if (dd$date[2]-dd$date[1]<minutes(30)){
  hat_matrix=colSums(t(hat_matrix)*t(design_matrix_suiv))
  U_stepbis=RMSE*(sqrt(1+hat_matrix))
  hat_matrix2=colSums(t(hat_matrix2)*t(design_matrix_ref))
  U_stepbis2=RMSE*(sqrt(1+hat_matrix2))
  # }else{
  # hat_matrix=hat_matrix%*%t(design_matrix_suiv)
  # U_stepbis=RMSE*(sqrt(1+colSums(hat_matrix)))
  # hat_matrix2=hat_matrix2%*%t(design_matrix_ref)
  # U_stepbis2=RMSE*(sqrt(1+colSums(hat_matrix2)))
  # }

  S=sqrt(sum((U_stepbis2)^2)/length(U_stepbis2))
  Utotbis=round(sqrt(sum((U_stepbis2)^2))/sum(dd.ref$energie)*100,2)
  Urep=data.frame(Ufdx=c(U_stepbis2,U_stepbis),S=c(S,rep("",length(U_stepbis)+length(U_stepbis2)-1)),Utotbis=c(Utotbis,rep("",length(U_stepbis)+length(U_stepbis2)-1)),date=c(dd.ref$date,dd.suiv$date), Utot=c(round(sqrt(sum((U_stepbis2)^2)),2),rep("",length(U_stepbis)+length(U_stepbis2)-1)))

  return(Urep)
}

#' Calcul la carte de controle cusum pour analyser les dérives lentes
#'
#' @param dd jeu de données avec à minima les colonnes date, energie et prediction
#' @param date_deb_ref date de début de la période de référence
#' @param date_fin_ref date de fin de la période de référence
#' @param incertit erreur type : incertitude du modèle divisée par la racine du nombre de valeurs
#'
#' @return un dataframe avec les dates, limites de la carte de contrôle et les valeurs SI+ et SI- de la CUSUM
#' @export
#'
cusum<-function(dd,date_deb_ref,date_fin_ref,incertit){
  dd.suiv=subset(dd,date<date_deb_ref|date>date_fin_ref)
  dd.ref=subset(dd,date>=date_deb_ref&date<=date_fin_ref)
  sigma=incertit
  delta1=1;
  k=delta1/2;
  if (dd$date[2]-dd$date[1]<hours(1)){
    POM1=1000
    }else{
      if(dd$date[2]-dd$date[1]<days(1)){
        POM1=100
    }else{
      POM1=10
    }
  }
  b=k*POM1+1/(2*k);
  POM0=((exp(2*b*k)-2*b*k-1)/(2*k^2))/2
  h=b-1.166;
  ecart_conso=dd.suiv$prediction-dd.suiv$energie
  z=ecart_conso/sigma
  SIplus=rep(0,nrow(dd.suiv))
  SImoins=rep(0,nrow(dd.suiv))
  for (i in seq(from=1, to=nrow(dd.suiv))){
    if (i==1){
      SIplus[i]=max(0,0+z[i]-k)
      SImoins[i]=min(0,0+z[i]+k)
    }else{
      SIplus[i]=max(0,SIplus[i-1]+z[i]-k)
      SImoins[i]=min(0,SImoins[i-1]+z[i]+k)
    }
  }
  SI<-data.frame(date=dd.suiv$date,h_moins=-h,SImoins,SIplus,h_plus=h)
  return(SI)
}
