#' Renvoie les statistiques du modèle
#'
#' @param dd jeu de données
#' @param date_deb_ref date de début de la période de référence
#' @param date_fin_ref date de fin de la période de référence
#' @param reg objet régression de R qui provient de la fonction model
#' @param var variable qui vaut 0 si le calcul du modèle n'a pas été fait, 1 sinon
#' @param econ économie attendue à la suite d'une AAPE
#'
#' @return Un tableau avec les paramètre RMSE, CVRMSE, r², r² ajusté et p-value du modèle
#' @export
#'

statistiques<-function(dd,date_deb_ref,date_fin_ref,reg,var,econ){
  dd.bis=subset(dd,date>=date_deb_ref&date<=date_fin_ref)
  dd.bis$energie=dd.bis$energie*(1-econ/100)
  residus=dd.bis$prediction-dd.bis$energie
  SST=sum((dd.bis$energie-mean(dd.bis$energie))^2)
  RMSE=sqrt(sum(residus^2)/(nrow(dd.bis)-ncol(dd.bis)+2))
  CVRMSE=paste0(round(RMSE/mean(dd.bis$energie)*100,2),"%")
  r2=1-(sum(residus^2)/SST)
  r2_ajust=paste0(round((1-((1-r2)*(nrow(dd.bis)-1))/(nrow(dd.bis)-ncol(dd.bis)+2))*100,2),"%")
  r2=paste0(round(r2*100,2),"%")
  if (is.null(reg)){
    pvalue="pas calculable"
  }else{
    f=summary(reg)$fstatistic
    pvalue=ifelse(is.null(length(f)),"pas calculable",stats::pf(f[1],f[2],f[3],lower.tail=F))
  }
  if(var==1){
    qualite_model=data.frame(RMSE,CVRMSE,r2,r2_ajust,pvalue)
  }else{
    qualite_model=NULL
  }
  return(qualite_model)
}

#' Renvoie les stats p de toutes les variables
#'
#' @param reg objet régression de R qui provient de la fonction model
#' @param var variable qui vaut 0 si le calcul du modèle n'a pas été fait, 1 sinon
#'
#' @return un dataframe avec toutes les p-values de toutes les variables du modèle
#' @export
#'

stat_p<-function(reg,var){
  if(var==1){
    res=summary(reg)$coefficients[,1]/summary(reg)$coefficients[,2]
    tvalues=data.frame(variable=names(res),stat_t=res)
    #pvalues=as.data.frame(t(summary(reg)$coefficients[,4]))
    #tvalues_t=data.frame(coef(summary(reg))[, "t value"])
    #pvalues=data.frame(variable=colnames(pvalues),p_value=t(pvalues[1,]),t_value=t(tvalues[1,]))
    #pvalues=data.frame(variables=tvalues$variable,t_value=tvalues$stat_t,t_value_t=tvalues_t)

    #colnames(pvalues)[2]="p_values (critere <0,05)"
    #colnames(pvalues)[3]="stat_t (critere : >2)"
  }else{
    #pvalues=NULL
    tvalues=NULL
  }
  return(tvalues)
}
