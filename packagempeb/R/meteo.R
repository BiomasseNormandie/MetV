#' Sélectionne les stations en activité durant la période du jeu de données
#'
#' @param stations liste des stations
#' @param date_deb première date du jeu de données
#' @param date_fin dernière date du jeu de données
#'
#' @return un dataframe avec les stations qui étaient en activité durant la plage de temps du jeu de données
#' @export
#'
select_station<-function(stations,date_deb,date_fin){
  stations$BEGIN<-as.Date(as.character(stations$BEGIN),"%Y%m%d")
  stations$END<-as.Date(as.character(stations$END),"%Y%m%d")
  stations<-stations[stations$BEGIN<=date_deb & stations$END>=date_fin,]
  return(stations)
}

#' Filtrage des stations par le pays sélectionné dans l'interfaces
#'
#' @param stations liste des stations
#' @param pays pays qui provient de l'interface shiny
#'
#' @return stations météorologiques actives dans le pays sélectionné lors de la plage de date du jeu de données
#' @export
#'
select_ss_station <-function(stations,pays){
  stations<-stations[stations$CTRY==list_country[list_country$COUNTRY.NAME==pays,1],]
}

#' Téléchargement des données à partir du serveur ftp du NOAA, format ISD lite
#'
#' @param dd jeu de données avec à minima une colonne date
#' @param station station sélectionnée à partir de l'interface shiny
#'
#' @return un jeu de données avec les dates en première colonne et les températures correspondantes
#' @export
#'
#' @import zoo
aspi_noaa<-function(dd,station){
  a=list()
  for (annee in c(min(year(dd$date)):max(year(dd$date)))){
  fn=paste0("ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-lite/",annee,"/",station,"-99999-",annee,".gz")
  utils::download.file(fn,destfile="tmp.tar.gz")
  a=rbind(a,utils::read.fwf(gzfile("tmp.tar.gz"),header=F,c(4,3,3,3,6,6,6,6,6,6,6,6)))
  }
  colnames(a) = c("year", "month", "day", "hour", "Temperature", "Dew_Point", "Pressure","Wind_D", "Wind_S",  "Sky", "Prec_1h", "Prec_6h")
  file.remove("tmp.tar.gz")
  a$Temperature[a$Temperature == -9999] = NA
  # a$Dew_Point[a$Dew_Point == -9999] = NA
  # a$Wind_S[a$Wind_S == -9999] = NA
  # a$Wind_D[a$Wind_D == -9999] = NA
  # a$Pressure[a$Pressure == -9999] = NA
  # a$Sky[a$Sky == -9999] = NA
  a[,c(5:7,9,11,12)]=a[,c(5:7,9,11,12)]/10
  if (requireNamespace("zoo", quietly=TRUE)) {
    a$Temperature= zoo::na.approx(a$Temperature)
  } else {
    warning("Would need zoo package")  # message optional
  }
  # a$Dew_Point= na.approx(a$Dew_Point)
  # a$Wind_S= na.approx(a$Wind_S)
  # a$Wind_D= na.approx(a$Wind_D)
  # a$Pressure= na.approx(a$Pressure)
  a$date = ISOdatetime(year = a$year, month = a$month, day = a$day, hour = a$hour, 0, 0, tz = "UTC")
  attr(a$date, "tzone") <- "Europe/Paris"
  a=a[,c("date","Temperature")]
  if (length(which(colnames(dd)=="Temperature"))>0){
  dd=dd[,-which(colnames(dd)=="Temperature")]
  }
  if (dd$date[2]-dd$date[1]==days(1)){
    a=packagempeb::aggregation(a,"Jour")
  }else{
    if (dd$date[2]-dd$date[1]==months(1)){
      a=packagempeb::aggregation(a,"Mois")
  }}
  dd=left_join(dd,a,by="date")
  dd$Temperature=zoo::na.approx(dd$Temperature,rule=2)
  return(dd)
}
