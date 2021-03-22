###Librairies, qui doivent être installées au préalable pour une utilisation en local###----
if (requireNamespace("shinythemes", quietly=TRUE)) {
  require(shinythemes)
} else {
  install.packages("shinythemes")
  require(shinythemes)
}
if (requireNamespace("shinybusy", quietly=TRUE)) {
  require(shinybusy)
} else {
  install.packages("shinybusy")
  require(shinybusy)
}
if (requireNamespace("shinycssloaders", quietly=TRUE)) {
  require(shinycssloaders)
} else {
  install.packages("shinycssloaders")
  require(shinycssloaders)
}
if (requireNamespace("shinydashboard", quietly=TRUE)) {
  require(shinydashboard)
} else {
  install.packages("shinydashboard")
  require(shinydashboard)
}
if (requireNamespace("shinyalert", quietly=TRUE)) {
  require(shinyalert)
} else {
  install.packages("shinyalert")
  require(shinyalert)
}
if (requireNamespace("shinyjs", quietly=TRUE)) {
  require(shinyjs)
} else {
  install.packages("shinyjs")
  require(shinyjs)
}
if (requireNamespace("DT", quietly=TRUE)) {
  require(DT)
} else {
  install.packages("DT")
  require(DT)
}
if (requireNamespace("plotly", quietly=TRUE)) {
  require(plotly)
} else {
  install.packages("plotly")
  require(plotly)
}
if (requireNamespace("ggplot2", quietly=TRUE)) {
  require(ggplot2)
} else {
  install.packages("ggplot2")
  require(ggplot2)
}
if (requireNamespace("writexl", quietly=TRUE)) {
  require(writexl)
} else {
  install.packages("writexl")
  require(writexl)
}
if (requireNamespace("leaflet", quietly=TRUE)) {
  require(leaflet)
} else {
  install.packages("leaflet")
  require(leaflet)
}
if (requireNamespace("lubridate", quietly=TRUE)) {
  require(lubridate)
} else {
  install.packages("lubridate")
  require(lubridate)
}
require(packagempeb)

###UI = User Interface, c'est la partie mise en forme###----
ui <-dashboardPage(
  title="MPEB",skin="green",
  dashboardHeader(title=span(img(src="mpeb_trans.png",style="height:50px;padding-bottom:5px;")),
                  tags$li(a(img(src="logo.png",style="height:50px;"),style="padding-top: 0px;padding-bottom: 0px;"),
                          class = "dropdown",
                  )),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      menuItem(text="Accueil/Home",tabName="home",icon=icon("home")),
      menuItem(text="Données/Data",tabName="data",icon=icon("upload")),
      menuItem(text="Température et occupation",tabName="temp_occ", icon=icon("cloud-sun")),
      menuItem(text="Période de référence/Baseline",tabName="baseline", icon=icon("chart-area")),
      menuItem(text="Modélisation",tabName="model", icon=icon("laptop-code")),
      menuItem(text="Résultats/Results",tabName="result", icon=icon("chart-pie")),
      menuItem(text="Export",tabName="export", icon=icon("download"))
    )
  ),
  dashboardBody(
    includeCSS("www/style.css"),
    withMathJax(),
    useShinyalert(),
    tabItems(
      ####page1----
      tabItem(tabName = "home",
              column(width=1),
              column(width=10,
                     HTML("Cet outil a été développé dans le cadre du projet MPEB par <b><a href='https://fr.linkedin.com/in/quentin-giffard-8b9419b9' style='color:#00a65a'>Quentin Giffard (CMVP)</a></b> et supervisé par <b><a href='https://fr.linkedin.com/in/paul-calberg-ellen-9aab0161' style='color:#00a65a'>Paul Calberg-Ellen (CMVP et formateur IPMVP)</a></b> de l'Association Régionale Biomasse Normandie ."),
                     tags$p("Il fonctionne avec la méthodologie suivante :"),
                     tags$li("je charge mes données"),
                     tags$li("je visualise mes données et je définis ma période de référence"),
                     tags$li("je sélectionne le type de modèle : régression multivariable à pente simple, multiple changement de pentes, TOWT"),
                     tags$li("je visualise mes résultats en terme d'économie et d'éventuelles dérives"),
                     tags$li("j'exporte mes résultats en fichier Excel"),
                     fluidRow(),
                     hr(),
                     tags$p(HTML("Ce package est sous licence <b><a href='http://www.gnu.org/licenses/gpl-3.0.html' style='color:#00a65a'>CeCILL</a></b>, il dépend des packages suivants :")),
                     tags$li(HTML("Pour le traitement de données : <a href='https://cran.r-project.org/web/packages/lubridate/index.html' style='color:#00a65a'>lubridate</a>,
                               <a href='https://cran.r-project.org/web/packages/dplyr/index.html' style='color:#00a65a'>dplyr</a>,
                               <a href='https://cran.r-project.org/web/packages/zoo/index.html' style='color:#00a65a'>zoo</a>")),
                     tags$li(HTML("Pour l'interface :
                          <a href='https://cran.r-project.org/web/packages/shiny/index.html' style='color:#00a65a'>shiny</a>,
                          <a href='https://cran.r-project.org/web/packages/shinythemes/index.html' style='color:#00a65a'>shinythemes</a>,
                          <a href='https://cran.r-project.org/web/packages/shinyWidgets/index.html' style='color:#00a65a'>shinyWidgets</a>,
                          <a href='https://cran.r-project.org/web/packages/shinydashboard/index.html' style='color:#00a65a'>shinydashboard</a>,
                          <a href='https://cran.r-project.org/web/packages/shinybusy/index.html' style='color:#00a65a'>shinybusy</a>,
                          <a href='https://cran.r-project.org/web/packages/shinycssloaders/index.html' style='color:#00a65a'>shinycssloaders</a>,
                          <a href='https://cran.r-project.org/web/packages/shinyjs/index.html' style='color:#00a65a'>shinyjs</a>")),
                     tags$li(HTML("Pour la visualisation des données :
                               <a href='https://cran.r-project.org/web/packages/DT/index.html' style='color:#00a65a'>DT</a>,
                               <a href='https://cran.r-project.org/web/packages/ggplot2/index.html' style='color:#00a65a'>ggplot2</a>,
                               <a href='https://cran.r-project.org/web/packages/leaflet/index.html' style='color:#00a65a'>leaflet</a>")),
                     tags$li(HTML("Pour l'export au format xlsx :
                               <a href='https://cran.r-project.org/web/packages/writexl/index.html' style='color:#00a65a'>writexl</a>,"))
              )),
      ####page2----
      tabItem(tabName = "data",
              column(width=4,
                     HTML("Importez votre jeu de données <br> <b> <i class='fa fa-exclamation-triangle' style = 'color:#00a65a; font-size: 1.3em;'></i>  Attention : </b> le format doit être le suivant <li> première colonne : date (dd/mm/YYYY HH:MM) <i class='fa fa-hand-point-right' style = 'color:#00a65a; font-size: 1.3em;'></i> </li><li> deuxième colonne : consommations ou charges </li> <li> autres colonnes : autres variables indépendantes </li>")
                     ),
              column(width=4,
                     img(src="excel.png",style="max-width: 100%;max-height: 20em")
              ),
              column(width=4,
                     wellPanel(
                       HTML("<i class='fa fa-database' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       fileInput("FileInput", "Sélectionnez un fichier (.csv)"))),
              fluidRow(),
              DTOutput("tbl")%>%withSpinner(size=0.5,type=6,color="#00a65a")
      ),
      ####page3----
      tabItem(tabName = "temp_occ",
              column(width=6,
                     wellPanel(
                       HTML("<i class='fa fa-temperature-low' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       selectInput("country","Choisissez le pays où se trouve votre batiment ",choices=sort(list_country$COUNTRY.NAME)),
                       leafletOutput("map", height="200px"),
                       selectInput("station","Choisissez la station météo la plus proche ",choices=NULL),
                       actionButton("calcul_temp",span("Détermination des températures",id="UpdateAnimate1",class="")))),
              column(width=6,
                     wellPanel(
                       HTML("<i class='fa fa-users-cog' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       tags$p("Pour calculer l'occupation il faut que vous ayez un colonne 'Temperature' au préalable."),
                       actionButton("calcul_occ",span("Détermination de l'occupation",id="UpdateAnimate2",class=""))),
                     wellPanel(
                       HTML("<i class='fa fa-ruler' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       tags$p("Pour calculer les DJU il faut que vous ayez un colonne 'Temperature' au préalable."),
                       tags$p("La méthode de calcul des DJU est la suivante : max(18-moyenne des températures,0), la moyenne des températures est déterminée à partir des données horaires."),
                       actionButton("calcul_dju",span("Détermination des DJU",id="UpdateAnimate3",class="")))),
              fluidRow(),
              tags$p("Attendez que la colonne 'Temperature' et/ou 'occup' soit ajouter au tableau avant de continuer."),
              DTOutput("tbl2")%>%withSpinner(size=0.5,type=6,color="#00a65a")
      ),
      ####page4----
      tabItem(tabName = "baseline",
              column(width=6,
                     wellPanel(
                       HTML("<i class='fa fa-search' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       dateRangeInput('dateRange',
                                      label = "Sélectionnez la période à afficher (attention l'affichage de beaucoup de points peut prendre du temps)",
                                      start = dataset$date[1], end = dataset$date[1000]))),
              column(width=6,
                     wellPanel(
                       HTML("<i class='fa fa-cogs' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       selectInput("periode","Sélectionnez le pas de temps du modèle : ",choices=c("Jeu de donnees","Heure","Jour","Mois"),selected="Jeu de donnees"),
                       dateRangeInput('dateRef',
                                      label = "Sélectionnez la période de référence, l'outil permet aussi bien l'ante-post que le post-ante",
                                      start = dataset$date[1], end = dataset$date[1] %m+% years(1) %m-% days(1)))),
              fluidRow(),
              plotlyOutput("graph_ref")%>%withSpinner(size=0.5,type=6,color="#00a65a")
      ),
      ####page5----
      tabItem(tabName = "model",
              column(width=6,
                     # actionButton("valeur_ab","Retrait des valeurs aberrantes (<= 0) ?"),
                     wellPanel(
                       HTML("<i class='fa fa-calculator' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       sliderInput("economie", label = "Pourcentage d'économies attendu :",min = -100, max = 100, value = 0),
                       tags$p("Pour pouvoir avoir accès au modèle à multiples pentes il faut à minima la variable 'Temperature', pour avoir accès au TOWT il faut à la fois la 'Temperature','occup' et 'ferie'."),
                       actionButton("model_dispo","Rafraichir les modèles disponibles"),
                       selectInput("model_type","Quel type de modèle souhaitez vous appliquer ?",choices=c("Regression simple"),selected="Regression simple"),
                       hr(),
                       checkboxGroupInput(inputId = "variables",label = "Sélection des variables pour la régression",choices = colnames(dataset)[3:ncol(dataset)],selected=colnames(dataset)[3:ncol(dataset)]),
                       actionButton("calcul",span("Calculer le modèle",id="UpdateAnimate3",class="")))),
              column(width=6,
                     wellPanel(
                       HTML("<i class='fa fa-poll' style = 'color:#00a65a; font-size: 1.3em;'></i>"),
                       tags$p("Vous trouverez ci-dessous les principaux coefficients statistiques du modèle :"),
                       htmlOutput("equation"),
                       tableOutput("table_reg"),
                       tableOutput("table_p"),
                       p(htmlOutput("uncert", inline = TRUE)),
                       div(
                         id = "tooltip", style = "display: none;",
                         HTML("$$\\int_0^1 f(x) dx = \\pi$$")))),
              hr()
      ),
      ####page6----
      tabItem(tabName = "result",
              column(width=6,
                     wellPanel(
                       dateRangeInput('date_ref',
                                      label = HTML("Sélectionnez la plage de la période de référence à afficher <br> (<i class='fa fa-exclamation-triangle' style = 'color:#00a65a;'></i>  attention l'affichage de beaucoup de points peut prendre du temps)"),
                                      start = NULL, end = NULL),
                       plotlyOutput("graph_ref2")%>%withSpinner(size=0.5,type=6,color="#00a65a"),
                     )),
              column(width=6,
                     wellPanel(
                       dateRangeInput('date_model',
                                      label = HTML("Sélectionnez la plage de la période de suivi à afficher <br> (<i class='fa fa-exclamation-triangle' style = 'color:#00a65a;'></i>  attention l'affichage de beaucoup de points peut prendre du temps)"),
                                      start = NULL, end = NULL),
                       plotlyOutput("graph_model")%>%withSpinner(size=0.5,type=6,color="#00a65a"),
                     )),
              fluidRow(),
              wellPanel(
                column(width=6,
                      #plotlyOutput("graph_eco")%>%withSpinner(size=0.5,type=6,color="#00a65a")),
                      plotlyOutput("graph_eco_ann")%>%withSpinner(size=0.5,type=6,color="#00a65a")),
                column(width=6,
                       plotlyOutput("graph_cusum")%>%withSpinner(size=0.5,type=6,color="#00a65a")),
                fluidRow()
              ),
              fluidRow(),
              column(width=6,
                     plotlyOutput("heatmap")%>%withSpinner(size=0.5,type=6,color="#00a65a")),
              column(width=6,
                     plotlyOutput("heatmap2")%>%withSpinner(size=0.5,type=6,color="#00a65a")),
              tags$p(".")
      ),
      ####page7----
      tabItem(tabName = "export",
              downloadButton("dl", "Télécharger l'analyse")
      )
    )
  )
)

####Serveur c'est la partie backend----
server <- function(input, output, session) {
  options(shiny.maxRequestSize=10*1024^2) #augmentation de la taille max des fichiers importés
  addCssClass(selector = "a[data-value='result']", class = "inactiveLink")
  addCssClass(selector = "a[data-value='export']", class = "inactiveLink")
  disable("calcul_occ")
  disable("calcul_dju")

  updateSelectInput(session,"country",choices=sort(list_country[,"COUNTRY.NAME"]),selected="FRANCE")
  updateDateRangeInput(session,'dateRange',
                       start = dataset[1,"date"], end = dataset[1000,"date"])
  updateDateRangeInput(session,'dateRef',
                       start = dataset[1,"date"], end = dataset[1,"date"] %m+% years(1) %m-% days(1))
  updateDateRangeInput(session,'date_ref',
                       start = dataset[1,"date"], end = min(dataset$date[which(dataset$date==dataset[1,"date"] %m+% years(1) %m-% days(1))],dataset[min(1000,nrow(dataset)),"date"]))
  updateDateRangeInput(session,'date_model',
                       start = dataset[1,"date"] %m+% years(1), end = min(dataset[which(dataset$date==(dataset[1,"date"] %m+% years(1)))+1000,"date"],dataset[nrow(dataset),"date"]))
  updateCheckboxGroupInput(session,inputId = "variables",label = "Selection des variable pour la regression",choices = colnames(dataset)[3:ncol(dataset)],selected=colnames(dataset)[3:ncol(dataset)])

  #liste_station<-read.csv("https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.csv")
  datas<-reactiveValues(df=dataset,df_rel=dataset,df_agg=dataset,df_bis=dataset)#initialisation du data set avec le jeu préchargé
  regression<-reactiveValues()
  regression$reg<-NULL
  regression$calc<-0
  agg_j<-reactiveValues(df=NULL)
  ####chargement des données page 2----
  observeEvent(input$FileInput, {
    infile <- input$FileInput
    if(is.null(infile)) {
      return(FALSE)
    }else{
      addCssClass(selector = "a[data-value='result']", class = "inactiveLink")
      addCssClass(selector = "a[data-value='export']", class = "inactiveLink")

      data.load=read.csv2(infile$datapath, header = TRUE, encoding="UTF-8")
      colnames(data.load)[c(1,2)]=c("date","energie")
      test1<-try(
        if (nchar(data.load$date[1])>10){
          data.load$date<-as.POSIXct(data.load$date,format="%d/%m/%Y %H:%M")
        }else{
          data.load$date<-as.POSIXct(data.load$date,format="%d/%m/%Y")
        })
      if (!inherits(test1,"try-error")){
        data.load<-data.load[!is.na(data.load$date),]
        datas$df<-data.load
        datas$df_rel<-data.load
        datas$df_agg<-data.load
        datas$df_bis<-data.load
        var_test<-0
        if (length(which(colnames(datas$df)=="Temperature"))==1){
          enable("calcul_occ")
          enable("calcul_dju")
        }else{
          disable("calcul_occ")
          disable("calcul_dju")

        }
        updateDateRangeInput(session,"dateRange",start=datas$df[1,"date"],end=as.Date(ifelse(nrow(datas$df)>1000,datas$df[1000,"date"],datas$df[nrow(datas$df),"date"]),origin="1970-01-01"))
        updateDateRangeInput(session,"dateRef",start=datas$df[1,"date"],end=min(datas$df[nrow(datas$df),"date"],datas$df[1,"date"] %m+% years(1) %m-% days(1)))
        updateDateRangeInput(session,"date_model",start=datas$df[1,"date"] %m+% years(1),end= min(datas$df[min(nrow(datas$df),1000),"date"],datas$df[1,"date"] %m+% years(2) %m-% days(1)))
        updateDateRangeInput(session,"date_ref",start=datas$df[1,"date"],end=min(datas$df[min(nrow(datas$df),1000),"date"],datas$df[1,"date"] %m+% years(1) %m-% days(1)))

      }else{
        shinyalert("Oops!", "Il semblerait que la mise en page du fichier ne soit pas compatible. Pour rappel la première colonne doit comporter les dates au format : JJ/MM/AAAA HH:MM:SS.", type = "error",confirmButtonCol = "#00a65a")
      }

      return(datas)}
  })

  output$tbl <- renderDT(datas$df,editable=T,options = list(
    pageLength = 10,scrollX=TRUE))
  proxy = dataTableProxy('tbl')

  ####détermination des températures et occupations page 3----
  observeEvent(input$calcul_temp, {
    show_modal_spinner(spin="semipolar",color="#00a65a", text=HTML("<br>Veuillez attendre que le téléchargement soit réalisé"))
    shinyjs::disable("calcul_temp")
    test<-try(aspi_noaa(datas$df,liste_station[liste_station$STATION.NAME==input$station,"USAF"]))
    if (!inherits(test,"try-error")){
      datas$df_bis=test
      enable("calcul_occ")
      enable("calcul_dju")

    }else{
      shinyalert("Oops!", "Il semblerait que la station sélectionnée ne contienne pas toutes les données. Nous vous invitons à sélectionner une autre station.", type = "error",confirmButtonCol = "#00a65a")
    }
    datas$df_agg<-datas$df_bis
    shinyjs::enable("calcul_temp")
    remove_modal_spinner()
  })

  observeEvent(input$calcul_occ, {
    show_modal_spinner(spin="semipolar",color="#00a65a", text=HTML("<br>Nous calculons l'occupation et les jours fériés"))
    shinyjs::disable("calcul_occ")
    datas$df_bis<<-occupation(datas$df_bis)
    datas$df_agg<-datas$df_bis
    shinyjs::enable("calcul_occ")
    remove_modal_spinner()
  })

  observeEvent(input$calcul_dju, {
    show_modal_spinner(spin="semipolar",color="#00a65a", text=HTML("<br>Les DJU sont en cours de calcul"))
    shinyjs::disable("calcul_dju")
    temp=aggregation(datas$df_bis,"Jour")
    temp$DJU=ifelse(temp$Temperature<18,18-temp$Temperature,0)
    colnames(temp)[which(colnames(temp)=="date")]="Temp"
    datas$df_bis[["Temp"]]=date(datas$df_bis[["date"]])
    datas$df_bis<-dplyr::left_join(datas$df_bis,temp[,c("Temp","DJU")],by="Temp")
    datas$df_bis[["DJU"]]=round(datas$df_bis[["DJU"]],2)
    datas$df_bis<-datas$df_bis[,-which(colnames(datas$df_bis)=="Temp")]
    datas$df_agg<-datas$df_bis
    shinyjs::enable("calcul_dju")
    remove_modal_spinner()
  })

  observeEvent(input$map_marker_click, {
    updateSelectInput(session,"station","Choisissez la station météo la plus proche ",choices=station_selec_bis[station_selec_bis$USAF==input$map_marker_click$id,"STATION.NAME"])
  })

  observeEvent(input$country, {
    station_selec<<-select_station(liste_station,min(as.Date(datas$df[["date"]])),max(as.Date(datas$df[["date"]])))
    station_selec_bis<<-select_ss_station(station_selec,input$country)
    output$map <- renderLeaflet({leaflet::leaflet("map")%>%addTiles()%>%
        addAwesomeMarkers(lng=station_selec_bis$LON,lat=station_selec_bis$LAT, layerId = station_selec_bis$USAF, popup = station_selec_bis$STATION.NAME, icon=makeAwesomeIcon(icon= 'cloud', markerColor = 'green',iconColor = "lightgrey", library = "glyphicon"))})
    updateSelectInput(session,"station","Choisissez la station météo la plus proche ",choices=station_selec_bis$STATION.NAME)
  })
  output$tbl2 <- renderDT(datas$df_bis,editable=F,options = list(
    pageLength = 10,scrollX=TRUE))
  proxy = dataTableProxy('tbl2')

  ####tracé des données et sélection de la période référence page 4----
  observeEvent(c(input$periode), {
    datas$df_agg<<-aggregation(datas$df_bis,input$periode)
    aggreg<<-datas$df_agg})
  observeEvent(c(input$model_dispo,input$periode),{
    if (input$periode=="Mois"){
      updateSelectInput(session,"model_type","Quel type de modèle souhaitez vous appliquer ?",choices=c("Regression simple"),selected="Regression simple")
    }else{
      if (length(which(input$variables=="Temperature"))==1){
        if (length(which(input$variables=="occup"))==1&length(which(input$variables=="ferie"))==1){
          updateSelectInput(session,"model_type","Quel type de modèle souhaitez vous appliquer ?",choices=c("Regression simple","Regression par morceaux","TOWT"),selected="Regression simple")
        } else {
          updateSelectInput(session,"model_type","Quel type de modèle souhaitez vous appliquer ?",choices=c("Regression simple","Regression par morceaux"),selected="Regression simple")
        }}
    }
  })

  output$graph_ref <- renderPlotly({
    p<-ggplot2::ggplot(data=subset(datas$df_agg,date>as.POSIXct(input$dateRange[1])&date<as.POSIXct(input$dateRange[2])),aes(x=date,y=energie))+
      geom_point(colour="black")+
      geom_line(colour="blue")
    plotly::ggplotly(p)
  })



  ####sélection du modèle calculs page 5----
  observeEvent(input$valeur_ab, {
    datas$df_agg<-datas$df_agg[datas$df_agg[["energie"]]>0,]
    datas$df_rel<-datas$df_agg
    datas$df_bis<-datas$df_agg
  })
  observeEvent(c(input$FileInput,input$calcul_temp,input$calcul_occ,input$calcul_dju,input$period, input$model_type), {
    updateCheckboxGroupInput(session,"variables",choices = colnames(datas$df_bis)[3:ncol(datas$df_bis)],selected=colnames(datas$df_bis)[3:ncol(datas$df_bis)])
    if (input$model_type=="TOWT"){
      newSelection <<- which(!is.na(match(colnames(datas$df_bis)[3:ncol(datas$df_bis)],c("Temperature","occup","ferie"))))
      subElement <<- paste0("#variables .checkbox:nth-child(", newSelection,") label")
      delay(1,shinyjs::disable(selector=subElement))
    }
    if (input$model_type=="Regression par morceaux"){
      newSelection <<- which(!is.na(match(colnames(datas$df_bis)[3:ncol(datas$df_bis)],c("Temperature"))))
      subElement <<- paste0("#variables .checkbox:nth-child(", newSelection,") label")
      delay(1,shinyjs::disable(selector=subElement))
    }
  })
  observeEvent(input$calcul, {
    show_modal_spinner(spin="semipolar",color="#00a65a", text=HTML("<br>Le modèle statistique est en cours de calcul, cela peut prendre plusieurs dizaines de secondes"))
    shinyjs::disable("calcul")
    removeCssClass(selector = "a[data-value='result']", class = "inactiveLink")
    removeCssClass(selector = "a[data-value='export']", class = "inactiveLink")

    datas$df_rel<-subset(datas$df_agg,select=c("date","energie",input$variables))
    temp <- modelisation(datas$df_rel,input$model_type,input$dateRef[1],input$dateRef[2],input$economie)
    regression$reg<-temp$regression
    regression$reg2<-temp$regression2

    cc=temp$regression[["coefficients"]]
    if (input$model_type=="TOWT"){
      output$equation=renderText("Le modèle TOWT est très long à écrire, vous trouverez les termes dans l'export excel.")
      output$table_p<-renderTable({NULL})

    }else{
      eqn <- paste("E =", paste(round(cc[1],2), paste(round(cc[-1],2), names(cc[-1]), sep=" * ", collapse=" + "), sep=" + "))
      output$equation=renderUI({HTML(paste("<b>Voici l'équation d'ajustement :</b></br>",eqn))})
      output$table_p<-renderTable({stat_p(regression$reg,regression$calc)})

    }
    datas$df_rel=temp$df
    f_t_o_w<<-temp$ftow
    regression$calc<-1
    agg_j$df<-aggregation(datas$df_rel,"Jour")
    uncertainty<<-incertitude(datas$df_rel,input$dateRef[1],input$dateRef[2],input$economie)
    output$uncert=renderUI({HTML(paste0("<b>",
                                        span(class="ttooltip",
                                             style="color:black",
                                             "Incertitude au sens de la FDX 30-148 : ",
                                             span(class="ttooltiptext",
                                                  withMathJax("$$u_{mod}=\\sqrt{\\sum_{1+n}^{m}{(s*\\sqrt{1+X_{k*}(X'X)^{-1}X'_{k*}})}^2 }$$"))),"</b></br>",
                                        uncertainty$Utot[1], "<br>Soit une économie attendue de ",input$economie,"%, +/- ",
                                        uncertainty$Utotbis[1],"% de la consommation de la période de référence"))

    })
    qualite_mod<<-statistiques(datas$df_rel,input$dateRef[1],input$dateRef[2],regression$reg,regression$calc,input$economie)
    output$table_reg<-renderTable({qualite_mod})
    shinyjs::enable("calcul")
    remove_modal_spinner()

    #  removeModal()
  })




  ####Tracé des courbes de résultats page 6----

  # output$graph_eco <- renderPlotly({
  #   p<-ggplot2::ggplot(data=subset(agg_j$df,date<as.POSIXct(input$dateRef[1])|date>as.POSIXct(input$dateRef[2])),aes(x=date,y=prediction-energie))+
  #     ggtitle("Economie en direct")+
  #     xlab("Date")+
  #     ylab("Economie")+
  #     theme(legend.position='none')+
  #     geom_col(aes(fill=(prediction-energie)>0))
  #   plotly::ggplotly(p)
  # })

  output$graph_eco_ann <- renderPlotly({
    uncertainty=subset(uncertainty,date<input$dateRef[1]|date>input$dateRef[2])
    economie_cum<<-economie(agg_j$df,input$dateRef[1],input$dateRef[2])

    if (datas$df_rel$date[2]-datas$df_rel$date[1]<days(1)){
      economie_cum=cbind(subset(economie_cum,select=-date),uncertainty%>%filter(uncertainty$date==date(uncertainty$date)))
    }else{
      economie_cum=cbind(economie_cum,subset(uncertainty,select=-date))
    }
    p<-ggplot2::ggplot(data=economie_cum,aes(x=date))+
      ggtitle("Ecart à l'économie estimée en fin d'année")+
      xlab("Date")+
      ylab("Ecart à l'économie (%)")+
      geom_col(aes(y=economie,fill=economie>0))+
      theme(legend.position='none')
    plotly::ggplotly(p)
  })

  output$graph_cusum <- renderPlotly({
    cusum_for_graph <- cusum(datas$df_rel,input$dateRef[1],input$dateRef[2],as.numeric(uncertainty$S[1]))
    if (cusum_for_graph$date[2]-cusum_for_graph$date[1]<days(1)){
      cusum_for_graph=cusum_for_graph%>%filter(cusum_for_graph$date==as.POSIXct(date(cusum_for_graph$date)))
    }
    p<-ggplot2::ggplot(data=cusum_for_graph,aes(x=date))+
      ggtitle("Carte CUSUM")+
      xlab("Date")+
      ylab("Sommes cumulées")+
      geom_hline(aes(yintercept=h_plus[1]),col="red")+
      geom_hline(aes(yintercept=h_moins[1]),col="red")+
      geom_col(aes(y=SIplus),col="blue")+
      geom_col(aes(y=SImoins),col="purple")
    plotly::ggplotly(p)
  })

  output$graph_ref2 <- renderPlotly({
    graph_refb<-data.frame(subset(datas$df_rel,date>=input$dateRef[1]&date<=input$dateRef[2]),subset(uncertainty,date>=input$dateRef[1]&date<=input$dateRef[2],select=-date))
    graph_refb<-subset(graph_refb,date>=input$date_ref[1]&date<=input$date_ref[2])
    colors <- c("Mesure" = "black", "Prédiction" = "blue")
    p<-ggplot2::ggplot(data=graph_refb,aes(x=date))+
      ggtitle("Période de référence et modèle")+
      labs(x="Date",y="Energie",color="Légende")+
      geom_point(aes(y=energie, color="Mesure"))+
      geom_line(aes(y=prediction, color="Prédiction"))+
      scale_color_manual(values = colors)+
      geom_ribbon(aes(ymin=prediction-1.96*Ufdx,ymax=prediction+1.96*Ufdx),alpha=0.3)
    plotly::ggplotly(p)
  })

  output$graph_model <- renderPlotly({
    graph_model<-data.frame(subset(datas$df_rel,date<input$dateRef[1]|date>input$dateRef[2]),subset(uncertainty,date<input$dateRef[1]|date>input$dateRef[2],select=-date))
    graph_model2<-subset(graph_model,date>=input$date_model[1]&date<=input$date_model[2])
    colors <- c("Mesure" = "black", "Prédiction" = "blue")
    p<-ggplot2::ggplot(data=graph_model2,aes(x=date))+
      ggtitle("Période de suivi et modèle")+
      labs(x="Date",y="Energie",color="Légende")+
      geom_point(aes(y=energie, color="Mesure"))+
      geom_line(aes(y=prediction, color="Prédiction"))+
      scale_color_manual(values = colors)+
      geom_ribbon(aes(ymin=prediction-1.96*Ufdx,ymax=prediction+1.96*Ufdx),alpha=0.3)
    plotly::ggplotly(p)
  })

  output$heatmap <- renderPlotly({
    if (input$model_type=="TOWT"){
      dataplot<-data.frame(datas$df_rel,ftow=f_t_o_w)
      dataplot<-subset(dataplot,date<=input$dateRef[2])
      dataplot$mois=month(dataplot$date,label=TRUE)
      z=plyr::ddply(dataplot,c("mois","ftow"),summarise,Consommation=energie)
      z$mois=as.factor(z$mois)
      z$ftow=as.factor(z$ftow)
      plotly::plot_ly(x=z$ftow,y=z$mois,z=z$Consommation,type="heatmap",colors = colorRamp(c("cyan", "orange")))%>%
        layout(title="Consommation en fonction des instants de la semaine (ftow)")%>%
        colorbar(limits=c(0,max(dataplot$energie,dataplot$prediction)))
    }
  })
  output$heatmap2 <- renderPlotly({
    if (input$model_type=="TOWT"){
      dataplot<-data.frame(datas$df_rel,ftow=f_t_o_w)
      dataplot<-subset(dataplot,date<=input$dateRef[2])
      dataplot$mois=month(dataplot$date,label=TRUE)
      z=plyr::ddply(dataplot,c("mois","ftow"),summarise,Consommation=prediction)
      z$mois=as.factor(z$mois)
      z$ftow=as.factor(z$ftow)
      plotly::plot_ly(x=z$ftow,y=z$mois,z=z$Consommation,type="heatmap",colors = colorRamp(c("cyan", "orange")))%>%
        layout(title="Prédiction du modèle TOWT")%>%
        colorbar(limits=c(0,max(dataplot$energie,dataplot$prediction)))
    }

  })

  ####Export résultats page 7----
  output$dl <- downloadHandler(
    filename = function() {
      paste("donnees_mpeb", ".xlsx", sep = "")
    },
    content = function(file) {
      sheets<-list()
      sheets$donnees=datas$df_rel
      if (input$model_type=="TOWT"){
        a=data.frame(variables=names(regression$reg[["coefficients"]]),coefficients=regression$reg[["coefficients"]],occup="0",type_modele=input$model_type)
        sheets$regression=rbind(a,data.frame(variables=names(regression$reg2[["coefficients"]]),coefficients=regression$reg2[["coefficients"]],occup="1",type_modele=input$model_type))
      }else{
        sheets$regression=data.frame(variables=names(regression$reg[["coefficients"]]),coefficients=regression$reg[["coefficients"]],type_modele=input$model_type)
      }
      sheets$qualite_model=qualite_mod
      sheets$economie=data.frame(date=economie_cum$date,economie_prct_annee_de_ref=economie_cum$economie)
      sheets$incertitude=data.frame(date=uncertainty$date,incertitude=uncertainty$Ufdx)
      writexl::write_xlsx(sheets, path=file)


    }

  )
}

shinyApp(ui=ui,server=server)


