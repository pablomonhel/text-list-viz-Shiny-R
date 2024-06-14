
list.of.packages <- c("data.table","plotly","readxl","tm","Rtsne","plyr","data.table","waiter","quanteda","DT","wordcloud","shinydashboard","shiny","plotly", "igraph", "ggraph")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Este c?digo permite la subida de archivos de 2000 megas
options(shiny.maxRequestSize=200*1024^2)


library(shiny)
library(shinydashboard)
library(plotly)
#library(plyr)
library(data.table)
library(waiter)
library(DT)
library(Rtsne)
library(tm)
library(readxl)

ui<-dashboardPage(
  dashboardHeader(title="Lista de textos"),
  dashboardSidebar(
    sidebarMenu(id="side_menu",
      menuItem("Lista original", tabName = "textos", icon = icon("list")),
      menuItem("Gráficos de frecuencias", tabName = "graficos_barras", icon = icon("chart-bar")),
      menuItem("Nubes de palabras", tabName = "nubes_palabras", icon = icon("cloud")),
      menuItem("Red de términos principales", tabName = "red_terminos", icon = icon("project-diagram")),
      menuItem("Tabla de frecuencia de palabras", tabName = "clusters_palabra", icon = icon("table")),
      menuItem("Tabla de frecuencia de bigramas", tabName = "clusters_bigramas", icon = icon("table")),
      menuItem("Gráfico de clusters",tabName="grafico_clusters",icon=icon("hubspot"))
    ),
    fileInput('file1','Importar archivo de Excel (".xlsx")',accept = c(".xlsx")),
    selectInput("cluster_despegable",label="Número de clusters",choices=c("Sin clusters","2","3","4","5")),
    uiOutput("secondSelection"),
    fileInput("file2",'Subir stopwords (".xlsx")',accept=c(".xlsx")),
    tags$style(type="text/css", "#descargar_tabla_cluster {background-color:white;color: black}"),
    downloadButton("descargar_tabla_cluster","Descargar tabla con clusters", class = "butt1")
  ),
  dashboardBody(
    use_waiter(),
    tabItems(
      tabItem("textos",
              fluidRow(box(title="Tabla",status="primary",width=12,tableOutput("tabla_con_PQRS")))),
      tabItem("graficos_barras", 
              fluidRow(box(title = "Unigramas",status = "primary",width = 12,plotOutput("barras_palabras"))),
              fluidRow(box(title = "Bigramas",status = "primary",width = 12,plotOutput("barras_bigramas")))),
      tabItem("red_terminos",
              fluidRow(box(title="Red",status="primary",height=12,width=12,plotOutput("red")))),
      tabItem("nubes_palabras",
              fluidRow(box(title="Unigramas",status="primary",width=12,plotOutput("nube_palabras"))),
              fluidRow(box(title="Bigramas",status="primary",width=12,plotOutput("nube_bigramas")))),
      tabItem("clusters_palabra",
              fluidRow(box(title="Unigramas",status="primary",width=12,div(style='overflow-x: scroll',dataTableOutput("clusters_palabras"))))),
      tabItem("clusters_bigramas",
              fluidRow(box(title="Bigramas",status="primary",width=12,div(style="overflow-x: scroll",dataTableOutput("clusters_bigramas"))))),
      tabItem("grafico_clusters",
              fluidRow(box(title="Gráfico de clusters",status="primary",width=12,plotlyOutput("cluster_graficos"))))
    
      ))
)

server <- function(input, output) { 
  # Funciones
  
  # 1. Preprocesamiento de texto
  preproc.text <- function(x){
    require(dplyr)
    y <- x %>%
      gsub("[^[:print:]]", " ", .) %>%
      tolower %>% 
      
      gsub("á","a",.) %>% 
      gsub("é","e",.) %>%
      gsub("í","i",.) %>%
      gsub("ó","o",.) %>%
      gsub("ú","u",.) %>%
      
      gsub("à","a",.) %>% 
      gsub("è","e",.) %>%
      gsub("ì","i",.) %>%
      gsub("ò","o",.) %>%
      gsub("ù","u",.) %>%
      
      gsub("[^[:lower:]^[:space:]]", " ", .) %>%
      gsub("[[:space:]]{1,}", " ", .) %>%
      trimws
    return(y)
  }  
  # 2. Contar ngramas presentes en un texto ----
  contar_ngramas <- function(texto, n_palabras){
    
    require(NLP)
    
    # Tokeniza el texto por palabras
    texto <- as.character(texto)
    texto <- sapply(texto, function(x) strsplit(x, split = " ")[[1]])

    # Reescribe el texto como ngramas
    ngramas <- sapply(texto, function(x) NLP::ngrams(x, n_palabras))
    ngramas <- sapply(ngramas, function(x) sapply(x, function(y) paste(y, collapse = " ")))
    
    # Realiza una tabla de frecuencias para los ngramas
    df <- data.frame(sort(table(unlist(ngramas)), decreasing = TRUE))
    colnames(df) <- c("Palabra", "Frecuencia")
    
    # Retorna un data frame que contiene los n-gramas y su frecuencia en el texto
    
    df<-df[df$Palabra!="",]
    
    return(df)
  }
  
  # 3. obtener tabla con 10 palabras más frecuentes
  palabras_10<-function(df){
    ngramas_10<-ngramas[order(ngramas$Frecuencia,decreasing=TRUE),]
    ngramas_10<-head(ngramas,10)
  }
  
  # 4. Graficar barras
  grafico_barras<-function(df){
    require(ggplot2)
    df<-head(df,10)
    ggplot(df,aes(x=Palabra,y=Frecuencia))+geom_bar(stat="identity",fill="coral2")+coord_flip()+theme(axis.text.y=element_text(size=12),axis.text.x=element_text(size=12))
  }
  
  # 5. Hacer red de palabras ----
  hacer_red_palabras <- function(texto){
    
    require(quanteda)
    require(igraph)
    require(ggraph)
    
    # Crea el corpus del texto ingresado
    corpus_texto <- quanteda::corpus(as.character(texto))
    
    # Crea la matriz tÃ©rminos y documentos (equivalente a un Bag of Words)
    dfmat <- dfm(tokens(corpus_texto))
    
    # Elimina los tÃ©rminos que no aparecen mÃ¡s de 1 vez
    dfmat <- dfm_trim(dfmat, min_termfreq = 2)
    
    # Crea la matriz de coocurrencia de tÃ©rminos
    fcmat <- fcm(dfmat)
    
    
    # Convert the fcmat to a matrix
    fcmat_matrix <- as.matrix(fcmat)
    
    graph <- graph_from_adjacency_matrix(fcmat_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)
    
    ggraph(graph, layout = "fr") + 
      geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) + 
      geom_node_point(color = "lightblue", size = 5) + 
      geom_node_text(aes(label = name), repel = TRUE) + 
      theme_void()
    
    }
  
  # 6. Nube de palabras
  hacer_nube_palabras<-function(df){
    require(wordcloud)
    df<-head(df,200)
    wordcloud(df$Palabra, df$Frecuencia, min.freq = 1, random.order = FALSE,
              colors=brewer.pal(7, "Dark2"), fixed.asp = T)
  }
  
  # 7.1. Quitar stopwords. 
  quitar_stopwords<-function(texto){
    library(tm)
    agregar<-data.frame(readLines("stopwords/agregar.txt",encoding="ANSI"))
    apellidos<-data.frame(readLines("stopwords/apellidos.txt",encoding="ANSI"))
    excluir<-data.frame(readLines("stopwords/excluir.txt",encoding="UTF-8"))
    github<-data.frame(readLines("stopwords/github.txt",encoding="UTF-8"))
    nombres<-data.frame(readLines("stopwords/nombres.txt",encoding="ANSI"))
    ranknl<-data.frame(readLines("stopwords/ranknl.txt",encoding="UTF-8"))

    lista<-list(agregar,apellidos,excluir,github,nombres,ranknl)
    
    stopno<-texto
    for(s in lista){
      stopno<-removeWords(stopno,as.vector(t(s)))
    }
    stopno<-gsub("[[:space:]]{1,}", " ", stopno)
    return(stopno)
  }
  
  # 7.2. Quitar stopwords. Función para quitar nuevos stopwords escogidos por el usuario desde la aplicación
  quitar_stopwords_file2<-function(texto,stopbase){
    library(tm)
    agregar<-data.frame(readLines("stopwords/agregar.txt",encoding="ANSI"))
    apellidos<-data.frame(readLines("stopwords/apellidos.txt",encoding="ANSI"))
    excluir<-data.frame(readLines("stopwords/excluir.txt",encoding="UTF-8"))
    github<-data.frame(readLines("stopwords/github.txt",encoding="UTF-8"))
    nombres<-data.frame(readLines("stopwords/nombres.txt",encoding="ANSI"))
    ranknl<-data.frame(readLines("stopwords/ranknl.txt",encoding="UTF-8"))
    nuevas_palabras<-apply(data.frame(stopbase),2,preproc.text)
    
    lista<-list(agregar,apellidos,excluir,github,nombres,ranknl,nuevas_palabras)
    
    stopno<-texto
    for(s in lista){
      stopno<-removeWords(stopno,as.vector(t(s)))
    }
    stopno<-gsub("[[:space:]]{1,}", " ", stopno)
    return(stopno)
  }
  
  # 8. Puntos en el espacio T-SNE bidimensional
  bidimensional<-function(texto){
    require(Rtsne)
    require(quanteda)
    
    stemis<-data.frame(stemDocument(texto,language="spanish"))
    stemis<-apply(stemis,2,as.character)

    # HACER MATRIZ DE BAG OF WORDS
    stemis<-as.character(stemis)
    #set.seed(39857)
    texto_dfm<-dfm(tokens(corpus(as.character(stemis))))

    texto_dfm_filtro <- dfm_trim(texto_dfm,min_termfreq = 10)

     # T-SNE
     dfm_matrix<-convert(texto_dfm_filtro,to="matrix")
     dfm_matrix<-dfm_matrix[,-1]
     mi_tsne <- Rtsne(dfm_matrix, dims = 2, perplexity = 10, theta = 0.1,check_duplicates=FALSE)
     puntos_bidimensional <- mi_tsne$Y
     return(puntos_bidimensional)
   }

  # 9. Tablas con textos y clusters
  tablas_clusters<-function(texto_original,puntos_bidimensional,ncluster,ngramas) {
    mikmeans<-kmeans(puntos_bidimensional,ncluster)

    # Frecuencias de palabras en cada cluster
    clusters<-mikmeans$cluster
    texto_original$clusters<-clusters

    lista_nclusters<-c()
    for(i in (1:ncluster)){
      texto_cluster<-texto_original[texto_original$clusters==i,]
      nostop<-quitar_stopwords(preproc.text(texto_cluster))

      ngrams<-contar_ngramas(nostop,ngramas)

      ngrams_100<-ngrams[1:100,]

      lista_nclusters[[i]]<-ngrams_100

    }
    return(lista_nclusters)
  }
   
  
  # 10. Gráfica de clusters en espacio de dos dimensiones T-SNE
  graficar_2_dimensiones<-function(puntos_bidimensionales,ncluster){
    library(plotly)
    mikmeans<-kmeans(puntos_bidimensionales,ncluster)
    p<-plot_ly(puntos_bidimensionales,x=~X1,y=~X2,color=mikmeans$cluster)
    return(p)
  }
  
  ### salidas (outputs)

  ## Leer la base
  
  tabla <- reactiveValues(df_data = NULL)
  tabla_nuevo_stop<-reactiveValues(df_data=NULL)
  procesado<-reactiveValues(df_data=NULL)
  nostop<-reactiveValues(df_data=NULL)
  ngramas1<-reactiveValues(df_data = NULL)
  ngramas2<-reactiveValues(df_data = NULL)
  df_bidimensional<-reactiveValues(df_data=NULL)
  df_bidimensional_texto<-reactiveValues(df_data=NULL)
  
  clusters_1<-reactiveValues(df_data = NULL)
  clusters_2<-reactiveValues(df_data = NULL)
  clusters_3<-reactiveValues(df_data = NULL)
  clusters_4<-reactiveValues(df_data = NULL)
  clusters_5<-reactiveValues(df_data = NULL)
  
  nostop_cluster_1<-reactiveValues(df_data = NULL)
  nostop_cluster_2<-reactiveValues(df_data = NULL)
  nostop_cluster_3<-reactiveValues(df_data = NULL)
  nostop_cluster_4<-reactiveValues(df_data = NULL)
  nostop_cluster_5<-reactiveValues(df_data = NULL)
  
  texto_cluster_1<-reactiveValues(df_data = NULL)
  texto_cluster_2<-reactiveValues(df_data = NULL)
  texto_cluster_3<-reactiveValues(df_data = NULL)
  texto_cluster_4<-reactiveValues(df_data = NULL)
  texto_cluster_5<-reactiveValues(df_data = NULL)
  
  tabla_clusteruni_1<-reactiveValues(df_data=NULL)
  tabla_clusteruni_2<-reactiveValues(df_data=NULL)
  tabla_clusteruni_3<-reactiveValues(df_data=NULL)
  tabla_clusteruni_4<-reactiveValues(df_data=NULL)
  tabla_clusteruni_5<-reactiveValues(df_data=NULL)

  preprocesamiento<-reactiveValues(df_data=NULL)
  nostopwords<-reactiveValues(df_data=NULL)
  
  observeEvent(input$file1,{
    # show_waiter(html="",color="#FFFFFF19","")
    waiter_show(html="",color="#FFFFFF19","")
    
    tabla$df_data<-read_excel(input$file1$datapath,col_names=c("asunto"))

    preprocesamiento$df_data<-apply(tabla$df_data,2,preproc.text)
    nostopwords$df_data<-apply(preprocesamiento$df_data,2,quitar_stopwords)
    
    set.seed(83276)
    df_bidimensional$df_data<-data.frame(bidimensional(nostopwords$df_data))
    
    clusters_1$df_data<-kmeans(df_bidimensional$df_data,1)$cluster
    clusters_2$df_data<-kmeans(df_bidimensional$df_data,2)$cluster
    clusters_3$df_data<-kmeans(df_bidimensional$df_data,3)$cluster
    clusters_4$df_data<-kmeans(df_bidimensional$df_data,4)$cluster
    clusters_5$df_data<-kmeans(df_bidimensional$df_data,5)$cluster
    
    nostop_cluster_1$df_data<-cbind(nostopwords$df_data,data.frame(clusters_1$df_data))
    nostop_cluster_2$df_data<-cbind(nostopwords$df_data,data.frame(clusters_2$df_data))
    nostop_cluster_3$df_data<-cbind(nostopwords$df_data,data.frame(clusters_3$df_data))
    nostop_cluster_4$df_data<-cbind(nostopwords$df_data,data.frame(clusters_4$df_data))
    nostop_cluster_5$df_data<-cbind(nostopwords$df_data,data.frame(clusters_5$df_data))
    
    texto_cluster_1$df_data<-cbind(tabla$df_data,data.frame(clusters_1$df_data))
    texto_cluster_2$df_data<-cbind(tabla$df_data,data.frame(clusters_2$df_data))
    texto_cluster_3$df_data<-cbind(tabla$df_data,data.frame(clusters_3$df_data))
    texto_cluster_4$df_data<-cbind(tabla$df_data,data.frame(clusters_4$df_data))
    texto_cluster_5$df_data<-cbind(tabla$df_data,data.frame(clusters_5$df_data))
    waiter_hide()
    # hide_waiter()
  })  
  
  observeEvent(input$file2,{
    # show_waiter(spin_double_bounce(),color="#FFFFFF19")
    waiter_show(spin_double_bounce(),color="#FFFFFF19")

    tabla_nuevo_stop$df_data<-apply(read_excel(input$file2$datapath,col_names=FALSE),2,preproc.text)
    preprocesamiento$df_data<-apply(tabla$df_data,2,preproc.text)

    #nostopwords$df_data<-apply(preprocesamiento$df_data,2,quitar_stopwords)
    nostopwords$df_data<-apply(preprocesamiento$df_data,2,function(x) quitar_stopwords_file2(x,tabla_nuevo_stop$df_data))
    
    set.seed(83276)
    
    df_bidimensional$df_data<-data.frame(bidimensional(nostopwords$df_data))

    clusters_1$df_data<-kmeans(df_bidimensional$df_data,1)$cluster
    clusters_2$df_data<-kmeans(df_bidimensional$df_data,2)$cluster
    clusters_3$df_data<-kmeans(df_bidimensional$df_data,3)$cluster
    clusters_4$df_data<-kmeans(df_bidimensional$df_data,4)$cluster
    clusters_5$df_data<-kmeans(df_bidimensional$df_data,5)$cluster

    nostop_cluster_1$df_data<-cbind(nostopwords$df_data,data.frame(clusters_1$df_data))
    nostop_cluster_2$df_data<-cbind(nostopwords$df_data,data.frame(clusters_2$df_data))
    nostop_cluster_3$df_data<-cbind(nostopwords$df_data,data.frame(clusters_3$df_data))
    nostop_cluster_4$df_data<-cbind(nostopwords$df_data,data.frame(clusters_4$df_data))
    nostop_cluster_5$df_data<-cbind(nostopwords$df_data,data.frame(clusters_5$df_data))
    
    texto_cluster_1$df_data<-cbind(tabla$df_data,data.frame(clusters_1$df_data))
    texto_cluster_2$df_data<-cbind(tabla$df_data,data.frame(clusters_2$df_data))
    texto_cluster_3$df_data<-cbind(tabla$df_data,data.frame(clusters_3$df_data))
    texto_cluster_4$df_data<-cbind(tabla$df_data,data.frame(clusters_4$df_data))
    texto_cluster_5$df_data<-cbind(tabla$df_data,data.frame(clusters_5$df_data))
    waiter_hide()
    # hide_waiter()

  })

  # Que segundo selecInput dependa del primero
  output$secondSelection <- renderUI({
    
    if (input$side_menu=="graficos_barras" & input$cluster_despegable!="Sin clusters") {
      selectInput("cluster_escoger",label="Escoger cluster",choices=c(1:input$cluster_despegable))
    }else if (input$side_menu=="nubes_palabras" & input$cluster_despegable!="Sin clusters") {
      selectInput("cluster_escoger",label="Escoger cluster",choices=c(1:input$cluster_despegable))
    }else if (input$side_menu=="red_terminos" & input$cluster_despegable!="Sin clusters") {
      selectInput("cluster_escoger",label="Escoger cluster",choices=c(1:input$cluster_despegable))
    }
    
  })

  # 1 Tabla de PQRSD
  output$tabla_con_PQRS<-renderTable({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    data.frame(tabla$df_data)
  })
  
  # 2.1 Tabla de palabras más frecuentes
  output$barras_palabras<-renderPlot({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    
    if (input$cluster_despegable=="Sin clusters"){
      grafico_barras(contar_ngramas(nostopwords$df_data,1))
    }
    else if(input$cluster_despegable==2){
      grafico_barras(contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==as.integer(input$cluster_escoger),]$asunto,1))
    }
    else if(input$cluster_despegable==3){
      grafico_barras(contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==as.integer(input$cluster_escoger),]$asunto,1))
    }
    else if(input$cluster_despegable==4){
      grafico_barras(contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==as.integer(input$cluster_escoger),]$asunto,1))
    }
    else{
      grafico_barras(contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==as.integer(input$cluster_escoger),]$asunto,1))
    }
    })

  # 2.2 Tabla de bigramas más frecuentes
  output$barras_bigramas<-renderPlot({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    
    if (input$cluster_despegable=="Sin clusters"){
      grafico_barras(contar_ngramas(nostopwords$df_data,2))
    }
    else if(input$cluster_despegable==2){
      grafico_barras(contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==as.integer(input$cluster_escoger),]$asunto,2))
    }
    else if(input$cluster_despegable==3){
      grafico_barras(contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==as.integer(input$cluster_escoger),]$asunto,2))
    }
    else if(input$cluster_despegable==4){
      grafico_barras(contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==as.integer(input$cluster_escoger),]$asunto,2))
    }
    else{
      grafico_barras(contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==as.integer(input$cluster_escoger),]$asunto,2))
    }
  })
  
  # 3 Red de palabras
  output$red<-renderPlot({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
  
    if (input$cluster_despegable=="Sin clusters"){
      hacer_red_palabras(nostopwords$df_data)
    }
    else if(input$cluster_despegable==2){
      hacer_red_palabras(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==as.integer(input$cluster_escoger),]$asunto)
    }
    else if(input$cluster_despegable==3){
      hacer_red_palabras(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==as.integer(input$cluster_escoger),]$asunto)
    }
    else if(input$cluster_despegable==4){
      hacer_red_palabras(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==as.integer(input$cluster_escoger),]$asunto)
    }
    else{
      hacer_red_palabras(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==as.integer(input$cluster_escoger),]$asunto)
    }
    })

  # # 4.1 Nube de palabras
  output$nube_palabras<-renderPlot({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    
    if (input$cluster_despegable=="Sin clusters"){
      hacer_nube_palabras(contar_ngramas(nostopwords$df_data,1))
    }
    else if(input$cluster_despegable==2){
      hacer_nube_palabras(contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==as.integer(input$cluster_escoger),]$asunto,1))
    }
    else if(input$cluster_despegable==3){
      hacer_nube_palabras(contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==as.integer(input$cluster_escoger),]$asunto,1))
    }
    else if(input$cluster_despegable==4){
      hacer_nube_palabras(contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==as.integer(input$cluster_escoger),]$asunto,1))
    }
    else{
      hacer_nube_palabras(contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==as.integer(input$cluster_escoger),]$asunto,1))
    }
  })

  # 4.2 Nube de bigramas
  output$nube_bigramas<-renderPlot({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    
    if (input$cluster_despegable=="Sin clusters"){
      hacer_nube_palabras(contar_ngramas(nostopwords$df_data,2))
    }
    else if(input$cluster_despegable==2){
      hacer_nube_palabras(contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==as.integer(input$cluster_escoger),]$asunto,2))
    }
    else if(input$cluster_despegable==3){
      hacer_nube_palabras(contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==as.integer(input$cluster_escoger),]$asunto,2))
    }
    else if(input$cluster_despegable==4){
      hacer_nube_palabras(contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==as.integer(input$cluster_escoger),]$asunto,2))
    }
    else{
      hacer_nube_palabras(contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==as.integer(input$cluster_escoger),]$asunto,2))
    }
  })
  
  # 5.1 Tabla de frecuencia de palabras 
  output$clusters_palabras<-renderDataTable({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    
    if (input$cluster_despegable=="Sin clusters"){
      ngramas_tablas<-contar_ngramas(nostopwords$df_data,1)
      ngramas_tablas<-ngramas_tablas[ngramas_tablas$Frecuencia!=1,]
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else if(input$cluster_despegable==2){
      ngramas_tabla_2_1<-contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==1,]$asunto,1)
      ngramas_tabla_2_2<-contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==2,]$asunto,1)

      rownames(ngramas_tabla_2_1) <- NULL
      rownames(ngramas_tabla_2_2) <- NULL
      
      ngramas_tabla_2_1<-setDT(ngramas_tabla_2_1,keep.rownames=TRUE)[]
      ngramas_tabla_2_2<-setDT(ngramas_tabla_2_2,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_2_1<-ngramas_tabla_2_1[ngramas_tabla_2_1$Frecuencia!=1,]
      ngramas_tabla_2_2<-ngramas_tabla_2_2[ngramas_tabla_2_2$Frecuencia!=1,]
      
      ngramas_tablas<-merge(ngramas_tabla_2_1,ngramas_tabla_2_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Palabra cluster 1","Frecuencia cluster 1","Palabra cluster 2","Frecuencia cluster 2")
      #colnames(ngramas_tablas) = str_wrap(colnames(ngramas_tablas),width = 10)
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else if(input$cluster_despegable==3){
      ngramas_tabla_3_1<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==1,]$asunto,1)
      ngramas_tabla_3_2<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==2,]$asunto,1)
      ngramas_tabla_3_3<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==3,]$asunto,1)
      
      rownames(ngramas_tabla_3_1) <- NULL
      rownames(ngramas_tabla_3_2) <- NULL
      rownames(ngramas_tabla_3_3) <- NULL
      
      ngramas_tabla_3_1<-setDT(ngramas_tabla_3_1,keep.rownames=TRUE)[]
      ngramas_tabla_3_2<-setDT(ngramas_tabla_3_2,keep.rownames=TRUE)[]
      ngramas_tabla_3_3<-setDT(ngramas_tabla_3_3,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_3_1<-ngramas_tabla_3_1[ngramas_tabla_3_1$Frecuencia!=1,]
      ngramas_tabla_3_2<-ngramas_tabla_3_2[ngramas_tabla_3_2$Frecuencia!=1,]
      ngramas_tabla_3_3<-ngramas_tabla_3_3[ngramas_tabla_3_3$Frecuencia!=1,]
      
      ngramas_tablas<-merge(ngramas_tabla_3_1,ngramas_tabla_3_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_3_3,by="rn",all=TRUE,merge=TRUE)
      
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Palabra cluster 1","Frecuencia cluster 1","Palabra cluster 2","Frecuencia cluster 2","Palabra cluster 3","Frecuencia cluster 3")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else if(input$cluster_despegable==4){
      ngramas_tabla_4_1<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==1,]$asunto,1)
      ngramas_tabla_4_2<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==2,]$asunto,1)
      ngramas_tabla_4_3<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==3,]$asunto,1)
      ngramas_tabla_4_4<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==4,]$asunto,1)
      
      rownames(ngramas_tabla_4_1) <- NULL
      rownames(ngramas_tabla_4_2) <- NULL
      rownames(ngramas_tabla_4_3) <- NULL
      rownames(ngramas_tabla_4_4) <- NULL
      
      ngramas_tabla_4_1<-setDT(ngramas_tabla_4_1,keep.rownames=TRUE)[]
      ngramas_tabla_4_2<-setDT(ngramas_tabla_4_2,keep.rownames=TRUE)[]
      ngramas_tabla_4_3<-setDT(ngramas_tabla_4_3,keep.rownames=TRUE)[]
      ngramas_tabla_4_4<-setDT(ngramas_tabla_4_4,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_4_1<-ngramas_tabla_4_1[ngramas_tabla_4_1$Frecuencia!=1,]
      ngramas_tabla_4_2<-ngramas_tabla_4_2[ngramas_tabla_4_2$Frecuencia!=1,]
      ngramas_tabla_4_3<-ngramas_tabla_4_3[ngramas_tabla_4_3$Frecuencia!=1,]
      ngramas_tabla_4_4<-ngramas_tabla_4_4[ngramas_tabla_4_4$Frecuencia!=1,]
      
      ngramas_tablas<-merge(ngramas_tabla_4_1,ngramas_tabla_4_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_4_3,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_4_4,by="rn",all=TRUE,merge=TRUE)
      
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Palabra cluster 1","Frecuencia cluster 1","Palabra cluster 2","Frecuencia cluster 2","Palabra cluster 3","Frecuencia cluster 3","Palabra cluster 4","Frecuencia cluster 4")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else{
      ngramas_tabla_5_1<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==1,]$asunto,1)
      ngramas_tabla_5_2<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==2,]$asunto,1)
      ngramas_tabla_5_3<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==3,]$asunto,1)
      ngramas_tabla_5_4<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==4,]$asunto,1)
      ngramas_tabla_5_5<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==5,]$asunto,1)
      rownames(ngramas_tabla_5_1) <- NULL      

      rownames(ngramas_tabla_5_2) <- NULL
      rownames(ngramas_tabla_5_3) <- NULL
      rownames(ngramas_tabla_5_4) <- NULL
      rownames(ngramas_tabla_5_5) <- NULL
      
      ngramas_tabla_5_1<-setDT(ngramas_tabla_5_1,keep.rownames=TRUE)[]
      ngramas_tabla_5_2<-setDT(ngramas_tabla_5_2,keep.rownames=TRUE)[]
      ngramas_tabla_5_3<-setDT(ngramas_tabla_5_3,keep.rownames=TRUE)[]
      ngramas_tabla_5_4<-setDT(ngramas_tabla_5_4,keep.rownames=TRUE)[]
      ngramas_tabla_5_5<-setDT(ngramas_tabla_5_5,keep.rownames=TRUE)[]
      
      # borrar filas de una frecuencia
      ngramas_tabla_5_1<-ngramas_tabla_5_1[ngramas_tabla_5_1$Frecuencia!=1,]
      ngramas_tabla_5_2<-ngramas_tabla_5_2[ngramas_tabla_5_2$Frecuencia!=1,]
      ngramas_tabla_5_3<-ngramas_tabla_5_3[ngramas_tabla_5_3$Frecuencia!=1,]
      ngramas_tabla_5_4<-ngramas_tabla_5_4[ngramas_tabla_5_4$Frecuencia!=1,]
      ngramas_tabla_5_5<-ngramas_tabla_5_5[ngramas_tabla_5_5$Frecuencia!=1,]
      
      # cambiar nombres antes porque si no no funciona el merge
      names(ngramas_tabla_5_1)<-c("rn","Palabra cluster 1","Frecuencia cluster 1")
      names(ngramas_tabla_5_2)<-c("rn","Palabra cluster 2","Frecuencia cluster 2")
      names(ngramas_tabla_5_3)<-c("rn","Palabra cluster 3","Frecuencia cluster 3")
      names(ngramas_tabla_5_4)<-c("rn","Palabra cluster 4","Frecuencia cluster 4")
      names(ngramas_tabla_5_5)<-c("rn","Palabra cluster 5","Frecuencia cluster 5")
      
      ngramas_tablas<-merge(ngramas_tabla_5_1,ngramas_tabla_5_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_5_3,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_5_4,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_5_5,by="rn",all=TRUE,merge=TRUE)

      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)

      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Palabra cluster 1","Frecuencia cluster 1","Palabra cluster 2","Frecuencia cluster 2","Palabra cluster 3","Frecuencia cluster 3","Palabra cluster 4","Frecuencia cluster 4","Palabra cluster 5","Frecuencia cluster 5")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
  })
    
  # 5.2 Tabla de frecuencia de  bigramas
  output$clusters_bigramas<-renderDataTable({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    
    if (input$cluster_despegable=="Sin clusters"){
      ngramas_tablas<-contar_ngramas(nostopwords$df_data,2)
      ngramas_tablas<-ngramas_tablas[ngramas_tablas$Frecuencia!=1,]
      names(ngramas_tablas)<-c("Bigrama","Frecuencia")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else if(input$cluster_despegable==2){
      ngramas_tabla_2_1<-contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==1,]$asunto,2)
      ngramas_tabla_2_2<-contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==2,]$asunto,2)
      
      rownames(ngramas_tabla_2_1) <- NULL
      rownames(ngramas_tabla_2_2) <- NULL
      
      ngramas_tabla_2_1<-setDT(ngramas_tabla_2_1,keep.rownames=TRUE)[]
      ngramas_tabla_2_2<-setDT(ngramas_tabla_2_2,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_2_1<-ngramas_tabla_2_1[ngramas_tabla_2_1$Frecuencia!=1,]
      ngramas_tabla_2_2<-ngramas_tabla_2_2[ngramas_tabla_2_2$Frecuencia!=1,]
      
      ngramas_tablas<-merge(ngramas_tabla_2_1,ngramas_tabla_2_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Bigrama cluster 1","Frecuencia cluster 1","Bigrama cluster 2","Bigrama cluster 2")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else if(input$cluster_despegable==3){
      ngramas_tabla_3_1<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==1,]$asunto,2)
      ngramas_tabla_3_2<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==2,]$asunto,2)
      ngramas_tabla_3_3<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==3,]$asunto,2)
      
      rownames(ngramas_tabla_3_1) <- NULL
      rownames(ngramas_tabla_3_2) <- NULL
      rownames(ngramas_tabla_3_3) <- NULL
      
      ngramas_tabla_3_1<-setDT(ngramas_tabla_3_1,keep.rownames=TRUE)[]
      ngramas_tabla_3_2<-setDT(ngramas_tabla_3_2,keep.rownames=TRUE)[]
      ngramas_tabla_3_3<-setDT(ngramas_tabla_3_3,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_3_1<-ngramas_tabla_3_1[ngramas_tabla_3_1$Frecuencia!=1,]
      ngramas_tabla_3_2<-ngramas_tabla_3_2[ngramas_tabla_3_2$Frecuencia!=1,]
      ngramas_tabla_3_3<-ngramas_tabla_3_3[ngramas_tabla_3_3$Frecuencia!=1,]
      
      ngramas_tablas<-merge(ngramas_tabla_3_1,ngramas_tabla_3_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_3_3,by="rn",all=TRUE,merge=TRUE)
      
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Bigrama cluster 1","Frecuencia cluster 1","Bigrama cluster 2","Frecuencia cluster 2","Bigrama cluster 3","Frecuencia cluster 3")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    else if(input$cluster_despegable==4){
      ngramas_tabla_4_1<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==1,]$asunto,2)
      ngramas_tabla_4_2<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==2,]$asunto,2)
      ngramas_tabla_4_3<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==3,]$asunto,2)
      ngramas_tabla_4_4<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==4,]$asunto,2)
      
      rownames(ngramas_tabla_4_1) <- NULL
      rownames(ngramas_tabla_4_2) <- NULL
      rownames(ngramas_tabla_4_3) <- NULL
      rownames(ngramas_tabla_4_4) <- NULL
      
      ngramas_tabla_4_1<-setDT(ngramas_tabla_4_1,keep.rownames=TRUE)[]
      ngramas_tabla_4_2<-setDT(ngramas_tabla_4_2,keep.rownames=TRUE)[]
      ngramas_tabla_4_3<-setDT(ngramas_tabla_4_3,keep.rownames=TRUE)[]
      ngramas_tabla_4_4<-setDT(ngramas_tabla_4_4,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_4_1<-ngramas_tabla_4_1[ngramas_tabla_4_1$Frecuencia!=1,]
      ngramas_tabla_4_2<-ngramas_tabla_4_2[ngramas_tabla_4_2$Frecuencia!=1,]
      ngramas_tabla_4_3<-ngramas_tabla_4_3[ngramas_tabla_4_3$Frecuencia!=1,]
      ngramas_tabla_4_4<-ngramas_tabla_4_4[ngramas_tabla_4_4$Frecuencia!=1,]
      
      ngramas_tablas<-merge(ngramas_tabla_4_1,ngramas_tabla_4_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_4_3,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_4_4,by="rn",all=TRUE,merge=TRUE)
      
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      names(ngramas_tablas)<-c("Bigrama cluster 1","Frecuencia cluster 1","Bigrama cluster 2","Frecuencia cluster 2","Bigrama cluster 3","Frecuencia cluster 3","Bigrama cluster 4","Frecuencia cluster 4")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
      
    }
    else{
      ngramas_tabla_5_1<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==1,]$asunto,2)
      ngramas_tabla_5_2<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==2,]$asunto,2)
      ngramas_tabla_5_3<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==3,]$asunto,2)
      ngramas_tabla_5_4<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==4,]$asunto,2)
      ngramas_tabla_5_5<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==5,]$asunto,2)
      
      rownames(ngramas_tabla_5_1) <- NULL
      rownames(ngramas_tabla_5_2) <- NULL
      rownames(ngramas_tabla_5_3) <- NULL
      rownames(ngramas_tabla_5_4) <- NULL
      rownames(ngramas_tabla_5_5) <- NULL
      
      ngramas_tabla_5_1<-setDT(ngramas_tabla_5_1,keep.rownames=TRUE)[]
      ngramas_tabla_5_2<-setDT(ngramas_tabla_5_2,keep.rownames=TRUE)[]
      ngramas_tabla_5_3<-setDT(ngramas_tabla_5_3,keep.rownames=TRUE)[]
      ngramas_tabla_5_4<-setDT(ngramas_tabla_5_4,keep.rownames=TRUE)[]
      ngramas_tabla_5_5<-setDT(ngramas_tabla_5_5,keep.rownames=TRUE)[]
      
      #borrar filas de una frecuencia
      ngramas_tabla_5_1<-ngramas_tabla_5_1[ngramas_tabla_5_1$Frecuencia!=1,]
      ngramas_tabla_5_2<-ngramas_tabla_5_2[ngramas_tabla_5_2$Frecuencia!=1,]
      ngramas_tabla_5_3<-ngramas_tabla_5_3[ngramas_tabla_5_3$Frecuencia!=1,]
      ngramas_tabla_5_4<-ngramas_tabla_5_4[ngramas_tabla_5_4$Frecuencia!=1,]
      ngramas_tabla_5_5<-ngramas_tabla_5_5[ngramas_tabla_5_5$Frecuencia!=1,]
      
      # cambiar nombres antes porque si no no funciona el merge
      names(ngramas_tabla_5_1)<-c("rn","Bigrama cluster 1","Frecuencia cluster 1")
      names(ngramas_tabla_5_2)<-c("rn","Bigrama cluster 2","Frecuencia cluster 2")
      names(ngramas_tabla_5_3)<-c("rn","Bigrama cluster 3","Frecuencia cluster 3")
      names(ngramas_tabla_5_4)<-c("rn","Bigrama cluster 4","Frecuencia cluster 4")
      names(ngramas_tabla_5_5)<-c("rn","Bigrama cluster 5","Frecuencia cluster 5")
      
      ngramas_tablas<-merge(ngramas_tabla_5_1,ngramas_tabla_5_2,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_5_3,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_5_4,by="rn",all=TRUE,merge=TRUE)
      ngramas_tablas<-merge(ngramas_tablas,ngramas_tabla_5_5,by="rn",all=TRUE,merge=TRUE)
      
      ngramas_tablas$rn<-as.integer(ngramas_tablas$rn)
      
      ngramas_tablas<-ngramas_tablas[order(rn),]
      ngramas_tablas$rn<- NULL
      #names(ngramas_tablas)<-c("Bigrama cluster 1","Frecuencia cluster 1","Bigrama cluster 2","Frecuencia cluster 2","Bigrama cluster 3","Frecuencia cluster 3","Bigrama cluster 4","Frecuencia cluster 4","Bigrama cluster 5","Frecuencia cluster 5")
      datatable(ngramas_tablas,options = list(language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json')))
    }
    })
  
  # 6 Gráfico de clusters
  output$cluster_graficos<-renderPlotly({
    validate(need(tabla$df_data,"Por favor importe un archivo"))
    if (input$cluster_despegable=="Sin clusters"){
      ngramas_tabla_1<-contar_ngramas(nostop_cluster_1$df_data$asunto,1)
      palabra_1_1<-ngramas_tabla_1[1,c("Palabra")]
      palabra_1_2<-ngramas_tabla_1[2,c("Palabra")]
      clusters_1$df_data[clusters_1$df_data=="1"]<-sprintf("Sin cluster. Palabras más frecuentes: \n -%s\n -%s",palabra_1_1,palabra_1_2)
      plot_ly(df_bidimensional$df_data,x=~X1,y=~X2,color=factor(clusters_1$df_data),hovertext=tabla$df_data$asunto,hoverinfo="text",type="scatter",mode="markers") %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE)) %>% layout(showlegend = TRUE)
    }
    else if(input$cluster_despegable==2){
      ngramas_tabla_2_1<-contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==1,]$asunto,1)
      ngramas_tabla_2_2<-contar_ngramas(nostop_cluster_2$df_data[nostop_cluster_2$df_data$clusters_2==2,]$asunto,1)
      
      palabra_2_1_1<-ngramas_tabla_2_1[1,c("Palabra")]
      palabra_2_1_2<-ngramas_tabla_2_1[2,c("Palabra")]
      palabra_2_2_1<-ngramas_tabla_2_2[1,c("Palabra")]
      palabra_2_2_2<-ngramas_tabla_2_2[2,c("Palabra")]
      
      clusters_2$df_data[clusters_2$df_data=="1"]<-sprintf("Cluster 1. Palabras más frecuentes: \n  -%s\n  -%s",palabra_2_1_1,palabra_2_1_2)
      clusters_2$df_data[clusters_2$df_data=="2"]<-sprintf("Cluster 2. Palabras más frecuentes: \n  -%s\n  -%s",palabra_2_2_1,palabra_2_2_2)
      
      plot_ly(df_bidimensional$df_data,x=~X1,y=~X2,color=factor(clusters_2$df_data),hovertext=tabla$df_data$asunto,hoverinfo="text",type="scatter",mode="markers") %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }
    else if(input$cluster_despegable==3){
      ngramas_tabla_3_1<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==1,]$asunto,1)
      ngramas_tabla_3_2<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==2,]$asunto,1)
      ngramas_tabla_3_3<-contar_ngramas(nostop_cluster_3$df_data[nostop_cluster_3$df_data$clusters_3==3,]$asunto,1)
      
      palabra_3_1_1<-ngramas_tabla_3_1[1,c("Palabra")]
      palabra_3_1_2<-ngramas_tabla_3_1[2,c("Palabra")]
      palabra_3_2_1<-ngramas_tabla_3_2[1,c("Palabra")]
      palabra_3_2_2<-ngramas_tabla_3_2[2,c("Palabra")]
      palabra_3_3_1<-ngramas_tabla_3_3[1,c("Palabra")]
      palabra_3_3_2<-ngramas_tabla_3_3[2,c("Palabra")]
      
      clusters_3$df_data[clusters_3$df_data=="1"]<-sprintf("Cluster 1. Palabras más frecuentes: \n  -%s \n  -%s",palabra_3_1_1,palabra_3_1_2)
      clusters_3$df_data[clusters_3$df_data=="2"]<-sprintf("Cluster 2. Palabras más frecuentes: \n  -%s \n  -%s",palabra_3_2_1,palabra_3_2_2)
      clusters_3$df_data[clusters_3$df_data=="3"]<-sprintf("Cluster 3. Palabras más frecuentes: \n  -%s \n  -%s",palabra_3_3_1,palabra_3_3_2)
      
      plot_ly(df_bidimensional$df_data,x=~X1,y=~X2,color=factor(clusters_3$df_data),hovertext=tabla$df_data$asunto,hoverinfo="text",type="scatter",mode="markers") %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }
    else if(input$cluster_despegable==4){
      ngramas_tabla_4_1<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==1,]$asunto,1)
      ngramas_tabla_4_2<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==2,]$asunto,1)
      ngramas_tabla_4_3<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==3,]$asunto,1)
      ngramas_tabla_4_4<-contar_ngramas(nostop_cluster_4$df_data[nostop_cluster_4$df_data$clusters_4==4,]$asunto,1)
      
      palabra_4_1_1<-ngramas_tabla_4_1[1,c("Palabra")]
      palabra_4_1_2<-ngramas_tabla_4_1[2,c("Palabra")]
      palabra_4_2_1<-ngramas_tabla_4_2[1,c("Palabra")]
      palabra_4_2_2<-ngramas_tabla_4_2[2,c("Palabra")]
      palabra_4_3_1<-ngramas_tabla_4_3[1,c("Palabra")]
      palabra_4_3_2<-ngramas_tabla_4_3[2,c("Palabra")]
      palabra_4_4_1<-ngramas_tabla_4_4[1,c("Palabra")]
      palabra_4_4_2<-ngramas_tabla_4_4[2,c("Palabra")]
      
      clusters_4$df_data[clusters_4$df_data=="1"]<-sprintf("Cluster 1. Palabras más frecuentes: \n  -%s \n  -%s",palabra_4_1_1,palabra_4_1_2)
      clusters_4$df_data[clusters_4$df_data=="2"]<-sprintf("Cluster 2. Palabras más frecuentes: \n  -%s \n  -%s",palabra_4_2_1,palabra_4_2_2)
      clusters_4$df_data[clusters_4$df_data=="3"]<-sprintf("Cluster 3. Palabras más frecuentes: \n  -%s \n  -%s",palabra_4_3_1,palabra_4_3_2)
      clusters_4$df_data[clusters_4$df_data=="4"]<-sprintf("Cluster 4. Palabras más frecuentes: \n  -%s \n  -%s",palabra_4_4_1,palabra_4_4_2)
    
      plot_ly(df_bidimensional$df_data,x=~X1,y=~X2,color=factor(clusters_4$df_data),hovertext=tabla$df_data$asunto,hoverinfo="text",type="scatter",mode="markers") %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
      }
    else{
      ngramas_tabla_5_1<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==1,]$asunto,1)
      ngramas_tabla_5_2<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==2,]$asunto,1)
      ngramas_tabla_5_3<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==3,]$asunto,1)
      ngramas_tabla_5_4<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==4,]$asunto,1)
      ngramas_tabla_5_5<-contar_ngramas(nostop_cluster_5$df_data[nostop_cluster_5$df_data$clusters_5==5,]$asunto,1)
      
      palabra_5_1_1<-ngramas_tabla_5_1[1,c("Palabra")]
      palabra_5_1_2<-ngramas_tabla_5_1[2,c("Palabra")]
      palabra_5_2_1<-ngramas_tabla_5_2[1,c("Palabra")]
      palabra_5_2_2<-ngramas_tabla_5_2[2,c("Palabra")]
      palabra_5_3_1<-ngramas_tabla_5_3[1,c("Palabra")]
      palabra_5_3_2<-ngramas_tabla_5_3[2,c("Palabra")]
      palabra_5_4_1<-ngramas_tabla_5_4[1,c("Palabra")]
      palabra_5_4_2<-ngramas_tabla_5_4[2,c("Palabra")]
      palabra_5_5_1<-ngramas_tabla_5_5[1,c("Palabra")]
      palabra_5_5_2<-ngramas_tabla_5_5[2,c("Palabra")]
      
      clusters_5$df_data[clusters_5$df_data=="1"]<-sprintf("Cluster 1. Palabras más frecuentes: \n  -%s \n  -%s",palabra_5_1_1,palabra_5_1_2)
      clusters_5$df_data[clusters_5$df_data=="2"]<-sprintf("Cluster 2. Palabras más frecuentes: \n  -%s \n  -%s",palabra_5_2_1,palabra_5_2_2)
      clusters_5$df_data[clusters_5$df_data=="3"]<-sprintf("Cluster 3. Palabras más frecuentes: \n  -%s \n  -%s",palabra_5_3_1,palabra_5_3_2)
      clusters_5$df_data[clusters_5$df_data=="4"]<-sprintf("Cluster 4. Palabras más frecuentes: \n  -%s \n  -%s",palabra_5_4_1,palabra_5_4_2)
      clusters_5$df_data[clusters_5$df_data=="5"]<-sprintf("Cluster 5. Palabras más frecuentes: \n  -%s \n  -%s",palabra_5_5_1,palabra_5_5_2)
      
      plot_ly(df_bidimensional$df_data,x=~X1,y=~X2,color=factor(clusters_5$df_data),hovertext=tabla$df_data$asunto,hoverinfo="text",type="scatter",mode="markers") %>% config(displayModeBar = F) %>% layout(xaxis=list(fixedrange=TRUE)) %>% layout(yaxis=list(fixedrange=TRUE))
    }
  })
  
    output$descargar_tabla_cluster<-downloadHandler(
      filename=function(){
        if (input$cluster_despegable=="Sin clusters"){
        "Tabla original sin clusters.csv"
        }
        else if(input$cluster_despegable==2){
          "Tabla original con 2 clusters.csv"
        }
        else if(input$cluster_despegable==3){
          "Tabla original con 3 clusters.csv"
        }
        else if(input$cluster_despegable==4){
          "Tabla original con 4 clusters.csv"
        }
        else{
          "Tabla original con 5 clusters.csv"
        }
        
      },content=function(file){
        if (input$cluster_despegable=="Sin clusters"){
          tabla_out<-tabla$df_data
          names(tabla_out)<-c("PQRSD")
          write.csv(tabla_out,file, row.names = FALSE)
        }
        else if(input$cluster_despegable==2){
          tabla_out<-texto_cluster_2$df_data
          names(tabla_out)<-c("PQRSD","Cluster")
          write.csv(tabla_out,file, row.names = FALSE)
        }
        else if(input$cluster_despegable==3){
          tabla_out<-texto_cluster_3$df_data
          names(tabla_out)<-c("PQRSD","Cluster")
          write.csv(tabla_out,file, row.names = FALSE)
        }
        else if(input$cluster_despegable==4){
          tabla_out<-texto_cluster_4$df_data
          names(tabla_out)<-c("PQRSD","Cluster")
          write.csv(tabla_out,file, row.names = FALSE)
        }
        else{
          tabla_out<-texto_cluster_5$df_data
          names(tabla_out)<-c("PQRSD","Cluster")
          write.csv(tabla_out,file, row.names = FALSE)
        }
      }
    )
  
 
}

shinyApp(ui, server)

############ cluster
#### T-SNE. Primero bag of words y luego reducción de dimensionalidad con t-sne
#### luego de bag of words. Clustering con distancias de coseno (es similitud más que distancia)
#### mirar doc2vec pero no hay en R (buscar) 

#### antes de hacer bag of words, hacer stemming
#### antes de hacer bag of words (y después de stemming), borrar palabras de menos de 2 veces (mirar si jugar con eso, Sebastián borra con 10)


# sacar palabras y bigramas más frecuentes por cluster

# update select input

# texto: hovertext en plotly
# poner as factor en plotly para que paleta sea discreta

# data table output

#runApp('D:/OneDrive - Departamento Nacional de Planeacion/Shiny/script_aplicación.R')