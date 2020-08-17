# librerias ---------------------------------------------------------------
install.packages("dplyr")

library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
library(timevis)
library(scales)
library(DT)

load("Datos_generales.Rdata")
load("tablas_qa.Rdata")
load("tabla_riesgos.Rdata")
load("tabla_esfuerzo.Rdata")
load("tabla_avance.Rdata")
load("tabla_montos.Rdata")
load("tabla_estatus.Rdata")

actualizacion <- Sys.time()

ln <- "Qualtop Business Improvement"  # para QBI usar "Qualtop Business Improvement", para sw poner "Qualtop Software"

ln2 <- ifelse(ln == "Qualtop Software","Software","QBI")

############ header ##########################

header <-  dashboardHeader(title = ln,
                           dropdownMenu(type = "notifications",
                                        notificationItem(
                                          text = "Gracias por usar nuestra aplicación",
                                          icon = icon("angellist"),
                                          status = "success"
                                        ),
                                        notificationItem(
                                          text = paste("Actualización:",actualizacion),
                                          icon = icon("upload"),
                                          status = "success"
                                        )
                           )
)

############ menu sidebar #########################

sidebar <-  dashboardSidebar(
  sidebarMenu(
    menuItem("Resumen", icon = icon("bar-chart-o"), tabName = "subitem1"),
    menuItem("Proyectos", icon = icon("users"), tabName = "subitem2"),
    menuItem("QA", icon = icon("award"), tabName = "subitem3"),
    menuItem("Sugerencias y mejoras", icon = icon("file-code-o"), 
             href = "https://qualtopgroup.atlassian.net/servicedesk/customer/portal/3/create/41")
  ))



# Resumen proyectos Software ---------------------------------------------------



tabitem1 <- tabItem(tabName = "subitem1",
                    fluidRow(
                      box(width=12, status = "primary", collapsible = TRUE, 
                          
                          h2("Estatus Global de Proyectos"),
                          DT::dataTableOutput("mytable"), style = "height:900px;overflow-x: scroll;")
                    )
)


# Detalle proyecto software--------------------------------------------------------
Tabla_Proyectos <- Tabla_Dat_generales[Tabla_Dat_generales$linea_de_negocio == ln,]

lista1 <- unique(Tabla_Proyectos$resumen)

tabitem2 <- tabItem(tabName = "subitem2",
                    fluidRow(
                      box(selectInput("Entrada1", "Proyecto", choices = lista1),
                          width = 3,solidHeader = TRUE),
                      box(width=3, height = 100,solidHeader = TRUE,
                          textOutput("texto1_2"),
                          textOutput("texto2_2"),
                          textOutput("texto3_2")),
                      infoBoxOutput("progressBox", width = 3),
                      infoBoxOutput("progressBox2", width = 3)),
                    fluidRow(
                      box(width=3, height = 450, solidHeader = TRUE,
                          highchartOutput("esfuerzo")),
                      box(width=3, height = 450, solidHeader = TRUE,
                          highchartOutput("avance")),
                      box(width = 6,height = 450, solidHeader = TRUE,
                          box(status = "primary", height = 400,solidHeader = TRUE,
                              highchartOutput("ingreso_pl")),
                          box(status = "primary", height = 400,solidHeader = TRUE,
                              highchartOutput("ingreso_re")))
                    ),
                    fluidRow(
                      
                      box(width=3, status = "primary", height = 450,
                          highchartOutput("ingreso_mensual")),
                      box(width=3, status = "primary", height = 450,
                          highchartOutput("egreso_mensual")),
                      box(width=3, status = "primary", height = 450,
                          highchartOutput("ingreso_acum")),
                      box(width=3, status = "primary", height = 450,
                          highchartOutput("egreso_acum")),
                      box(uiOutput("segundaOpcion"), width = 4, height = 100),
                      box(width=4, height = 100,solidHeader = TRUE,
                          "Ingresos y egresos reales al mes",
                          textOutput("texto1_3"),
                          textOutput("texto2_3")),
                      box(width=4, height = 100,solidHeader = TRUE,
                          "Ingresos y egresos acumulados al mes",
                          textOutput("texto1_4"),
                          textOutput("texto2_4")),
                      box(width = 8, status = "primary",
                          timevisOutput("grafico_hitos")),
                      box(width = 4, status = "primary",
                          tableOutput("table_hitos")),
                      box(width = 12, status = "primary",
                          "MONITOREO DE RIESGOS",
                          tableOutput("table_riesgos")),
                      box(width = 12, status = "primary",
                          "MONITOREO DE NO CONFORMIDADES",
                          textOutput("texto1_qa"),
                          tableOutput("table_qa"))
                    )
)

#lista3 <- unique(t_auditorias$unidad_de_negocio)

lista3 <- unique(t_auditorias$unidad_de_negocio)


tabitem3 <- tabItem(tabName = "subitem3",
                    fluidRow(
                      # linea de negocio y mes
                      box(width = 6,solidHeader = TRUE,selectInput("line", "Linea de negocio",
                                                                   choices = lista3,
                                                                   selected = ln2)),
                      box(width = 6,solidHeader = TRUE,selectizeInput("inmes", "Selecciona los meses",
                                                                      multiple = TRUE,
                                                                      choices = 1:12,
                                                                      selected = "5")),
                      box(width = 6, height = 450,
                          highchartOutput("qa1"),
                          footer = "Incluye los resultados de los meses seleccionados",
                          solidHeader = TRUE),
                      box(width = 6, height = 450, 
                          highchartOutput("qa2"),
                          footer = "Incluye los resultados de los meses seleccionados",
                          solidHeader = TRUE),
                      box(width = 12, height = 480, solidHeader = TRUE,
                          box(width = 6, height = 450, 
                              highchartOutput("qa3"),
                              footer = "Muestra todas las no conformidades, independiente al mes seleccionado",
                              solidHeader = TRUE),
                          box(width = 6, height = 450, 
                              highchartOutput("qa4"),
                              footer = "Considera los resultados de 2019 agrupado por mes",
                              solidHeader = TRUE))
                    )
)


########### Body dashboard #########

body <-  dashboardBody(
  tags$head(
    tags$link( 
      rel = "shortcut icon", 
      type = "image/png", 
      href = "https://media.licdn.com/dms/image/C560BAQE12eIlprMOcA/company-logo_200_200/0?e=2159024400&v=beta&t=kRSuYglOBSJKrCmzOP87ZuOuaeGCdz-TqejFBoa5JrE"
    ),
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js")
  ),
  tabItems(
    tabitem1,
    tabitem2,
    tabitem3
  )
)

############ server #############

server= function(input, output) {
  
  ############ textos de resumen #############################################
  
  #----------- tab de estatus global
  
  output$mytable = DT::renderDataTable({
    if(ln == "Qualtop Software"){
      datatable(global_sw, options = list(pageLength = 50)) %>% 
        formatCurrency(c('ingreso', 'egreso', 'margen'), '$') %>%
        formatStyle('resumen', fontWeight = 'bold') %>%
        formatStyle('dif_avance',
                    color = styleInterval(c(5, 10), c('white', 'blue', 'white')),
                    backgroundColor = styleInterval(c(5, 10), c('green', 'yellow', 'red'))
        ) %>%
        formatStyle('dif_esfuerzo',
                    color = styleInterval(0, c('white', 'white')),
                    backgroundColor = styleInterval(0, c('red', 'green'))
        ) %>%
        formatStyle('margen',
                    color = styleInterval(0, c('white', 'white')),
                    backgroundColor = styleInterval(0, c('red', 'green'))
        )
    } else{
      datatable(global_qbi, options = list(pageLength = 50)) %>% 
        formatCurrency(c('ingreso', 'egreso', 'margen'), '$') %>%
        formatStyle('resumen', fontWeight = 'bold') %>%
        formatStyle('dif_avance',
                    color = styleInterval(c(-15, -10), c('white', 'blue', 'white')),
                    backgroundColor = styleInterval(c(-15, -10), c('red', 'yellow', 'green'))
        ) %>%
        formatStyle('dif_esfuerzo',
                    color = styleInterval(c(-15, -10), c('white', 'blue', 'white')),
                    backgroundColor = styleInterval(c(-15, -10), c('red', 'yellow', 'green'))
        ) %>%
        formatStyle('margen',
                    color = styleInterval(0, c('white', 'white')),
                    backgroundColor = styleInterval(0, c('red', 'green'))
        )
    }
    
  })
  
  
  #----------- tab de proyectos
  
  datos <- reactive({
    Tabla_Dat_generales[Tabla_Dat_generales$proyecto == input$Entrada1,]
  })
  
  #muestra el resumen del proyecto
  output$texto1_2 <- renderText({
    paste("Lider:", unique(datos()$responsable))
  })
  
  output$texto2_2 <- renderText({
    paste("Cliente:",   unique(datos()$`cliente(s)`))
  })
  
  output$texto3_2 <- renderText({
    paste("Clave:", unique(datos()$clave))
  })
  
  # muestra el avance en fechas
  output$progressBox <- renderInfoBox({
    infoBox( "Fecha de inicio",
             datos()$fecha_inicio_real, icon = icon("hourglass-start"),
             color = "light-blue"
    )
  })
  
  output$progressBox2 <- renderInfoBox({
    infoBox("Fecha fin planeada",
            datos()$fecha_fin_planificada,  icon = icon("hourglass-end"),
            color = "light-blue"
    )
  })
  
  ########################### esfuerzo #############################################
  
  tablaesfuerzo <- reactive({
    mesfuerzo[mesfuerzo$proyecto == input$Entrada1,]
  })
  
  tabla_avance <- reactive({
    mesfuerzo1[mesfuerzo1$proyecto == input$Entrada1,]
  })
  
  output$esfuerzo <- if( ln == "Qualtop Software"){
    renderHighchart({
      highchart() %>% 
        hc_title(text = "Esfuerzo",
                 style = list(fontWeight = "bold")) %>%
        hc_chart(zoomType = "x") %>% 
        hc_add_theme(hc_theme_gridlight())%>%
        hc_add_series(data =round((tabla_avance()$horas_totales), 2),
                      dataLabels = list(enabled = TRUE,
                                        style= list(fontFamily = "Verdana"),
                                        format = "{point.y:,.0f}"),
                      type = "column",
                      name = "Horas Planeadas Totales",
                      color = "#BABABA", 
                      showInLegend = TRUE, 
                      stacking = 0)%>%
        hc_add_series(data = round(tabla_avance()$planeado_actual, 4), 
                      dataLabels = list(enabled = TRUE, 
                                        style= list(fontFamily = "Verdana"),
                                        format = "{point.y:,.0f}"),
                      type = "column",
                      name = "Horas planeadas a la fecha",
                      color = "#EE7600", 
                      showInLegend = TRUE, 
                      stacking = 0)%>%
        hc_add_series(data = round(tabla_avance()$esfuerzo_actual, 4), 
                      dataLabels = list(enabled = TRUE, 
                                        style= list(fontFamily = "Verdana"),
                                        format = "{point.y:,.0f}"),
                      type = "column",
                      name = "Horas Reales",
                      color = "#EE7600", 
                      showInLegend = TRUE, 
                      stacking = 0)
    }) 
    
  } else {
    
    renderHighchart({
      highchart() %>% 
        hc_chart(type = "line") %>%
        hc_title(text = "Seguimiento de esfuerzo ",margin = 15,style = list(color = "#144746", useHTML = TRUE)) %>%
        hc_add_series(data = tablaesfuerzo()$Lo.90, type="line", name = "Low 90", color= "red") %>%
        hc_add_series(data = tablaesfuerzo()$Hi.90, type="line", name = "High 90", color= "red") %>%
        hc_add_series(data = tablaesfuerzo()$Point.Forecast, type="line", name = "Esperados", color= "salmon") %>%
        hc_add_series(data = tablaesfuerzo()$horas_totales, type="line", name = "Horas totales planeadas", color= "green") %>%
        hc_add_series(data = tablaesfuerzo()$avance_planeado, type="line", name = "Horas planeadas a la fecha", color= "blue") %>%
        hc_add_series(data = tablaesfuerzo()$revision, type="column", name = "Horas reales al dia de hoy", color= "red") %>%
        hc_xAxis(categories = tablaesfuerzo()$total_dias) %>%
        hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>
                   <br> {point.y:,.0f} hrs<br/>",
                   shared = TRUE) 
    })
    
  }
  
  
  ########################### avance #############################################
  
  tablapuntos <- reactive({
    mpuntos[mpuntos$proyecto == input$Entrada1,]
  })
  
  tabla_puntos2 <- reactive({
    mpuntos1[mpuntos1$proyecto == input$Entrada1,]
  })
  
  output$avance <- if( ln == "Qualtop Software"){
    renderHighchart({
      highchart() %>% 
        hc_title(text = "Seguimiento de avance",
                 style = list(fontWeight = "bold")) %>%
        hc_chart(zoomType = "x") %>% 
        hc_add_theme(hc_theme_gridlight())%>%
        hc_add_series(data =round((tabla_puntos2()$planeado_actual), 1),
                      dataLabels = list(enabled = TRUE,
                                        style= list(fontFamily = "Verdana"),
                                        format = "{point.y:,.0f}"),
                      type = "column",
                      name = "Avance Planeado a la Fecha",
                      color = "#BABABA", 
                      showInLegend = TRUE, 
                      stacking = 0)%>%
        hc_add_series(data = round(tabla_puntos2()$esfuerzo_actual, 1), 
                      dataLabels = list(enabled = TRUE, 
                                        style= list(fontFamily = "Verdana"),
                                        format = "{point.y:,.0f}"),
                      type = "column",
                      name = "Avance Real a la Fecha",
                      color = "#EE7600", 
                      showInLegend = TRUE, 
                      stacking = 0)
    }) 
    
  } else {
    
    renderHighchart({
      highchart() %>% 
        hc_chart(type = "line") %>%
        hc_title(text = "Seguimiento de avance",margin = 15,style = list(color = "#144746", useHTML = TRUE)) %>%
        hc_add_series(data = tablapuntos()$Lo.90, type="spline", name = "Low 90", color= "red") %>%
        hc_add_series(data = tablapuntos()$Hi.90, type="spline", name = "High 90", color= "red") %>%
        hc_add_series(data = tablapuntos()$Point.Forecast, type="spline", name = "Esperados", color= "salmon") %>%
        hc_add_series(data = tablapuntos()$total_puntos, type="spline", name = "Puntos Totales", color= "green") %>%
        hc_add_series(data = tablapuntos()$avance_planeado_r, type="spline", name = "Avance planeado a la fecha", color= "blue") %>%
        hc_add_series(data = tablapuntos()$revision, type="column", name = "Puntos al dia de hoy", color= "red") %>%
        hc_xAxis(categories = tablapuntos()$total_dias) %>%
        hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>
                   <br> {point.y:,.0f} pts<br/>",
                   shared = TRUE) 
    })
    
  }
  
  
  ###################  Graficos de montos #######################
  
  # ingreso plan----------------------------------------------------------------
  tabla_ingreso <- reactive({
    tabla_acum_pl[tabla_acum_pl$resumen == input$Entrada1,]
  })
  output$ingreso_pl = renderHighchart({
    highchart() %>% 
      hc_title(text = "Presupuesto Plan",
               style = list(fontWeight = "bold")) %>%
      hc_chart(zoomType = "x") %>% 
      hc_add_theme(hc_theme_gridlight())%>%
      hc_add_series(data =round((tabla_ingreso()$ingreso_acum_pl), 2),
                    dataLabels = list(enabled = TRUE,
                                      style= list(fontFamily = "Verdana"),
                                      format = "{point.y:,.0f}"),
                    type = "column",
                    name = "Ingreso planeado",
                    color = "#BABABA", 
                    showInLegend = TRUE, 
                    stacking = 0)%>%
      hc_add_series(data =round((tabla_ingreso()$egreso_acum_pl), 2),
                    dataLabels = list(enabled = TRUE,
                                      style= list(fontFamily = "Verdana"),
                                      format = "{point.y:,.0f}"),
                    type = "column",
                    name = "Egreso planeado",
                    color = "#FF9966", 
                    showInLegend = TRUE, 
                    stacking = 0)%>%
      hc_add_series(data = round(tabla_ingreso()$margen, 4), 
                    dataLabels = list(enabled = TRUE, 
                                      style= list(fontFamily = "Verdana"),
                                      format = "{point.y:,.0f}"),
                    type = "column",
                    name = "Margen",
                    color = "#EE7600", 
                    showInLegend = TRUE, 
                    stacking = 0)
  }) 
  # ingreso real----------------------------------------------------------------
  ingreso_real <- reactive({
    egresos_proyecto_total[egresos_proyecto_total$resumen == input$Entrada1,]
  })
  output$ingreso_re = renderHighchart({
    highchart() %>% 
      hc_title(text = "Real",
               style = list(fontWeight = "bold")) %>%
      hc_chart(zoomType = "x") %>% 
      hc_add_theme(hc_theme_gridlight())%>%
      hc_add_series(data =round((ingreso_real()$ingreso_dev_t), 2),
                    dataLabels = list(enabled = TRUE,
                                      style= list(fontFamily = "Verdana"),
                                      format = "{point.y:,.0f}"),
                    type = "column",
                    name = "Ingreso devengado",
                    color = "#BABABA", 
                    showInLegend = TRUE, 
                    stacking = 0)%>%
      hc_add_series(data =round((ingreso_real()$CostosDirectos), 2),
                    dataLabels = list(enabled = TRUE,
                                      style= list(fontFamily = "Verdana"),
                                      format = "{point.y:,.0f}"),
                    type = "column",
                    name = "Egreso real",
                    color = "#FF9966", 
                    showInLegend = TRUE, 
                    stacking = 0)%>%
      hc_add_series(data = round(ingreso_real()$margen, 4), 
                    dataLabels = list(enabled = TRUE, 
                                      style= list(fontFamily = "Verdana"),
                                      format = "{point.y:,.0f}"),
                    type = "column",
                    name = "Margen",
                    color = "#EE7600", 
                    showInLegend = TRUE, 
                    stacking = 0)
  }) 
  
  
  costos_pl <- reactive({
    tabla_finanzas[tabla_finanzas$resumen == input$Entrada1,]
  })
  
  
  output$ingreso_mensual = renderHighchart({
    highchart(height = 400) %>%  
      hc_title(text = "Seguimiento de Ingresos",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE))%>%
      hc_add_series(data = costos_pl()$ingresos_planeados, type="column", name = "Ingresos Planeados", color= "lightgreen",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_add_series(data = costos_pl()$ingreso_devengado, type="column", name = "Ingresos devengados", color= "salmon",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_xAxis(categories = costos_pl()$ancla) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>
                 <br> $ {point.y:,.0f}<br/>", shared = TRUE)
  })
  
  output$egreso_mensual = renderHighchart({
    highchart(height = 400) %>%  
      hc_title(text = "Seguimiento de Egresos",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE))%>%
      hc_add_series(data = costos_pl()$egresos_planeados, type="column", name = "Egresos Planeados", color= "lightgreen",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_add_series(data = costos_pl()$CostosDirectos, type="column", name = "Egresos Reales", color= "salmon",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_xAxis(categories = costos_pl()$ancla) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>
                 <br> $ {point.y:,.0f}<br/>", shared = TRUE)
  })
  
  
  output$ingreso_acum = renderHighchart({
    highchart(height = 400) %>%  
      hc_title(text = "Seguimiento de Ingresos Acumulado",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE))%>%
      hc_add_series(data = costos_pl()$ingresos_plan_acum, type="column", name = "Ingresos Planeados", color= "lightgreen",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_add_series(data = costos_pl()$ingresos_dev_acum, type="column", name = "Ingresos devengados", color= "salmon",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_xAxis(categories = costos_pl()$ancla) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>
                 <br> $ {point.y:,.0f}<br/>", shared = TRUE)
  })
  
  output$egreso_acum = renderHighchart({
    highchart(height = 400) %>%  
      hc_title(text = "Seguimiento de Egreso Acumulado",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE))%>%
      hc_add_series(data = costos_pl()$egresos_plan_acum, type="column", name = "Egresos Planeados", color= "lightgreen",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_add_series(data = costos_pl()$egresos_dev_acum, type="column", name = "Egresos Reales", color= "salmon",
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}")) %>%
      hc_xAxis(categories = costos_pl()$ancla) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\">{series.name}</span>
                 <br> $ {point.y:,.0f}<br/>", shared = TRUE)
  })
  
  
  ################### boxe datos financieros #################################
  
  output$segundaOpcion <- renderUI({
    selectInput("Entrada2", "Selecciona el mes para visualizar detalle", choices = as.character(tabla_finanzas[tabla_finanzas$resumen==input$Entrada1, 14]))
  })
  
  datos_financieros<- reactive({
    tabla_finanzas %>%
      filter(tabla_finanzas$resumen %in% input$Entrada1 & tabla_finanzas$ancla %in% input$Entrada2)
  })
  
  output$texto1_3 <- renderText({
    paste("Ingresos:", dollar_format()(datos_financieros()$ingreso_devengado))
  })
  
  output$texto2_3 <- renderText({
    paste("Egresos:", dollar_format()(datos_financieros()$CostosDirectos))
  })  
  
  output$texto1_4 <- renderText({
    paste("Ingresos acumulados:", dollar_format()(datos_financieros()$ingresos_dev_acum))
  })
  
  output$texto2_4 <- renderText({
    paste("Egresos acumulados:", dollar_format()(datos_financieros()$egresos_dev_acum))
  })
  
  
  
  ###################  Graficos de hitos #######################
  
  hitos <- reactive({
    timevis(tabla_hitos[tabla_hitos$resumen == input$Entrada1, c("content","start","end")])
  })
  
  output$grafico_hitos = renderTimevis({
    hitos()
  })
  
  
  output$table_hitos <- renderTable({
    actividades_cerradas[actividades_cerradas$resumen == input$Entrada1,c("historia", "fecha_resuelto")]
  })
  
  
  ######################### Tabla de riesgos ############################  
  
  output$table_riesgos <- renderTable({
    t_riesgos[t_riesgos$resumen == input$Entrada1,c("descripcion","impacto","exposicion","plan_de_accion")]
    
  })
  
  ######################### Tabla de QA ############################ 
  
  datosqa <- reactive({
    t_auditorias[t_auditorias$proyecto == input$Entrada1,]
  })
  
  #muestra el resumen del proyecto
  output$texto1_qa <- renderText({
    paste("Apego a Procesos:", unique(datosqa()$resultado))
  })
  
  output$table_qa <- renderTable({
    t_noconformidades[t_noconformidades$proyecto == input$Entrada1,c("resumen","responsable","estado","seguimiento_a_nc")]
    
  })
  
  ######################### Reporte de QA ############################
  
  # grafico1 de pay 
  
  tablaqa <- reactive({
    t_auditorias %>%
      filter(t_auditorias$mes %in% input$inmes & t_auditorias$unidad_de_negocio %in% input$line)%>%
      count(clasificacion)
  })
  
  output$qa1 = renderHighchart({
    highchart(height = 650) %>% 
      hc_chart(type = "pie") %>% 
      hc_add_series_labels_values(labels = tablaqa()$clasificacion, 
                                  values = tablaqa()$n,
                                  showInLegend = TRUE     ) %>% 
      hc_tooltip(crosshairs = TRUE, 
                 borderWidth = 5, 
                 sort = TRUE, 
                 shared = TRUE, 
                 table = TRUE,
                 pointFormat = paste('</b><br> {point.percentage: .1f} % </b><br>Cantidad: <b>{point.y:3.1f}</b>'))%>% 
      hc_title(text = "Clasificación de resultados",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE)) %>% 
      hc_add_theme(hc_theme_gridlight()) 
  }) 
  
  
  # grafico2 de barras 
  
  tablaqa2 <- reactive({
    t_auditorias %>%
      filter(t_auditorias$mes %in% input$inmes & t_auditorias$unidad_de_negocio %in% input$line)
  })
  
  output$qa2 = renderHighchart({
    highchart(height = 650) %>%  
      hc_title(text = "% de Apego a procesos",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE))%>%
      hc_add_series(data = tablaqa2()$resultado,
                    type = "column",
                    color = "tomato",
                    name = "% de apego a proceso",
                    pointPadding= 0.2,
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}"),
                    PointPlacement= -0.2) %>%
      hc_add_series(data = tablaqa2()$objetivo,
                    type = "line",
                    color = "red",
                    name = "Objetivo") %>%
      hc_xAxis(categories = tablaqa2()$proyecto) %>%
      hc_yAxis(max = 100) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\"></span><br>{point.x:,.0f} <br/>", shared = TRUE)%>%
      hc_add_theme(hc_theme_gridlight())
  }) 
  
  
  # grafico3  de lineas 
  
  tablaqa3 <- reactive({
    t_noconformidades1 %>%
      filter(t_noconformidades1$unidad_de_negocio %in% input$line)
  })
  
  output$qa3 = renderHighchart({
    highchart(height = 650) %>%  
      hc_title(text = "No conformidades abiertas",
               margin = 15,
               style = list(color = "#144746", useHTML = TRUE))%>%
      hc_add_series(data = tablaqa3()$n,
                    type = "column",
                    color = "tomato",
                    name = "Cantidad de no conformidades",
                    pointPadding= 0.2,
                    dataLabels = list(enabled = TRUE,
                                      format = "{point.y:,.0f}"),
                    PointPlacement= -0.2) %>%
      hc_xAxis(categories = tablaqa3()$resumen) %>%
      hc_tooltip(pointFormat = "<span style=\"color:{series.color}\"></span>
                 <br> {point.y:,.0f}<br/>", shared = TRUE)%>%
      hc_add_theme(hc_theme_gridlight())
    
    
  }) 
  # grafico de box 
  
  tablaqa4 <- reactive({
    t_auditorias %>%
      filter(t_auditorias$unidad_de_negocio %in% input$line)
    
  })
  
  output$qa4 = renderHighchart({
    highchart() %>% 
      highchart() %>%
      hc_add_series_boxplot(tablaqa4()$resultado, tablaqa4()$mes, colors = c("#FF6A6A","#A1A1A1","#6CA6CD", "#00CD66"), showInLegend = FALSE) %>%
      hc_title(text = "Diagrama de caja ",margin = 15,style = list(color = "#144746", useHTML = TRUE)) %>% 
      hc_yAxis(title = "y", min = 0, max = 100) %>%
      hc_plotOptions(boxplot = list(colorByPoint = TRUE), scatter = list(color = c("#A6BBC8")))
  }) 
  
  
}

shinyApp(ui = dashboardPage(skin = "red",header, sidebar, body, title = ln2), server)