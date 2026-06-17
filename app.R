library(shiny)
library(bslib)
library(googlesheets4)
library(ggplot2)
library(DT)

SHEET_ID <- "1e0GRCNDVOphb4em7p5OkbXc_W3mDLKxjQsRHo__I3vs"
# Auth con service account.
gs4_auth(path = Sys.getenv("CRONO_KEY"))

proyectos_conocidos <- c("PLMS", "ROM", "PROTEGIC", "BAH", "PARSTATS")

fmt_hm <- function(m) sprintf("%dh %02dm", round(m) %/% 60, round(m) %% 60)

ui <- page_navbar(
  title = "Log-laboral",
  
  nav_panel(
    "Carga",
    card(
      card_header("Cargar tarea"),
      dateInput("fecha", "Fecha", value = Sys.Date(), weekstart = 1, language = "es"),
      selectizeInput(
        "proyecto", "Proyecto", choices = proyectos_conocidos,
        options = list(create = TRUE, placeholder = "Elegi o escribi uno nuevo")
      ),
      textInput("tarea", "Tarea"),
      layout_columns(
        col_widths = c(6, 6),
        numericInput("horas", "Horas", value = 0, min = 0, step = 1),
        numericInput("minutos", "Min", value = 0, min = 0, max = 59, step = 5)
      ),
      checkboxInput("es_reunion", "Es reunion", value = FALSE),
      actionButton("agregar", "Agregar", class = "btn-primary")
    )
  ),
  
  nav_panel(
    "Diario",
    layout_columns(
      col_widths = c(4, 8),
      
    card(card_header("Totales por dia"), dataTableOutput("tabla_diaria")),
    card(card_header("Detalle cargado"), dataTableOutput("tabla_detalle"))
    )
  ),
  
  nav_panel(
    "Semanal",
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
        card_header("Resumen semanal"),
        DT::dataTableOutput("tabla_semanal")
      ),
      
      card(
        card_header("Por proyecto"),
        DT::dataTableOutput("tabla_proyecto")
      )
    ),
    
    card(
      card_header("Tiempo por proyecto"),
      plotOutput("torta", height = "500px")
    )
  )
)

server <- function(input, output, session) {
  
  datos <- reactiveVal(read_sheet(SHEET_ID, col_types = "Dccnl"))
  
  observeEvent(input$agregar, {
    nueva <- data.frame(
      fecha = input$fecha, proyecto = input$proyecto, tarea = input$tarea,
      minutos = input$horas * 60 + input$minutos, es_reunion = input$es_reunion,
      stringsAsFactors = FALSE
    )
    sheet_append(SHEET_ID, nueva)
    datos(rbind(datos(), nueva))
    updateTextInput(session, "tarea", value = "")
    updateNumericInput(session, "horas", value = 0)
    updateNumericInput(session, "minutos", value = 0)
    updateCheckboxInput(session, "es_reunion", value = FALSE)
  })
  
  output$tabla_diaria <- renderDataTable({
    agg <- aggregate(minutos ~ fecha, datos(), sum)
    agg <- agg[order(agg$fecha), ]
    data.frame(Fecha = format(agg$fecha, "%a %d/%m"), Total = fmt_hm(agg$minutos))
  },
  rownames = FALSE,
  options = list(
    dom = 't',
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE,
    scrollX = TRUE
  ))
  
  output$tabla_proyecto <- renderDataTable({
    d <- datos()
    tot  <- aggregate(minutos ~ proyecto, d, sum)
    dias <- aggregate(fecha ~ proyecto, d, function(x) length(unique(x)))
    names(dias)[2] <- "dias"
    m <- merge(tot, dias, by = "proyecto")
    m <- m[order(-m$minutos), ]
    data.frame(Proyecto = m$proyecto, Total = fmt_hm(m$minutos),
               `Prom./dia` = fmt_hm(m$minutos / m$dias), check.names = FALSE)
  },
  rownames = FALSE,
  options = list(
    dom = 't',
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE,
    scrollX = TRUE
  ))
  
  output$tabla_semanal <- renderDataTable({
    d <- datos()
    d$semana <- format(d$fecha, "%G-S%V")
    tot  <- aggregate(minutos ~ semana, d, sum)
    dias <- aggregate(fecha ~ semana, d, function(x) length(unique(x)))
    names(dias)[2] <- "dias"
    reu  <- aggregate(minutos ~ semana, d[d$es_reunion, ], sum)
    names(reu)[2] <- "reu_min"
    m <- merge(tot, dias, by = "semana")
    m <- merge(m, reu, by = "semana", all.x = TRUE)
    m$reu_min[is.na(m$reu_min)] <- 0
    m <- m[order(m$semana), ]
    data.frame(Semana = m$semana, Total = fmt_hm(m$minutos),
               `Prom./dia` = fmt_hm(m$minutos / m$dias),
               `% reuniones` = sprintf("%.1f%%", 100 * m$reu_min / m$minutos),
               check.names = FALSE)
  },
  rownames = FALSE,
  options = list(
    dom = 't',
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE,
    scrollX = TRUE
  ))
  
  output$tabla_detalle <- renderDataTable({
    d <- datos()
    d <- d[order(d$fecha), ]
    data.frame(Fecha = format(d$fecha, "%d/%m"), Proyecto = d$proyecto, Tarea = d$tarea,
               Tiempo = fmt_hm(d$minutos), Reunion = ifelse(d$es_reunion, "si", ""),
               check.names = FALSE)
  },
  rownames = FALSE,
  options = list(
    dom = 't',
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = FALSE,
    scrollX = TRUE
  ))
  
  output$torta <- renderPlot({
    agg <- aggregate(minutos ~ proyecto, datos(), sum)
    agg$horas <- agg$minutos / 60
    agg <- agg[order(agg$horas), ]
    agg$proyecto <- factor(agg$proyecto, levels = agg$proyecto)
    ggplot(agg, aes(x = horas, y = proyecto)) +
      geom_col() +
      geom_text(aes(label = fmt_hm(minutos)), hjust = -0.1) +
      xlim(0, max(agg$horas) * 1.2) +
      labs(title = "Tiempo por proyecto", x = "horas", y = NULL) +
      theme_minimal()
  })
}

shinyApp(ui, server)
