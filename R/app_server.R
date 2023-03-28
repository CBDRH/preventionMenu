#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # load libraries
  library(shinydashboard)


  # Read the raw data

 rankings <- c('Unknown', 'Unlikely', 'Limited', 'Potential', 'Possible', 'Likely')

  menuData <- read.csv("data-raw/menu-data.csv") %>%
    dplyr::mutate(
      alcohol = factor(alcohol, levels=0:5, labels=rankings),
      tobacco = factor(tobacco, levels=0:5, labels=rankings),
      cannabis = factor(cannabis, levels=0:5, labels=rankings),
      other = factor(other, levels=0:5, labels=rankings),
      alcoholUse = factor(alcoholUse, levels=0:5, labels=rankings),
      alcoholBehav = factor(alcoholBehav, levels=0:5, labels=rankings),
      alcoholHarm = factor(alcoholHarm, levels=0:5, labels=rankings),
      tobaccoUse = factor(tobaccoUse, levels=0:5, labels=rankings),
      tobaccoBehav = factor(tobaccoBehav, levels=0:5, labels=rankings),
      tobaccoQuit = factor(tobaccoQuit, levels=0:5, labels=rankings),
      cannabisUse = factor(cannabisUse, levels=0:5, labels=rankings),
      cannabisBehav = factor(cannabisBehav, levels=0:5, labels=rankings)
      )

  # Define colors
  colors <- data.frame(ranking = rankings,
                       shade = c('teal', 'red', 'red', 'orange', 'lime', 'green'))

  icons <- data.frame(ranking = rankings,
                       icon = c('notdef', 'xmark', 'xmark', 'xmark', 'check', 'check'))

  myColors <- c('#00A65A', '#ff851b', '#DD4B39')
  names(myColors) <- c('support', 'mixed', 'none')


  # Set up reactive value to track currently selected intervention
  rv <- reactiveValues(item='A')

  # Update the selection when user clicks on a button
  observeEvent(input$A, {rv$item = 'A'})
  observeEvent(input$B, {rv$item = 'B'})
  observeEvent(input$C, {rv$item = 'C'})
  observeEvent(input$D, {rv$item = 'D'})
  observeEvent(input$E, {rv$item = 'E'})
  observeEvent(input$F, {rv$item = 'F'})
  observeEvent(input$G, {rv$item = 'G'})
  observeEvent(input$H, {rv$item = 'H'})
  observeEvent(input$I, {rv$item = 'I'})
  observeEvent(input$J, {rv$item = 'J'})
  observeEvent(input$K, {rv$item = 'K'})
  observeEvent(input$L, {rv$item = 'L'})
  observeEvent(input$M, {rv$item = 'M'})
  observeEvent(input$N, {rv$item = 'N'})
  observeEvent(input$O, {rv$item = 'O'})
  observeEvent(input$P, {rv$item = 'P'})
  observeEvent(input$Q, {rv$item = 'Q'})
  observeEvent(input$R, {rv$item = 'R'})
  observeEvent(input$S, {rv$item = 'S'})
  observeEvent(input$T, {rv$item = 'T'})
  observeEvent(input$U, {rv$item = 'U'})
  observeEvent(input$V, {rv$item = 'V'})
  observeEvent(input$W, {rv$item = 'W'})
  observeEvent(input$X, {rv$item = 'X'})
  observeEvent(input$Y, {rv$item = 'Y'})
  observeEvent(input$Z, {rv$item = 'Z'})
  observeEvent(input$AA, {rv$item = 'AA'})
  observeEvent(input$AB, {rv$item = 'AB'})
  observeEvent(input$AC, {rv$item = 'AC'})
  observeEvent(input$AD, {rv$item = 'AD'})
  observeEvent(input$AE, {rv$item = 'AE'})
  observeEvent(input$AF, {rv$item = 'AF'})
  observeEvent(input$AG, {rv$item = 'AG'})

  output$text <- renderText({
    menuData$name[menuData$id==rv$item]
  })


  # Update panel

  observeEvent(input$viewShortlist, {
    updateTabsetPanel(session, "hidden_tabs", selected = "Shortlist")
  })

  observeEvent(input$viewDashboard, {
    updateTabsetPanel(session, "hidden_tabs", selected = "Dashboard")
  })


  # Waffle plot

  # Title
  output$waffleTitle <- renderUI({
    n <- menuData$reco[menuData$id==rv$item]
    title <- HTML(paste0(h3(span(style="color: #00A65A", n)," out of 71 respondents expressed interest in this initiative")))
  })

  # Plot
  output$waffle <- renderPlot({
    n <- menuData$reco[menuData$id==rv$item]
    waffle::waffle(c(n, 71-n), rows=3,
                   colors = c('#00A65A', '#DD4B39')) +
      ggplot2::theme(legend.position = 'none')
  })


  # Alcohol value box
  output$alcoholBox <- shinydashboard::renderValueBox({

    rank = menuData$alcohol[menuData$id==rv$item]
    color = colors$shade[colors$rank==rank]

    shinydashboard::valueBox(
      rank, "ALCOHOL",
      icon = icon("wine-bottle", fill=TRUE),
      color = color
    )
  })


  # Alcohol summary text
  output$alcoholSummary <- renderUI({

    useRank = menuData$alcoholUse[menuData$id==rv$item]

    useIcon = dplyr::case_when(
      useRank=='Likely' ~ 'check-double',
      useRank=='Possible' ~ 'check',
      useRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    harmRank = menuData$alcoholHarm[menuData$id==rv$item]
    harmIcon = dplyr::case_when(
      harmRank=='Likely' ~ 'check-double',
      harmRank=='Possible' ~ 'check',
      useRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    behavRank = menuData$alcoholBehav[menuData$id==rv$item]
    behavIcon = dplyr::case_when(
      behavRank=='Likely' ~ 'check-double',
      behavRank=='Possible' ~ 'check',
      useRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    HTML(paste(tags$div(class=useRank, icon("wine-bottle"), "Use", icon(useIcon)),
               tags$div(class=harmRank, icon("person-falling"), "Related Harm", icon(harmIcon)),
               tags$div(class=behavRank, icon("person-praying"), "Behavioural", icon(behavIcon))
               ))
  })

  # Tobacco value box
  output$tobaccoBox <- shinydashboard::renderValueBox({

    rank = menuData$tobacco[menuData$id==rv$item]
    color = colors$shade[colors$rank==rank]

    shinydashboard::valueBox(
      rank, "TOBACCO",
      icon = icon("smoking", fill=TRUE),
      color = color
    )
  })

  # Tobacco summary text
  output$tobaccoSummary <- renderUI({

    useRank = menuData$tobaccoUse[menuData$id==rv$item]

    useIcon = dplyr::case_when(
      useRank=='Likely' ~ 'check-double',
      useRank=='Possible' ~ 'check',
      useRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    behavRank = menuData$tobaccoBehav[menuData$id==rv$item]
    behavIcon = dplyr::case_when(
      behavRank=='Likely' ~ 'check-double',
      behavRank=='Possible' ~ 'check',
      behavRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    quitRank = menuData$tobaccoQuit[menuData$id==rv$item]
    quitIcon = dplyr::case_when(
      quitRank=='Likely' ~ 'check-double',
      quitRank=='Possible' ~ 'check',
      quitRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    HTML(paste(tags$div(class=useRank, icon("smoking"), "Use", icon(useIcon)),
               tags$div(class=quitRank, icon("ban-smoking"), "Quitting", icon(quitIcon)),
               tags$div(class=behavRank, icon("person-praying"), "Behavioural", icon(behavIcon))
    ))
  })



  # Cannabis
  output$cannabisBox <- shinydashboard::renderValueBox({

    rank = menuData$cannabis[menuData$id==rv$item]
    color = colors$shade[colors$rank==rank]

    shinydashboard::valueBox(
      rank, "CANNABIS",
      icon = icon("cannabis", fill=TRUE),
      color = color
    )
  })

  # Cannabis summary text
  output$cannabisSummary <- renderUI({

    useRank = menuData$cannabisUse[menuData$id==rv$item]

    useIcon = dplyr::case_when(
      useRank=='Likely' ~ 'check-double',
      useRank=='Possible' ~ 'check',
      useRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    behavRank = menuData$cannabisBehav[menuData$id==rv$item]
    behavIcon = dplyr::case_when(
      behavRank=='Likely' ~ 'check-double',
      behavRank=='Possible' ~ 'check',
      behavRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    HTML(paste(tags$div(class=useRank, icon("cannabis"), "Use", icon(useIcon)),
               tags$div(class=behavRank, icon("person-praying"), "Behavioural", icon(behavIcon))
    ))
  })


  # Other drugs
  output$otherBox <- shinydashboard::renderValueBox({

    rank = menuData$other[menuData$id==rv$item]
    color = colors$shade[colors$rank==rank]

    shinydashboard::valueBox(
      rank, "OTHER DRUGS",
      icon = icon("pills", fill=TRUE),
      color = color
    )
  })

  # Other summary text
  output$otherSummary <- renderUI({

    useRank = menuData$other[menuData$id==rv$item]

    useIcon = dplyr::case_when(
      useRank=='Likely' ~ 'check-double',
      useRank=='Possible' ~ 'check',
      useRank=='Potential' ~ 'circle-question',
      TRUE ~ ""
    )

    HTML(paste(tags$div(class=useRank, icon("pills"), "Use", icon(useIcon))))
  })


  # Support

  # Title
  output$studiesTitle <- renderUI({
    n1 <- menuData$support[menuData$id==rv$item]
    n2 <- menuData$mixed[menuData$id==rv$item]
    n3 <- menuData$none[menuData$id==rv$item]
    N = n1 + n2 + n3
    title <- HTML(paste0(h3(
      span(style="color: #00A65A", n1)," out of ", N, " evaluations suppoted this initiative"
      )))
  })

  # Plot
  output$studiesPlot <- renderPlot( height=100, {
    menuData %>%
      dplyr::filter(id==rv$item) %>%
      dplyr::select(support, mixed, none) %>%
      tidyr::pivot_longer(cols = everything()) %>%
      dplyr::mutate(
        name=factor(name, levels = c('support', 'mixed', 'none')),
        lab = ifelse(value==0, NA, value)
             ) %>%
      ggplot2::ggplot(ggplot2::aes(y=value, x=1, fill=name, label=lab)) +
      ggplot2::geom_col(position = ggplot2::position_stack(reverse=TRUE)) +
      ggplot2::geom_text(color = 'white', position = ggplot2::position_stack(vjust = .5, reverse=TRUE), size=12) +
      ggplot2::coord_flip() +
      ggplot2::scale_x_continuous(NULL) +
      ggplot2::scale_y_continuous(NULL) +
      ggplot2::scale_fill_manual(NULL, values = myColors, labels=c('Supportive', 'Mixed', 'No Impact')) +
      ggplot2::theme(legend.position = 'bottom',
            plot.background = ggplot2::element_rect(fill='white'),
            panel.background = ggplot2::element_rect(fill='white'),
            axis.text = ggplot2::element_blank(),
            axis.ticks = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size=16)
      )

  })


} # Closes app_server
