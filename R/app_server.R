#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  rankings <- c('Unknown', 'Unlikely', 'Limited', 'Potential', 'Possible', 'Likely')

  # Define colors
  colors <- data.frame(ranking = rankings,
                       shade = c('teal', 'red', 'red', 'orange', 'lime', 'green'))

  icons <- data.frame(ranking = rankings,
                       icon = c('notdef', 'xmark', 'xmark', 'xmark', 'check', 'check'))

  myColors <- c('#00A65A', '#ff851b', '#DD4B39')
  names(myColors) <- c('support', 'mixed', 'none')

  myPeople <- c('#31708f', '#A6D6F6')
  names(myPeople) <- c('Yes', 'No')

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

  output$text <- renderUI({
    h2(icon("lightbulb", style = "color:orange;"), menuData$name[menuData$id==rv$item])
  })

  # Add to shortlist on click
  observeEvent(input$addShortlist, {
    current_df <- shortList()
    row_to_update <- match(rv$item, menuData$id)
    current_df[row_to_update, "include"] <- 1L
    shortList(current_df)

    shinyalert::shinyalert(title = "Added to shortlist!",
                           type = "success",
                           text = "Click view shortlist to review your shortlist",
                           timer=1000,
                           showConfirmButton=TRUE)
  })

  # Remove from shortlist on click
  observeEvent(input$removeShortlist, {
    current_df <- shortList()
    row_to_update <- match(rv$item, menuData$id)
    current_df[row_to_update, "include"] <- 0L
    shortList(current_df)

    shinyalert::shinyalert(title = "Removed from shortlist!",
                           type = "warning",
                           text = "Click view shortlist to review your shortlist",
                           timer=2000,
                           showConfirmButton=TRUE)
  })

  # Define shortlist list
  shortList <- reactiveVal(data.frame(id = 1:nrow(menuData), include = rep(0, nrow(menuData))))

  # Define summary
  summary <- reactive({
    menuData[shortList()$id[shortList()$include==1], ] %>%
      dplyr::select(groupA, brief, support, reco)
  })


  # Display dataframe in table
  output$table <- DT::renderDataTable({
    DT::datatable(
      summary(),
      colnames = c('Type', 'Description', 'Community support', 'Positive evaluations')
    )
  })



  output$addButton <- renderUI({

    added <- ifelse(shortList()$include[shortList()$id==rv$item]==1L, greenTick, greyTick)

    actionButton("addShortlist", "Add to shortlist", icon = icon("check", class = added))
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
    title <- HTML(paste0(h4(span(style="color: #05668D", n)," out of 71 respondents", span(style="color: #05668D", "expressed interest"), "in this initiative")))
  })

  # Plot
  output$waffle <- renderPlot({
    n <- menuData$reco[menuData$id==rv$item]

    df <- data.frame(
      x = c(seq(1,24), seq(1,24), seq(1,23)),
      y = c(rep(1,24), rep(2,24), rep(3,23)),
      interest = c(rep('Yes', n), rep('No',71-n))
    )

    ggplot2::ggplot(df, ggplot2::aes(x, y, fill=interest, color=interest)) +
      ggplot2::geom_tile(color='white', linewidth=1.6) +
      ggplot2::coord_equal() +
      ggplot2::scale_fill_manual(values = myPeople) +
      ggplot2::labs(y=NULL,
                    x="Each square represents one survey respondent. Dark blue squares represent survey respondents that expressed an interest in this initiative.") +
                    # x="Each square represents one survey respondent. <span style='color:#31708f;'>Dark blue squares</span> represent survey respondents that expressed an interest in this initiative.") +
      ggplot2::theme_void() +
      ggplot2::theme(legend.position = 'none',
                     axis.title.x = ggplot2::element_text(size=18, color="grey50"))

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
    title <- HTML(paste0(h4(
      span(style="color: #00A65A", n1)," out of ", N, " evaluations supported this initiative"
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
            legend.text = ggplot2::element_text(size=20)
      )

  })


} # Closes app_server
