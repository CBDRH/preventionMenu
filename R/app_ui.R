#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shinyWidgets::useShinydashboard(),
      tags$script(src = "https://kit.fontawesome.com/7ad3cfe706.js"),
      tags$head(
        tags$style("html, body { height: 100%; width: 100%}")
      ),

      titlePanel(tags$div(class="title", "SUSTAIN: Supporting Uptake of Sustainable Tailored AoD Interventions in NSW")),

      sidebarLayout(position="left",
        sidebarPanel(tagList(
                 h3(icon("lightbulb"), "Intervention Menu"),
                 bsplus::bs_accordion(id = "menu") %>%
                 bsplus::bs_set_opts(panel_type = "info", use_heading_link = TRUE) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("people-group"), "Mobilisation")),
                           content = shiny::tagList(
                             actionButton('A', '‘Dry’ community events', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                             actionButton('B', 'Sporting groups and participation', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                             actionButton('C', 'After-school activities and groups for youths', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                           )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("handshake"), "Coalition")),
                                 content = shiny::tagList(
                                   actionButton('D', 'Community coalitions', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                   actionButton('E', 'Monitoring and feedback', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
               bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("cash-register"), "Point-of-sale")),
                                   content = shiny::tagList(
                                     actionButton('F', 'RSA training', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('G', 'Consumer awareness training', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('H', 'Store policy', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("legal"), "Legislation and Policy")),
                                   content = shiny::tagList(
                                     actionButton('I', 'Bans and restrictions', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('J', 'Location bans', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('K', 'Council revenue', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("building-columns"), "Infrastructure")),
                                   content = shiny::tagList(
                                     actionButton('L', 'Specialised services', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('M', 'Safety audit', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('N', 'Public transport', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("school"), "Education")),
                                  content = shiny::tagList(
                                    actionButton('O', 'School education', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                    actionButton('P', 'Community education', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                  )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("building-shield"), "Law Enforcement")),
                                   content = shiny::tagList(
                                     actionButton('Q', 'Random Breath Testing', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('R', 'Police presence', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('S', 'Punishment', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('T', 'Reducing importation', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("radio"), "Mass Media")),
                                   content = shiny::tagList(
                                     actionButton('U', 'Project awareness', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('V', 'Targeted messages', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("money-bill-trend-up"), "Price Increase")),
                                   content = shiny::tagList(
                                     actionButton('W', 'Purchasing tax', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("user-doctor"), "Health workers")),
                                   content = shiny::tagList(
                                     actionButton('X', 'Lived experience staff', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('Y', 'Public relations', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('Z', 'More counsellors', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('AA', 'Dedicated treatments', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('AB', 'Remote service', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('AC', 'Upskilling', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("people-roof"), "Families")),
                                   content = shiny::tagList(
                                     actionButton('AD', 'Parent/student focus', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('AE', 'Parent education', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )) %>%
                 bsplus::bs_append(title = shiny::HTML(paste(shiny::icon("person-digging"), "Workplace")),
                                   content = shiny::tagList(
                                     actionButton('AF', 'Workplace eduction', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                                     actionButton('AG', 'Workplace policy', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                                   )),
               div(class="puzzle",
                   "This interactive visualisation was created by Dr Mark Hanly, UNSW Sydney ",
                   tags$a(href="mailto:m.hanly@unsw.edu.au", icon("envelope"))
                     )

                  ) # Closes tagList
               ), # Closes column

        mainPanel(
          tabsetPanel(id = "hidden_tabs", type="hidden",
            tabPanel("Dashboard",
               tags$div(style="display: block; background-color: white; text-align: left; position: -webkit-sticky; position: sticky; top: 0; z-index:1;",
                tagList(
                  tags$div(style="display: block; height: 60px;",
                    htmlOutput("text"),
                  ),
                  hr(),
                  fluidRow(
                    column(width=3, offset=0,
                           actionButton('addShortlist', 'Add to shortlist', icon = icon("check", class="greenTick"), width="100%")
                           ),
                    column(width=4, offset = 1,
                           actionButton('viewShortlist', 'View shortlist', icon = icon("table"), width="100%")
                            ),
                    column(width=3, offset=1,
                           actionButton('removeShortlist', 'Remove from shortlist', icon = icon("xmark", class="redX"), width="100%")
                    )
                  )
                  ),
                  hr()
                  ),
               tags$div(style="z-index:1;",
               fluidRow(
                 h3(HTML(paste(icon("puzzle-piece", class="puzzle"), "Potential impact (from previous studies)"))),
               column(width=3,
                shinydashboard::valueBoxOutput("alcoholBox", width=NULL),
                 htmlOutput("alcoholSummary")
               ),
               column(width=3,
                shinydashboard::valueBoxOutput("tobaccoBox", width=NULL),
                htmlOutput("tobaccoSummary")
               ),
               column(width=3,
                shinydashboard::valueBoxOutput("cannabisBox", width=NULL),
                htmlOutput("cannabisSummary")
               ),
               column(width=3,
                      shinydashboard::valueBoxOutput("otherBox", width=NULL),
                      htmlOutput("otherSummary")
               )),

               hr(),
               h3(HTML(paste(icon("puzzle-piece", class="puzzle"), "Previous community-wide studies"))),
               htmlOutput("studiesTitle"),
               plotOutput("studiesPlot", height = "100px"),

               hr(),
               h3(HTML(paste(icon("puzzle-piece", class="puzzle"), "Community survey recommendations"))),
               htmlOutput("waffleTitle"),
               plotOutput("waffle", height = "200px")



               ) # Closes div
            ), # Closes Dashbard tabPanel

            tabPanel("Shortlist",
                     tags$div(style="height:100px; line-height: 100px; text-align: left;",
                              h2("Interventions shortlist")
                     ),
                     hr(),
                     fluidRow(
                     column(width=4, offset=4,
                            actionButton('viewDashboard', 'Return to dashboard', icon = icon("table"), width="100%"),
                     )),
                     hr(),
                     DT::dataTableOutput("table")
                     )

          ) # Closes tabsetPanel
        ) # Closes right-hand column


      ) # Closes fluidRow

    ) # Closes fluidPage
  ) # Closes tagList
} # Closes app_ui

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "preventionMenu"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
