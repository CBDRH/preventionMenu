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


      fluidRow(
        column(width=6,
               tagList(
                 tags$div(style = "overflow-y: scroll; height: 1200px;",
                          actionButton('A', '‘Dry’ community events focussed on health promotion', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('B', 'Sporting groups and participation', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('C', 'After-school activities and groups for youths', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('D', 'Community coalitions', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('E', 'Monitoring and feedback', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('F', 'RSA training', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('G', 'Consumer awareness training', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('H', 'Store policy', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('I', 'Bans and restrictions', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('J', 'Location bans', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('K', 'Council revenue', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('L', 'Specialised services', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('M', 'Safety audit', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('N', 'Public transport', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('O', 'School education', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('P', 'Community education', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('Q', 'Random Breath Testing', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('R', 'Police presence', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('S', 'Punishment', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('T', 'Reducing importation', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('U', 'Project awareness', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('V', 'Targeted messages', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('W', 'Purchasing tax', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('X', 'Lived experience staff', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('Y', 'Public relations', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('Z', 'More counsellors', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AA', 'Dedicated treatments', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AB', 'Remote service', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AC', 'Upskilling', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AD', 'Parent/student focus', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AE', 'Parent education', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AF', 'Workplace eduction', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb")),
                          actionButton('AG', 'Workplace policy', width='32%', class="myButton", icon = icon("lightbulb", class="myLightbulb"))
                      )
                  )
               ),

        column(width=6,
          tabsetPanel(id = "hidden_tabs", type="hidden",
            tabPanel("Dashboard",
               tags$div(style="display: block; background-color: white; text-align: left; position: -webkit-sticky; position: sticky; top: 0; z-index:1;",
                tagList(
                  tags$div(style="display: block; height: 80px;",
                    h2(textOutput("text")),
                  ),
                  hr(),
                  fluidRow(
                    column(width=2, offset=2,
                           actionButton('viewShortlist', 'View shortlist', icon = icon("table"))
                           ),
                    column(width=2, offset = 4,
                            shinyWidgets::prettySwitch("addtoShortlist", label = "Add to shortlist", status = "success", slim=TRUE, inline=TRUE)
                            )
                  )
                  ),
                  hr()
                  ),
               tags$div(style="z-index:1;",
               fluidRow(
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

               h2("Community survey recommendations"),
               htmlOutput("waffleTitle"),
               plotOutput("waffle", height = "200px"),

               hr(),
               h2("Previous community-wide studies"),
               htmlOutput("studiesTitle"),
               plotOutput("studiesPlot")

               ) # Closes div
            ), # Closes Dashbard tabPanel

            tabPanel("Shortlist",
                     tags$div(style="height:100px; line-height: 100px; text-align: left;",
                              h2("Interventions shortlist")
                     ),
                     actionButton('viewDashboard', 'Return to dashboard', width='50%')
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
