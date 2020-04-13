library(shiny)
library(shinydashboard)
library(plotly)

convertMenuItem <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  mi
}

shinyUI <- dashboardPage(
  skin = "black",
  dashboardHeader(title = "Temperature ramps"),
  dashboardSidebar(
    sidebarMenu(
      convertMenuItem(
        menuItem(
          "Input info", tabName = "input",
          fileInput("wildtype", "Choose wildtype file",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          fileInput("mutant", "Choose mutant file",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          tags$hr(),
          checkboxInput("header", "Header", FALSE),
          radioButtons("sep", "Separator",
                       choices = c(Comma = ",",
                                   Semicolon = ";",
                                   Tab = "\t"),
                       selected = "\t"),
          tags$hr(),
          radioButtons("disp", "Display",
                       choices = c(Head = "head",
                                   All = "all"),
                       selected = "head"),
          numericInput("bins","Bins (min)",60,1,120,1),
          numericInput("zt0", "ZT 0", 0,0,23.9,0.1),
          numericInput("start_time", "Start time", 0,0,23.9,0.1),
          numericInput("modulotau","Desired modulo tau",24,16,32,1),
          numericInput("period","Photo/Thermoperiod",12,0,32,0.1)
        ),
        tabName = "input"
      ),
      convertMenuItem(
        menuItem(
          "Cycle wise profiles",
          tabName = "cyc_profiles",
          numericInput("cycle_no", "Cycle", 1,1,15,1)
        ),
        tabName = "cyc_profiles"
      ),
      convertMenuItem(
        menuItem(
          "Cycle wise heatmaps",
          tabName = "cyc_heatmaps",
          numericInput("cycle_no_hm", "Cycle", 1,1,15,1)
        ),
        tabName = "cyc_heatmaps"
      ),
      convertMenuItem(
        menuItem(
          "Cycle wise Slopes",
          tabName = "cyc_slopes"
        ),
        tabName = "cyc_slopes"
      ),
      convertMenuItem(
        menuItem(
          "Average Slopes",
          tabName = "avg_slopes"
        ),
        tabName = "avg_slopes"
      ),
      convertMenuItem(
        menuItem(
          "Piecewise linear estimation",
          tabName = "line_slopes",
          numericInput("ind_no", "ID", 1,1,32,1)
        ),
        tabName = "line_slopes"
      ),
      convertMenuItem(
        menuItem(
          "Cycle wise CoM",
          tabName = "cyc_com"
        ),
        tabName = "cyc_com"
      ),
      convertMenuItem(
        menuItem(
          "Average CoM",
          tabName = "avg_com"
        ),
        tabName = "avg_com"
      ),
      convertMenuItem(
        menuItem(
          "Activity consolidation",
          tabName = "avg_consol"
        ),
        tabName = "avg_consol"
      )
    )
  ),
  
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(
          tabName = "input",
          class = "active",
          box(
            title = "Preview wildtype data",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            div(
              style = 'overflow-x: scroll',
              tableOutput("contents1") 
            )
          ),
          box(
            title = "Preview mutant data",
            solidHeader = T,
            collapsible = F,
            background = "red",
            # width=5.5,
            div(
              style = 'overflow-x: scroll',
              tableOutput("contents2") 
            )
          )
        ),
        tabItem(
          tabName = "cyc_profiles",
          class = "active",
          box(
            title = "Wildtype profiles",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            # height = 10,
            plotlyOutput(
              "wt_profiles"
            )
            ),
            box(
              title = "Mutant profiles",
              solidHeader = T,
              collapsible = F,
              background = "red",
              # width=5.5,
              plotlyOutput(
                "mut_profiles"
              )
            )
          # )
        ),
        tabItem(
          tabName = "cyc_heatmaps",
          class = "active",
          box(
            title = "Wildtype heatmaps",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            # height = 10,
            plotlyOutput(
              "wt_heatmaps"
            )
          ),
          box(
            title = "Mutant heatmaps",
            solidHeader = T,
            collapsible = F,
            background = "red",
            # width=5.5,
            plotlyOutput(
              "mut_heatmaps"
            )
          ),
          h2("The scale of the colorbar in both these heatmaps are set such that the maximum value depicted is the slope of the zeitgeber increase between ZT00 and ZT12.")
          # )
        ),
        tabItem(
          tabName = "cyc_slopes",
          class = "active",
          box(
            # fluidRow(
              title = "Wildtype slopes",
              solidHeader = T,
              collapsible = F,
              background = "green",
              # width=5.5,
              # height = 10,
              plotlyOutput(
                "wt_slopes"
              ) 
            # )
          ),
          box(
            # fluidRow(
              title = "Mutant slopes",
              solidHeader = T,
              collapsible = F,
              background = "red",
              # width=5.5,
              plotlyOutput(
                "mut_slopes"
              ) 
            # )
          ),
          h2("In both the panels, the red line is the slope of the zeitgeber increase throught the duration of the increase. The black lines are the slopes of activity profiles averaged over all individuals in the same time window.")
        ),
        tabItem(
          tabName = "avg_slopes",
          class = "active",
          box(
            title = "Wildtype",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            # height = 10,
            plotlyOutput(
              "wt_avg_slopes"
            )
          ),
          box(
            title = "Mutant",
            solidHeader = T,
            collapsible = F,
            background = "red",
            # width=5.5,
            plotlyOutput(
              "mut_avg_slopes"
            ) 
          )
        ),
        tabItem(
          tabName = "line_slopes",
          class = "active",
          box(
            title = "Wildtype",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            # height = 10,
            plotlyOutput(
              "wt_line_sl"
            )
          ),
          box(
            title = "Mutant",
            solidHeader = T,
            collapsible = F,
            background = "red",
            # width=5.5,
            plotlyOutput(
              "mut_line_sl"
            ) 
          )
        ),
        tabItem(
          tabName = "cyc_com",
          class = "active",
          box(
            title = "Wildtype",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            # height = 10,
            plotlyOutput(
              "wt_cyc_com"
            )
          ),
          box(
            title = "Mutant",
            solidHeader = T,
            collapsible = F,
            background = "red",
            # width=5.5,
            plotlyOutput(
              "mut_cyc_com"
            ) 
          ),
          box(
            title = "Wildtype Data",
            solidHeader = T,
            collapsible = F,
            background = "green",
            div(
              style = 'overflow-x: scroll; overflow-y: scroll',
              tableOutput('datatable.wt.cyc.com')
            ),
            downloadButton("downloadData1","Download data")
          ),
          box(
            title = "Mutant Data",
            solidHeader = T,
            collapsible = F,
            background = "red",
            div(
              style = 'overflow-x: scroll; overflow-y: scroll',
              tableOutput('datatable.mut.cyc.com')
            ),
            downloadButton("downloadData2","Download data")
          )
        ),
        tabItem(
          tabName = "avg_com",
          class = "active",
          box(
            title = "Wildtype",
            solidHeader = T,
            collapsible = F,
            background = "green",
            # width=5.5,
            # height = 10,
            plotlyOutput(
              "wt_avg_com"
            )
          ),
          box(
            title = "Mutant",
            solidHeader = T,
            collapsible = F,
            background = "red",
            # width=5.5,
            plotlyOutput(
              "mut_avg_com"
            ) 
          ),
          box(
            title = "Wildtype Data",
            solidHeader = T,
            collapsible = F,
            background = "green",
            div(
              style = 'overflow-x: scroll; overflow-y: scroll',
              tableOutput('datatable.wt.avg.com')
            ),
            downloadButton("downloadData3","Download data")
          ),
          box(
            title = "Mutant Data",
            solidHeader = T,
            collapsible = F,
            background = "red",
            div(
              style = 'overflow-x: scroll; overflow-y: scroll',
              tableOutput('datatable.mut.avg.com')
            ),
            downloadButton("downloadData4","Download data")
          )
        ),
        tabItem(
          tabName = "avg_consol",
          class = "active",
          box(
            title = "Comparing consolidation",
            solidHeader = T,
            collapsible = F,
            background = "teal",
            align = "center",
            plotlyOutput(
              "avg_consol"
            )
          )
        )
      )
    )
  )
)
